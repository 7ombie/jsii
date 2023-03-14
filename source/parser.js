import {
    ParserError,
    CloseBrace,
    Comma,
    EOF,
    Header,
    Keyword,
    OpenBrace,
    Operator,
    LineFeed,
    Terminator,
    Word,
    Variable,
} from "./objects.js"

import lex from "./lexer.js"

export default function * (source, literate=false) {

    /* This generator implements the parser stage, and yields an AST node for each top-level
    statement. Like the lexer stage, the specifics of parsing any given grammar are left to
    the corresponding grammar object in `objects.js`.

    The parser API object collects references to a subset of the functions defined below, and
    every token has `prefix` and `infix` methods that take a reference to the parser API object.
    The `infix` methods also take a reference to the lefthand production as a second argument.

    The `source` string and `literate` flag are passed along to the lexer stage. */

    function * LIST(nested=true) { // internal

        /* This is the (often) recursive, block-level parsing function that wraps the
        Pratt parser to implement a statement grammar that replaces ASI with LIST
        (Linewise Implicit Statement Termination). */

        let statement, skip = false;

        while (skip || advance()) {

            skip = false;

            if (on(EOF)) break;

            if (on(CloseBrace) && nested) return advance();

            if (on(Terminator)) continue;

            yield statement = gather();

            if (on(Terminator)) continue;

            if (on(CloseBrace) && nested) return advance();

            if (statement instanceof Header && previous instanceof CloseBrace) {

                skip = true; // statements can immediately follow a braced block

            } else throw new ParserError("required a terminator", token.location);
        }

        if (nested) throw new ParserError("nested end of file", token.location);
    }

    function ignoreInsignificantNewlines() { // internal

        /* Advance the parser state until the current `token` is significant, if it is not
        significant already. */

        if (!listStateStack.at(-1)) while (on(LineFeed)) advance();
    }

    function gather(RBP=0) { // api function

        /* This function implements Pratt's Algorithm. It is textbook, except for some extra
        validation that ensures two things:

        1) Only valid expressions get passed to `infix` methods. Statements that cannot also
           be expressions are immediately returned.
        2) Statements (like `break` and `return`) and expressions (like `yield`) that must
           only appear in specific contexts are (recursively) validated, and an exception
           is raised if a statement cannot appear on the current block stack.

        Note: The main reason for Point 1 is to avoid passing statements to infix operators,
        which can lead to less explanatory error messages. */

        function validate(result) {

            /* Use the `validate` method of the token instance to check it, and complain if
            it is not valid (given the blocks it is nested within). */

            if (result.validate(check)) return result;

            throw new ParserError(`unexpected ${result.value} statement`, result.location);
        }

        let current, result;

        current = token;
        token = advance();
        result = validate(current.prefix(api));

        if (result.expression) while (RBP < token.LBP) {

            current = token;
            token = advance();
            result = validate(current.infix(api, result));
        }

        return result;
    }

    function advance(returnPrevious=false) { // api function

        /* Advance the token stream by one token, updating the nonlocal `token`, then
        return a reference to it, unless the `returnPrevious` argument is truthy. In
        that case, return the token that was current when the invocation was made. */

        [previous, token] = [token, tokens.next().value];

        ignoreInsignificantNewlines();

        return returnPrevious ? previous : token;
    }

    function on(...types) { // api function

        /* Take any number of `Token` subclasses, and return `true` if the current `token` is
        an instance of a given class (or any subclass), else `false`. */

        for (const type of types) if (token instanceof type) return true;

        return false;
    }

    function gatherProperty() { // api function

        /* This function gathers a single property token, which can be a variable name or any
        kind of word (keyword, operator name, reserved word etc). If the token is a property,
        the function advances the parser, then returns the token (which is noted before
        advancing), complaining otherwise. */

        if (on(Word) || on(Operator) && token.named) return advance(true);
        else throw new ParserError("required a property", token.location);
    }

    function gatherVariable() { // api function

        /* This function gathers a single variable token, then advances the parser, before
        returning the token (which is noted before advancing), complaining if the token is
        not a variable. */

        if (on(Variable)) return advance(true);
        else throw new ParserError("required a variable", advance().location);
    }

    function gatherExpression() { // api function

        /* This function wraps the Pratt function to ensure that the result is an expression
        (not a formal statement), complaining otherwise. */

        const candidate = gather();

        if (candidate.expression) return candidate;
        else throw new ParserError("required an expression", candidate.location);
    }

    function gatherBlock(type) { // api function

        /* This function takes a block type (an integer, see `check`), and pushes it to the
        block stack, before gathering a formal statement or an array of statements (of any
        number and kind). Once the block has been parsed, the block type is popped from
        the stack, and the parsed block is returned.

        If the block type is functional (including a class), the LIST state `true` is also
        pushed on to the list stack, and popped when the function body has been parsed.

        Furthermore, if the block type is functional, this function requires that the body
        is wrapped in braces (it cannot be a naked formal statement). */

        function gatherFormalStatement() {

            /* This function wraps the Pratt function to ensure that the result is a formal
            statement, complaining otherwise. */

            const candidate = gather();

            if (candidate instanceof Keyword) return candidate;
            else throw new ParserError("required a formal statement", candidate.location);
        }

        const [functional, braced] = [type > 0, on(OpenBrace)];

        if (functional && !braced) throw new ParserError("required a body", token.location);

        blockTypeStack.push(type);

        if (functional) listStateStack.push(true);

        let result = braced ? [...LIST()] : gatherFormalStatement();

        if (functional) { listStateStack.pop(); ignoreInsignificantNewlines() }

        blockTypeStack.pop();

        return result;
    }

    function gatherCompoundExpression(closer, validate=expression=>expression) {

        /* This function takes a `Token` subclass which indicates which closing character to
        use to gather a compound statement (a sequence of zero or more comma-separated expr-
        essions, wrapped in parens, brackets or braces). It also takes an optional function
        that validates each expression, as they are gathered into a results array, which is
        eventually returned. The validation function defaults to a function that validates
        anything it is passed (which will always be a valid expression).

        The validator function is passed a reference to the expression it needs to check, as
        well as a reference to the results array. It is expected to either return some valid
        expression (in practice, its first argument) or raise an exception.

        The results array may contain `null` expressions, as adjacent commas can be used to
        imply empty expressions (note that these expression are automatically valid).

        This API function is used whenever a parser method needs to parse an expresion that
        is wrapped in parens, brackets or braces, and the function will implicitly update
        the LIST state on the way in and out.

        Note: The (minimum-precedence) pair-operator (`:`) is used to map one expression to
        another, and allows object expressions to be parsed like other compound expressions.

        Note: This function is also used for bracketed notation, computed properties etc,
        and the caller checks the result has the required length (`1`) after the fact.

        The parser is able to handle the LIST states implicitly. Parser methods can use the
        API to parse blocks and compound expressions, and insignificant newlines will just
        disappear from the stream automatically. */

        listStateStack.push(false);         /// firstly, disable significant newlines, then
        ignoreInsignificantNewlines();      /// ensure that the first `token` is significant

        const results = on(Comma) ? [null] : []; // account for leading empty expressions

        while (!on(closer)) {

            if (on(Comma)) { // account for empty expressions...

                advance();

                if (on(Comma, closer)) results.push(null);

            } else { // push an actual expression, then require it is terminated properly...

                results.push(validate(gatherExpression(), results));

                if (on(Comma, closer)) continue;
                else throw new ParserError("required delimiter", token.location);
            }
        }

        listStateStack.pop();   /// firstly, restore the previous LIST state - then, once the
        advance();              /// state is restored, drop the `closer` instance, leaving it
                                /// to `advance` to drop any insignificant newlines
        return results;
    }

    function check(doStop, isValid, fallback=false) { // api function

        /* This function allows statements (like `return` and `break`) that can only
        appear in specific contexts to establish whether they are in a valid context.

        The process walks the nonlocal `blockTypeStack` backwards (from innermost to
        outermost), and calls the `doStop` callback on each type to establish when
        to stop (it should return `true` to stop, and `false` otherwise).

        If the loop stops, the `isValid` callback is invoked on the type that was
        stopped on, and the result of that invocation (which is expected to be a
        bool) is returned.

        The `fallback` is returned when the stack is exhausted without a match.

        The block types are enumerated as follows:

            + loop blocks = -1
            + simple blocks = 0
            + function blocks = 1
            + generator blocks = 2
            + async generator blocks = 3
            + async function blocks = 4
            + class blocks = 5

        It is always possible to establish the validity of a statement by walking to
        the last related block, then checking whether its enumeration is within some
        range, falling back to a bool if the top-level is reached.

        Note: Reaching the top would always result in `false`, except top-level-await
        is valid (in modules), so the fallback must be `true` in that case. */

        const stack = blockTypeStack;
        const top = stack.length - 1;

        for (let i = top; i >= 0; i--) if (doStop(stack[i])) return isValid(stack[i]);

        return fallback;
    }

    const api = {
        on,
        advance,
        gather,
        gatherVariable,
        gatherProperty,
        gatherExpression,
        gatherCompoundExpression,
        gatherBlock,
    };

    const blockTypeStack = [3, -1];
    const listStateStack = [true];
    const tokens = lex(source, literate);

    let token, previous;

    yield * LIST(false);
}
