import {
    CloseBrace,
    Closer,
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

    function gather(RBP=0) { // internal

        /* This function implements Pratt's Algorithm. It is textbook. */

        let current, result;

        current = token;
        token = advance();
        result = current.prefix(api);

        // if (!result.expression) return result;

        while (RBP < token.LBP) {

            current = token;
            token = advance();
            result = current.infix(api, result);
        }

        return result;
    }

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

            yield statement = gatherStatement();

            if (on(Terminator)) continue;

            if (on(CloseBrace) && nested) return advance();

            if (statement instanceof Header && previous instanceof CloseBrace) {

                skip = true; // statements can immediately follow a braced block

            } else throw new SyntaxError("required terminator was not found");
        }

        if (nested) throw new SyntaxError("end of file inside block");
    }

    function ignoreInsignificantNewlines() { // internal

        /* Advance the parser state until the current `token` is significant, if it is not
        significant already. */

        if (!listStateStack.at(-1)) while (on(LineFeed)) advance();
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
        else throw new SyntaxError("required a property (dint get one)");
    }

    function gatherVariable() { // api function

        /* This function gathers a single variable token, then advances the parser, before
        returning the token (which is noted before advancing), complaining if the token is
        not a variable. */

        if (on(Variable)) return advance(true);
        else throw new SyntaxError("required a variable??");
    }

    function gatherExpression() { // api function

        /* This function wraps the Pratt function to ensure that the result is an expression
        (not a formal statement), complaining otherwise. */

        const candidate = gather();

        if (candidate.expression) return candidate;
        else throw new SyntaxError("required an expression");
    }

    function gatherStatement() { // api function

        /* This function wraps the Pratt function to ensure that the result (which could be
        a formal statement or an expression) is valid in the current context (block-wise),
        complaining otherwise. */

        const candidate = gather();

        if (candidate.validate(api)) return candidate;
        else throw new SyntaxError("invalid statement type, given current context");
    }

    function gatherFormalStatement() { // api function

        /* This function wraps the Pratt function to ensure that the result is  a formal
        statement, complaining otherwise. */

        const cadidate = gatherStatement();

        if (cadidate instanceof Keyword) return cadidate;
        else throw new SyntaxError("required a formal statement");
    }

    function gatherBlock(type) { // api function

        /* This function takes a block type (an integer, see `walk`), and pushes it to the
        block stack, before gathering a formal statement or an array of statements (of any
        number and kind). Once the block has been parsed, the block type is popped off the
        stack, and the parsed block is returned.

        If the block type is functional (including a class), the LIST state `true` is also
        pushed on to the list stack, and popped when the function body has been parsed.

        Furthermore, if the block type is functional, this function requires that the body
        is wrapped in braces (it cannot be a naked formal statement). */

        const [functional, braced] = [type > 0, on(OpenBrace)];

        if (functional && !braced) throw new SyntaxError("bodies require braces");

        blockTypeStack.push(type);

        if (functional) listStateStack.push(true);

        let result = braced ? [...LIST()] : gatherFormalStatement();

        if (functional) { listStateStack.pop(); ignoreInsignificantNewlines() }

        blockTypeStack.pop();

        return result;
    }

    function gatherCompoundExpression(closer, validate=expression=>expression) {

        /* This function takes a `Token` subclass which indicates which closing character to
        use to gather a compound statement (a sequence of comma-separated expressions, wrapped
        in parens, brackets or braces), as well as taking an optional function that validates
        each expression, as they are gathered into a results array and returned, defaulting
        to a function that validates any valid expression.

        The validator function is passed a reference to the expression it needs to check, as
        well as a reference to the results array. It is expected to either return some valid
        expression (in practice, its first argument) or raise an exception.

        The results array may contain `null` expressions, as adjacent commas can be used to
        imply empty expressions (note that these expression are automatically valid).

        This function is used whenever a parser method needs to parse one or more expresions
        in parens, brackets or braces, and will implicitly update the LIST state on the way
        in and out. */

        if (validate === undefined) validate = expression => expression;

        listStateStack.push(false);
        ignoreInsignificantNewlines();

        const results = on(Comma) ? [null] : []; // account for leading empty expressions

        while (!on(closer)) {

            if (on(Comma)) {

                advance();

                if (on(Comma, closer)) results.push(null);

            } else {

                results.push(validate(gatherExpression(), results));

                if (!on(Comma, closer)) {

                    console.log(token)
                    throw new SyntaxError("no sequence delimiter");
                }
            }
        }

        listStateStack.pop();   // firstly, restore the previous LIST state
        advance();              // only now that the state is restored, drop the closer
                                // any insignificant newlines get dropped by `advance`
        return results;
    }

    function walk(doStop, isValid, fallback=false) { // api function

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

    function pending(grammar) { // api function

        /* This function is used to establish whether a statement or expression that
        ends with an optional construct (`break`, `continue`, `return`, `yield`), has
        more to parse (returns `true`) or not (returns `false`). */

        if (grammar.expression) return ! on(Terminator, Comma, Closer);
        else return ! on(Terminator, CloseBrace);
    }

    const api = {
        advance,
        gatherBlock,
        gatherCompoundExpression,
        gatherExpression,
        gatherFormalStatement,
        gatherProperty,
        gatherStatement,
        gatherVariable,
        pending,
        on,
        walk,
    };

    const blockTypeStack = [1];
    const listStateStack = [true];
    const tokens = lex(source, literate);

    let token, previous;

    yield * LIST(false);
}
