import { put, not, Stack } from "./helpers.js"
import { LarkError } from "./error.js"
import { lex } from "./lexer.js"

import {
    Block,
    CloseBrace,
    Comma,
    CompoundExpression,
    EOF,
    Header,
    Keyword,
    LineFeed,
    OpenBrace,
    OpenBracket,
    Operator,
    Parameters,
    SkipAssignee,
    Terminator,
    Variable,
    Word
} from "../user/tokens.js"

export function * parse(source, {dev=false}={}) {

    /* This generator implements the parser stage, and yields an AST node for each top-level
    statement. Like the lexer stage, the specifics of parsing any given grammar are left to
    the corresponding grammar object in `objects.js`.

    The parser API object collects references to a subset of the functions defined below, and
    every token has `prefix` and `infix` methods that take a reference to the parser API object.
    The `infix` methods also take a reference to the lefthand production as a second argument. */

    function * LIST(nested=true) { // internal

        /* This is the (often) recursive, block-level parsing function that wraps the Pratt parser
        to implement a statement grammar that replaces ASI with LIST (Linewise Implicit Statement
        Termination). */

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

            } else throw new LarkError("required a terminator", token.location);
        }

        if (nested) throw new LarkError("nested end of file", token.location);
    }

    function advance(returnPrevious=false) { // api function

        /* Advance the token stream by one token, updating the nonlocal `token`, then
        return a reference to it, unless the `returnPrevious` argument is truthy. In
        that case, return the token that was current when the invocation was made. */

        [previous, token] = [token, tokens.next().value];

        ignoreInsignificantNewlines();

        return returnPrevious ? previous : token;
    }

    function on(...Classes) { // api function

        /* Optionally take one or more `Token` subclasses, if the current `token` is one of
        them, return it. If none of the subclasses match, return `false`. If no arguments
        are given, the function acts as a getter, just returning the current `token`. */

        if (Classes.length === 0) return token;

        for (const Class of Classes) if (token instanceof Class) return Class;

        return false;
    }

    function ignoreInsignificantNewlines() { // internal

        /* Advance the parser state until the current token is significant. When newlines are
        significant, all tokens are significant, so this is a noop. Otherwise, it skips over
        any newlines, stopping on the first non-newline token. */

        if (whitespace.top === false) while (on(LineFeed)) advance();
    }

    function gather(RBP=0, context=undefined) { // api function

        /* This function implements Pratt's Algorithm. It is textbook, except for some extra
        validation that ensures two things:

        1) Only valid expressions get passed to `infix` methods. Statements that cannot also
           be expressions are immediately returned.
        2) Statements (like `break` and `return`) and expressions (like `yield`) that must
           only appear in specific contexts are validated here, and an exception is raised
           if a given construct cannot appear on the current block stack.

        Point 1 prevents a statement (that ends without an expression) from being incorrectly
        passed to an operator that immediately follows the statement, which is a syntax error,
        unless it forms a valid expression (the statement is also a valid expression).

        Point 1 eliminates the issue that `function` has in JavaScript, where functions cannot
        also be expressions when the statement begins with the function-keyword (without valid-
        ating anything that should remain invalid). This also applies to all formal expressions
        (any statment beginning with `do`, `async`, `await`, `lambda`, `yield` etc).

        Note: The distinction between formal and informal statements only applies lexically
        (to blocks without braces). In general, a statement is a statement and an expression
        is an expression, without exception; they are just not mutually exclusive.
    
        The first (required) argument is the binding power. The second (optional) argument can
        be anything, as it is simply passed to the `prefix` method of the first token in the
        expression, as a context. In practice, it is used by `Async` to contextualize the
        function statement that follows (`Async.prefix` checks the next token begins a
        function literal before it invokes this function). */

        function validate(result) {

            /* Use the `validate` method of the token instance to check it, and complain if
            it is not valid (given the blocks it is nested within). */

            if (result.validate(api)) return result;

            throw new LarkError(`unexpected ${result.value} statement`, result.location);
        }

        let current, result;

        // use `current` to track the token we need to parse next when advancing the (nonlocal)
        // parser state to the next `token` in the stream, and use `result` to store the parse
        // tree is it's generated, first by an invocation of the `current` token's `prefix`
        // method, then by zero or more iterations of the following loop (which invokes
        // the `current` token's `infix` instead)...

        current = token;
        token = advance();
        result = validate(current.prefix(api, context));

        if (result.expression) while (RBP < token.LBP) {

            current = token;

            // check for and break on operative keywords (like `yield` and `throw`), so they still
            // act like keywords when used to begin an unbraced control-flow block, despite also
            // being operators (that always introduce valid expressions)...

            if (current instanceof Keyword) return result;

            // advance to the next token, then pass everything that has been parsed so far (as a
            // prefix) to the the `infix` method of the `current` token instance, updating the
            // `result` with whatever it returns (assuming it also validates)...

            token = advance();
            result = validate(current.infix(api, result));
        }

        return result;
    }

    function gatherExpression(RBP, context=undefined) { // api function

        /* This function wraps the Pratt function to ensure that the result is an expression
        (not a formal statement), complaining otherwise. */

        const candidate = gather(RBP, context);

        if (candidate.expression) return candidate;
        else throw new LarkError("expected an expression", candidate.location);
    }

    function gatherCompoundExpression(Closer) {

        /* This function takes a `Token` subclass which indicates which closing character to
        use to gather a compound statement (a sequence of zero or more comma-separated expr-
        essions, wrapped in parens, brackets or braces) into a `results` array, which is
        returned.

        The results array may contain `null` expressions, as adjacent commas can be used to
        imply empty assignees in destructuring assignments.

        This API function is used whenever a parser method needs to parse an expresion that
        is wrapped in parens, brackets or braces, and the function will implicitly update
        the LIST state on the way in and out.

        Note: The label-pseudo-operator (`:`) may be used to map one expression to another.

        Note: This function is also used for bracketed notation, computed properties etc. */

        whitespace.top = false;

        // initialize the `results` array, accounting for the possibility of a leading empty
        // expression (which is legal in destructuring assignments)...

        const block = new CompoundExpression(token.location)

        if (on(Comma)) block.push(new SkipAssignee(token.location));

        // now, loop until we find the closer, gathering the list of comma-separated, optionally
        // labelled expressions that comprise the compound expression, while allowing for empty
        // expressions to permit empty assignees in destructuring assignments...

        while (not(on(Closer))) if (on(Comma)) {

             // this block handles an empty assignee...

            advance();

            if (on(Comma, Closer)) block.push(new SkipAssignee(token.location));

        } else {

            // this block pushes an actual expression, then requires that it's terminated
            // properly, complaining otherwise...

            block.push(gatherExpression());

            if (on(Comma, Closer)) continue;
            else throw new LarkError("required delimiter", token.location);
        }

        // restore the previous significance of newlines *before* advancing to drop both the
        // the `Closer` instance and any insignificant newlines, then return the results...

        whitespace.pop;
        advance();

        return block;
    }

    function gatherVariable() { // api function

        /* This function gathers a single variable token, then advances the parser, before
        returning the token (which is noted before advancing), complaining if the token is
        not a variable. */

        if (on(Variable)) return advance(true);
        else throw new LarkError("required a variable", advance().location);
    }

    function gatherProperty() { // api function

        /* This function gathers a single property token, which can be a variable name or any
        kind of word (keyword, operator name, reserved word etc). If the token is a property,
        the function advances the parser, then returns the token (noting it before advancing),
        and simply complaining otherwise. */

        if (on(Word) || (on(Operator) && token.named)) return advance(true);
        else throw new LarkError("required a property", token.location);
    }

    function gatherAssignee() { // api function

        /* This function is used by the `For` class for gathering a single assignee, without
        treating it as an expression (so the in-keyword is not parsed as an infix operator). */

        token.expression = false;

        if (on(Variable)) return gatherVariable();

        if (on(OpenBracket, OpenBrace)) return gather();

        throw new LarkError("invalid assignee", token.location);
    }

    function gatherParameters() { // api function

        /* This function gathers the parameters for a function into a `Parameters` instance,
        which is returned. The operands will all be valid expressions, with no empty values,
        but are not otherwise validated (as function parameters).

        Note: Lark parameters do not use parens (so are not parsed as compound expressions).

        Note: Arrow functions use `gatherCompoundExpression` (implicitly, as the arrow will
        not be encountered until the parameters have already been parsed). */

        const result = new Parameters(token.location);

        if (on(OpenBrace)) return result;

        while (true) {

            result.push(gatherExpression());

            if (on(Comma)) advance();
            else if (on(OpenBrace)) break;
            else throw new LarkError("required a comma or block", token.location);
        }

        return result;
    }

    function gatherBlock(type) { // api function

        /* This function takes a block type (an integer, see `check`), and pushes it to the
        block stack, before gathering a formal statement or an array of statements (of any
        number and kind). Once the block has been parsed, the block type is popped from
        the stack, and the parsed block is returned.

        If the block type is functional, this function requires that the body is wrapped in
        braces (as only control-flow statements can have unbraced blocks). */

        function gatherFormalStatement() {

            /* This function wraps the Pratt function to ensure that the result is a formal
            statement, complaining otherwise. */

            const candidate = gather();

            if (candidate instanceof Keyword) return candidate;
            else throw new LarkError("required a formal statement", candidate.location);
        }

        const [functional, braced, block] = [type > 0, on(OpenBrace), new Block(token.location)];

        if (functional && not(braced)) throw new LarkError("required a body", token.location);

        blocktypes.top = type;
        whitespace.top = true;

        if (functional) labelspace.top = {};

        if (braced) block.push(...LIST());
        else block.push(gatherFormalStatement());

        if (functional) labelspace.pop;

        whitespace.pop
        blocktypes.pop;

        return block;
    }

    function check(doStop, isValid, fallback=false) { // api function

        /* This function allows statements (like `return` and `break`) that can only appear in
        specific contexts to establish whether they are in a valid context.

        If no arguments are given, the function returns `true` if the `blocktypes` stack is empty
        (indicating that the parser is at the top level), and `false` otherwise.

        When two or three arguments are givenm the function walks the nonlocal `blocktypes` stack
        backwards (from innermost to outermost), and calls the `doStop` callback on each type to
        establish when to stop (it should return `true` to stop, and `false` otherwise).

        If the loop stops, the `isValid` callback is invoked on the type that was stopped on, and
        the result of that invocation (which is expected to be a bool) is returned.

        The `fallback` is returned when the stack is exhausted without a match.

        The block types are enumerated as follows:

            + loop blocks = -1
            + simple blocks = 0
            + function blocks = 1
            + async function blocks = 2
            + class blocks = 3

        It is always possible to establish the validity of a statement by walking to the last
        related block, then checking whether its enumeration is within some range, assuming a
        `fallback` argument is returned when the top-level is reached.

        Note: Reaching the top would always result in `false`, except that top-level-await is
        valid (in modules), so the fallback must be `true` in that case. */

        if (arguments.length === 0) return blocktypes.length === 0;

        for (let i = blocktypes.length - 1; i >= 0; i--) {

            if (doStop(blocktypes[i])) return isValid(blocktypes[i]);
        }

        return fallback;
    }

    function label(name, value=undefined) { // api function

        /* This function is a getter-setter, used to get and set the state of labels.
        It takes a `Varaible` instance (`name`) and an optional ternary value (one of
        `true`, `false` or `null`).

        When the second argument is left undefined (getter mode), the function returns
        `true` when the given label is active and bound to a loop block, `false` when
        it is active and bound to a simple block, and `null` when it is inactive (as
        it cannot be found in the hash of labels on top of the `labelspace` stack).

        When the second argument is set (setter mode), its value is used to update the
        state of the given label on the `labelspace` stack.

        Note: The `labelspace` is a stack that has label-hashes pushed and popped auto-
        matically (by `gatherBlock`) whenever entering or leaving a function body. The
        hashes on the stack map label names (as strings) to their ternary state. */

        if (value === undefined) return labelspace.top[name.value] ?? null;
        else if (value === null) delete labelspace.top[name.value];
        else labelspace.top[name.value] = value;
    }

    // gather the api functions and flags to form the parser api object, initialize the internal
    // state, then invoke `LIST` to yield the resulting top-level statements, one by one...

    const api = {
        advance,
        check,
        dev,
        gather,
        gatherVariable,
        gatherProperty,
        gatherExpression,
        gatherCompoundExpression,
        gatherParameters,
        gatherAssignee,
        gatherBlock,
        label,
        on
    };

    const blocktypes = new Stack();
    const blockspace = new Stack({});
    const labelspace = new Stack({});
    const whitespace = new Stack(true).on(false, ignoreInsignificantNewlines);

    const tokens = lex(source, {dev});

    let token, previous;

    yield * LIST(false);
}
