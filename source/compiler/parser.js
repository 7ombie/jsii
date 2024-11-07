import { put, not, Stack } from "../compiler/helpers.js"
import { LarkError } from "../compiler/error.js"
import { lex } from "../compiler/lexer.js"

import {
    Block,
    CloseBrace,
    CloseBracket,
    CloseInterpolation,
    CloseParen,
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
    Terminator,
    Variable,
    Word
} from "../language/tokens.js"

export function * parse(source, {dev=false}={}) {

    /* This generator implements the parser stage, and yields an AST node for each top-level
    statement. Like the lexer stage, the specifics of parsing any given grammar are left to the
    corresponding grammar object in `objects.js`.

    The parser API object collects references to a subset of the functions defined below, and
    every token has `prefix` and `infix` methods that take a reference to the Parser API object.
    The `infix` methods also take a reference to the lefthand operand as a second argument. */

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

            } else throw new LarkError("expected a terminator", token.location);
        }

        if (nested) throw new LarkError("nested end of file", token.location);
    }

    function advance(returnNext=true) { // api function

        /* Advance the token stream by one token, updating the nonlocal `token`, then return the
        new token if the `returnNext` argument is truthy (the default). Otherwise, return the
        token that was current when the invocation was made. */

        [previous, token] = [token, tokens.next().value];

        ignoreInsignificantNewlines();

        return returnNext ? token : previous;
    }

    function on(...Classes) { // api function

        /* Optionally take one or more `Token` subclasses, if the current `token` is one of them,
        return it. If none of the subclasses match, return `false`. If no arguments are given, the
        function acts as a getter, just returning the current `token`. */

        if (Classes.length === 0) return token;

        for (const Class of Classes) if (token instanceof Class) return Class;

        return false;
    }

    function ignoreInsignificantNewlines() { // internal

        /* Advance the parser state until the current token is significant. When newlines are
        significant, all tokens are significant, so this is a noop. Otherwise, it skips over any
        newlines, stopping on the first non-newline token. */

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

        function validate(result, note) {

            /* Use the `validate` method of the token instance to check it, and complain if
            it is not valid (given the blocks it is nested within), also noting whether it's
            in the prefix or infix denotation. */

            if (result.validate(api)) return result.note(note);
            else throw new LarkError(`unexpected ${result.value} statement`, result.location);
        }

        let current, result;

        // use `current` to track the token we need to parse next when advancing the (nonlocal)
        // parser state to the next `token` in the stream, and use `result` to store the parse
        // tree is it's generated, first by an invocation of the `current` token's `prefix`
        // method, then by zero or more iterations of the following loop (which invokes
        // the `current` token's `infix` instead)...

        current = token;
        token = advance();
        result = validate(current.prefix(api, context), "prefixed");

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
            result = validate(current.infix(api, result), "infixed");
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

    function gatherCompoundExpression(closer) { // api function

        /* This function takes a subclass of `Closer` (not an instance) that it uses to gather a
        sequence of zero or more comma-separated expressions to form a `CompoundExpression` that
        is then returned.

        Note: It is important to restore the previous significance of newlines *before* advancing,
        to drop the the closing token as well as any insignificant newlines, immediately prior
        to returning the results (see the tail of the main while-loop). */

        const operands = new CompoundExpression(token.location);

        whitespace.top = false;

        while (not(on(closer))) {

            const operand = gatherExpression();

            operands.push(operand);

            if (on(Comma)) { advance(); continue } else if (on(closer)) break;

            const message = `expected a ${closer.name} character`;

            throw new LarkError(message, operands.at(-1).location);

        } whitespace.pop; advance(); return operands;
    }

    function gatherVariable() { // api function

        /* This function gathers a single variable token, then advances the parser, before
        returning the token (which is noted before advancing), complaining if the token is
        not a variable. */

        if (on(Variable)) return advance(false);
        else throw new LarkError("expected a variable", advance().location);
    }

    function gatherProperty() { // api function

        /* This function gathers a single property token, which can be a variable name or any
        kind of word (keyword, operator name, reserved word etc). If the token is a property,
        the function advances the parser, then returns the token (noting it before advancing),
        and simply complaining otherwise. */

        if (on(Word) || (on(Operator) && token.named)) return advance(false);
        else throw new LarkError("expected a property", token.location);
    }

    function gatherAssignees() { // api function

        /* This function is used for gathering a single group of one or more assignees, which
        need to be parsed carefully, as assignments, declarations and the various for-loops
        use grammars with an operator after the assignees. */

        token.expression = false;

        if (on(Variable)) return gatherVariable();

        if (on(OpenBracket, OpenBrace)) return gather();

        throw new LarkError("invalid assignee", token.location);
    }

    function gatherParameters() { // api function

        /* This function gathers the parameters for a function into a `Parameters` instance, which
        is returned. The operands will all be valid expressions, with no empty values, but are not
        otherwise validated (as function parameters).

        Note: Lark parameters do not use parens (so are not parsed as compound expressions).

        Note: Arrow functions use `gatherCompoundExpression` (implicitly, as the arrow will not be
        encountered until the parameters have already been parsed). */

        const result = new Parameters(token.location);

        if (on(OpenBrace)) return result;

        while (true) {

            result.push(gatherExpression());

            if (on(Comma)) advance();
            else if (on(OpenBrace)) break;
            else throw new LarkError("expected a comma or block", token.location);
        }

        return result;
    }

    function gatherBlock(functional=false) { // api function

        /* This function gathers a single formal statement or a braced block, containing any number
        of statements of any kind. The result is returned as a `Block` instance. It takes a single
        optional argument that determines whether to parse the block as a functon body (`true`),
        which must be wrapped in braces, or a control-flow block (`false`, the default), which
        will also accept a formal statement. */

        function gatherFormalStatement() {

            /* This function wraps the Pratt function to ensure that the result is a formal
            statement, complaining otherwise. */

            const candidate = gather();

            if (candidate instanceof Keyword) return candidate;
            else throw new LarkError("expected a formal statement", candidate.location);
        }

        const [braced, block] = [on(OpenBrace), new Block(token.location)];

        if (functional && not(braced)) throw new LarkError("expected a body", token.location);

        whitespace.top = true;

        if (functional) labelspace.top = {};

        if (braced) block.push(...LIST());
        else block.push(gatherFormalStatement());

        if (functional) labelspace.pop;

        whitespace.pop

        return block;
    }

    function label(name, value=undefined) { // api function

        /* This function is a getter-setter, used to get and set the state of labels. It takes a
        `Varaible` instance (`name`) and an optional ternary value (one of `true`, `false` or
        `null`). However, it is an error to set a given label more than once (reassign it).

        When the second argument is left undefined (getter mode), the function returns `true` when
        the given label is active and bound to a loop block, `false` when it is active and bound
        to a simple block, and `null` when it is inactive (as it cannot be found in the hash of
        labels on top of the `labelspace` stack).

        When the second argument is set (setter mode), its value is used to update the state of the
        given label on the `labelspace` stack.

        Note: The `labelspace` is a stack that has label-hashes pushed and popped automatically (by
        `gatherBlock`) whenever entering or leaving a function body. The hashes on the stack map
        label names (as strings) to their ternary state. */

        if (value === undefined) return labelspace.top[name.value] ?? null;

        if (value === null) return void delete labelspace.top[name.value];

        if (labelspace.top[name.value] === undefined) labelspace.top[name.value] = value;
        else throw new LarkError("cannot reassign active labels", left.location);
    }

    // initialze a hash, mapping the four notes used by compound expressions to the corresponding
    // closing token class (used internally, see `gatherCompoundExpression`)...

    const closers = Object.create(null);

    closers.parenthesized = CloseParen;
    closers.interpolated = CloseInterpolation;
    closers.bracketed = CloseBracket;
    closers.braced = CloseBrace;

    // gather the api functions and flags to form the parser api object, initialize the internal
    // state, then invoke `LIST` to yield the resulting top-level statements, one by one...

    const api = {
        advance,
        dev,
        gather,
        gatherVariable,
        gatherProperty,
        gatherExpression,
        gatherCompoundExpression,
        gatherParameters,
        gatherAssignees,
        gatherBlock,
        label,
        on
    };

    const labelspace = new Stack({});
    const whitespace = new Stack(true).on(false, ignoreInsignificantNewlines);

    const tokens = lex(source, {dev});

    let token, previous;

    yield * LIST(false);
}
