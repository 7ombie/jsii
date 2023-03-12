import {
    CloseBrace,
    Comma,
    EOF,
    Keyword,
    OpenBrace,
    Operator,
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

        while (RBP < token.LBP) {

            current = token;
            token = advance();
            result = current.infix(api, result);
        }

        return result;
    }

    function * LIST(nested=true) { // internal

        /* This is the (often) recursive, block-level parsing function that wraps the Pratt
        parser to implement a statement grammar that replaces ASI with LIST (Linewise Implicit
        Statement Termination). */

        while (advance()) {

            if (on(EOF)) break;

            if (on(CloseBrace) && nested) return advance();

            if (on(Terminator)) continue;

            yield gatherStatement();

            if (on(Terminator)) continue;

            if (on(CloseBrace) && nested) return advance();

            throw new SyntaxError("required terminator was not found");
        }

        if (nested) throw new SyntaxError("end of file inside block");
    }

    function advance(previous=false) { // api function

        /* Advance the token stream by one token, updating the nonlocal `token` and `next`
        variables, then return a reference to the newly current token, unless the `previous`
        argument is truthy. In which case, the token that was current before the invocation
        is returned instead. */

        const old = token;

        [token, next] = [next, tokens.next().value];

        return previous ? old : token;
    }

    function on(...types) { // api function

        /* Take any number of `Token` subclasses, and return `true` if the current `token` is
        an instance of a given class (or any subclass), else `false`. */

        for (const type of types) if (token instanceof type) return true;

        return false;
    }

    function at(...types) { // api function

        /* Take any number of `Token` subclasses, and return `true` if the `next` token is an
        instance of a given class (or any subclass), else `false`. */

        for (const type of types) if (next instanceof type) return true;

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
        a formal statement or expression) is valid in the current context (block-wise). */

        const candidate = gather();

        if (candidate.validate(api)) return candidate;
        else throw new SyntaxError("invalid statement type, given current context");
    }

    function gatherFormalStatement() { // api function

        /* This function wraps the Pratt function to ensure that the result is an expression
        (not a formal statement), complaining otherwise. */

        const candidate = gatherStatement();

        if (candidate instanceof Keyword) return candidate;
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

        blockStack.push(type);

        if (functional) listStack.push(true);

        let result = braced ? [...LIST()] : gatherFormalStatement();

        if (functional) listStack.pop();

        blockStack.pop();

        return result;
    }

    function gatherCompoundExpression(closer, validate=expression=>expression) {

        /* This function takes a `Token` subclass which indicates which closing character to
        use to gather a compound statement (a sequence of comma-separated expressions, wrapped
        in parens, brackets or braces), as well as taking an optional function that validates
        each expression, as they are gathered into a results array and returned, defaulting
        to a function that validates any valid expression.

        The validator function is passed a reference to the expression it needs to check, as
        well as a reference to the results array. It should raise an exception or return a
        valid expression.

        The results array may contain `null` expressions, as adjacent commas can be used to
        imply empty expressions (though these expression are implicitly valid). */

        if (validate === undefined) validate = expression => expression;

        const results = [];

        listStack.push(false);

        while (!on(closer)) {

            if (on(Comma)) { results.push(null); advance(); continue }

            results.push(validate(gatherExpression(), results));

            if (on(closer)) { advance(); break }

            if (on(Comma)) { advance(); continue }

            throw new SyntaxError("sequence delimiter not found");
        }

        listStack.pop();

        return results;
    }

    function walk(stopcheck, onmatch, fallback=false) { // api function

        /* This function allows statements (like `return` and `break`) that can only
        appear in specific contexts to establish whether they are in a valid context.

        The process walks the nonlocal `blockStack` of integer block types backwards
        (from innermost to outermost), and calls the `stopcheck` callback on each
        type to establish whether to stop yet.

        If the loop stops, the `onmatch` callback is passed the type that was stopped
        on, and the result is returned. The `fallback` bool is returned if the stack
        is exhausted without a match.

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
        range, and falling back to a bool if the top-level is reached. */

        for (let index = blockStack.length - 1; index >= 0; index--) {

            if (stopcheck(blockStack[index])) return onmatch(blockStack[index]);
        }

        return fallback;
    }

    const api = {
        advance, on, at,
        gatherVariable,
        gatherProperty,
        gatherExpression,
        gatherCompoundExpression,
        gatherStatement,
        gatherFormalStatement,
        gatherBlock,
        walk,
    };

    const blockStack = [];
    const listStack = [true];
    const tokens = lex(source, literate);

    let token, next;

    advance();

    yield * LIST(false);
}
