import {
    CloseBrace,
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

    /* This generator implements the parser stage, and yields an AST node for each
    top-level statement. Like the lexer stage, the specifics of parsing any given
    grammar are left to the corresponding grammar object in `objects.js`.

    The parser API object collects references to a subset of the functions defined
    below, and every token has `prefix` and `infix` methods that take a reference
    to the parser API object. The `infix` methods also take a reference to the
    lefthand production as a second argument.

    The `source` string and `literate` flag are passed along to the lexer stage. */

    function gather(RBP=0) { // internal

        /* This function implements Pratt's Algorithm. */

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

            if (on(CloseBrace) && nested) { advance(); return }

            if (on(Terminator)) continue;

            yield gatherStatement();

            if (on(Terminator)) continue;

            if (on(CloseBrace) && nested) { advance(); return }

            throw new SyntaxError("required terminator was not found");
        }

        if (nested) throw new SyntaxError("end of file inside block");
    }

    function advance(previous=false) { // api function

        /* Advance the token stream by one token, then return a reference to the newly current
        token, unless the `previous` argument is truthy. In which case, the previously current
        token is returned instead. */

        const old = token;

        [token, next] = [next, tokens.next().value];

        return previous ? old : token;
    }

    function on(type) { // api function

        /* Take a `Token` subclass, and return `true` if the current token is an
        instance of that class (or any subclass), else `false`. */

        return token instanceof type;
    }

    function at(type) { // api function

        /* Take a `Token` subclass, and return `true` if the *next token* is an
        instance of that class (or any subclass), else `false`. */

        return next instanceof type;
    }

    function gatherProperty() { // api function

        /* This function gathers a single property token, which can be a variable
        name or any kind of word (keyword, operator name, reserved word etc). If
        the token is a property, the function advances the parser, then returns
        the token (which is noted before advancing), complaining otherwise. */

        if (on(Word) || on(Operator) && token.named) return advance(true);
        else throw new SyntaxError("required a property (dint get one)");
    }

    function gatherVariable() { // api function

        /* This function gathers a single variable token, then advances the parser,
        before returning the token (which is noted before advancing), complaining
        if the token is not a variable. */

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
        stack, and the parsed block is returned. */

        let result;

        stack.push(type);

        if (on(OpenBrace)) result = [...LIST()];
        else result = gatherFormalStatement();

        stack.pop();

        return result;
    }

    function walk(stopcheck, onmatch, fallback=false) { // api function

        /* This function allows statements (like `return` and `break`) that can only
        appear in specific contexts to establish whether they are in a valid context.

        The process walks the nonlocal `stack` of integer block types backwards (from
        innermost to outermost), and calls the `stopcheck` callback on each type to
        establish whether to stop yet.

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

        for (let index = stack.length - 1; index >= 0; index--) {

            if (stopcheck(stack[index])) return onmatch(stack[index]);
        }

        return fallback;
    }

    const api = {
        advance, on, at,
        gatherVariable,
        gatherProperty,
        gatherExpression,
        gatherStatement,
        gatherFormalStatement,
        gatherBlock,
        walk,
    };

    const [stack, tokens] = [[2, -1], lex(source, literate)];

    let token, next;

    advance();

    yield * LIST(false);
}
