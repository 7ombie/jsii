import { empty, space, openBrace, closeBrace, semicolon, newline } from "../compiler/ascii.js"
import { put, not, lark, Stack } from "../compiler/helpers.js"
import { fix } from "../compiler/fixer.js"

import { Header, Label } from "../language/tokens.js"

export function * write(source, {dev=false}={}) {

    /* This function implements the Writer Stage. It is the entrypoint to the compiler. */

    function * walk(statements) { // internal helper

        /* Take a statement iterator, traverse it, and convert each statement to its corresponding
        JavaScript code, adding semi-colons as required. On each iteration, yield any preambles that
        were generated during the compilation of the statement, before yielding the JavaScript for
        the statement itself. */

        preambles.top = [];

        for (const statement of statements) if (statement["ignore"] === false) {

            const terminated = statement.is(Header, Label);
            const javascript = statement.js(api);

            yield * preambles.top.map(preamble => indentation + preamble);
            yield indentation + javascript + (terminated ? empty : semicolon);

            preambles.top.length = 0;
        }

        preambles.pop;
    }

    function writeBlock(block) { // api function

        /* Take a `Block` instance. Increase the indentation, `walk` the block and render each
        statement, before joining them with newlines and wrapping the result in curly braces.
        Finally, restore the previous indentation level, before returning the result. */

        indentation += space + space;

        const code = Array.from(walk(block)).join(newline);

        indentation = indentation.slice(2);

        return openBrace + newline + code + newline + indentation + closeBrace;
    }

    function register(declare=true) { // api function

        /* Increment the register counter to generate and return a Lark register name, declaring it
        in a preamble if the optional argument is truthy (the default). */

        const register = lark(registerCounter++);

        if (declare) preamble(`let ${register}`);

        return register;
    }

    function preamble(string, terminate=true) { // api function

        /* Take a preamble string and an optional bool. Push the string to the `preambles` array,
        concatenating a semicolon to the end if `terminated` is truthy (the default). */

        preambles.top.push(string + (terminate ? semicolon : empty));
    }

    // gather the api functions and flags into the api object, initialize the internal state, then
    // walk the parse tree, yielding the results (as strings), one top-level statement at a time...

    const api = {preamble, register, write, writeBlock, dev};

    let indentation = empty;
    let registerCounter = 0;
    let preambles = new Stack();

    yield * walk(fix(source, {dev}));
}

