import { empty, space, openBrace, closeBrace, semicolon, newline } from "../compiler/ascii.js"
import { put, not, Stack } from "../compiler/helpers.js"
import { validate } from "../compiler/validator.js"

import { Header, Label } from "../language/tokens.js"

export function * write(source, {dev=false}={}) {

    /* This function implements the Writer Stage. It is the entrypoint to the compiler. */

    function * walk(statements) { // internal helper

        /* Take a statement iterator, traverse it, and convert each statement to its corresponding
        JavaScript code, adding semi-colons as required. On each iteration, yield any preambles that
        were generated during the compilation of the statement, before yielding the JavaScript for
        the statement itself.

        When invoking the token's `js` method, the API object is passed twice, once as the API, and
        again as the context. This makes `context === w` when the grammar is at the top level of
        the block that contains it. */

        preambles.top = [];
        proambles.top = [];

        for (const statement of statements) if (not(statement.ignored)) {

            const js = statement.js(api, null);
            const terminated = statement.terminated;

            yield * preambles.top.map(preamble => indentation + preamble);

            if (js !== undefined) yield indentation + js + (terminated ? empty : semicolon);

            yield * proambles.top.map(proamble => indentation + proamble);

            preambles.top.length = proambles.top.length = 0;
        }

        preambles.pop;
        proambles.pop;
    }

    function writeBlock(block) { // api function

        /* Take a `Block` instance and `walk` its (statement) operands, rendering each one, before
        joining them with newlines. The indentation (inside the block) is increased one level, and
        the block is wrapped in curly braces. */

        indentation += space + space;

        const code = Array.from(walk(block)).join(newline);

        indentation = indentation.slice(2);

        return openBrace + newline + code + newline + indentation + closeBrace;
    }

    function preamble(string, terminate=true) { // api function

        /* Take a preamble string and an optional bool. Push the string to the `preambles` array,
        concatenating a semicolon to the end if `terminated` is truthy (the default). */

        preambles.top.push(string + (terminate ? semicolon : empty));
    }

    function proamble(string, terminate=true) { // api function

        /* Take a proamble string and an optional bool. Push the string to the `proambles` array,
        concatenating a semicolon to the end if `terminated` is truthy (the default). */

        proambles.top.push(string + (terminate ? semicolon : empty));
    }

    // gather the api functions and flags into the api object, initialize the internal state, then
    // walk the parse tree, yielding the results (as strings), one top-level statement at a time...

    const api = {preamble, proamble, write, writeBlock, dev};

    let indentation = empty;
    let preambles = new Stack();
    let proambles = new Stack();

    yield * walk(validate(source, {dev}));
}

