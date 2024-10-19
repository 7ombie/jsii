import { empty, space, openBrace, closeBrace, semicolon, newline } from "../core/ascii.js"
import { Token, Header } from "../user/concrete.js"
import { Label } from "../user/concrete.js"
import { parse } from "../core/parser.js"

export function * write(source, literate=false) {

    /* This function implements the Writer Stage. It is new, and just being sketched out. */

    function * walk(statements) { // internal helper

        /* Take a statement iterator, iterate over it, and convert each statement to JavaScript
        source, adding semi-colons as required. */

        for (const statement of statements) {

            if (statement instanceof Header || statement instanceof Label) {

                yield indentation + statement.js(api);

            } else yield indentation + statement.js(api) + semicolon;
        }
    }

    function writeBlock(statements) { // api function

        /* Take a statement iterator or a statement token that represents a block of code.
        Increase the indentation, walk the statements in the block, and render them as a
        string that starts and ends with a newline, before restoring the indentation,
        and returning the resulting string. */

        const oldIndentation = indentation;

        indentation += space + space;

        if (statements instanceof Token) statements = [statements];

        const code = Array.from(walk(statements)).join(newline);

        indentation = oldIndentation;

        return openBrace + newline + code + newline + closeBrace;
    }

    function register() { // api function

        /* Take no args, and return a globally unique lark register name as a string. */

        return `ƥ${registerCounter++}`;
    }

    // gather the api functions, initialize the internal state, and walk the parse tree, yielding
    // the results (as strings), one top-level statement at a time...

    const api = {write, writeBlock, register};

    let indentation = empty;
    let registerCounter = 0;

    yield * walk(parse(source, literate));
}

