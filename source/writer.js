import { semicolon, newline} from "./strings.js"
import { Token, Header, Label } from "./objects.js"
import { parse } from "./parser.js"

export function * write(source, literate=false) {

    /* This function implements the Writer Stage. It is new, and just being sketched out. */

    function * walk(statements) { // api function

        /* Take a statement iterator, iterate over it, and convert each statement to JavaScript
        source, adding semi-colons as required. */

        for (const statement of statements) {

            if (statement instanceof Header || statement instanceof Label) {
                
                yield indentation + statement.js(api);
            
            } else yield indentation + statement.js(api) + semicolon;
        }
    }

    function block(statements) { // api function

        /* Take a statement iterator or a statement token that represents a block of code. If it
        is a statement token, it will be wrapped in an array (by this function) automatically,
        so it acts like a (one-item) iterable in any case. Increase the indentation, walk the
        statements in the block, and render the contents of the block, adding whitespace,
        before returning the result as a string. */

        const oldIndentation = indentation;

        indentation += "  ";

        if (statements instanceof Token) statements = [statements];

        const code = Array.from(walk(statements)).join(newline);

        indentation = oldIndentation;

        return newline + code + newline;
    }

    // gather the api functions, initialize the internal state, and walk the parse tree, yielding
    // the results (as strings), one top-level statement at a time...

    const api = {walk, block};

    let indentation = "";

    yield * walk(parse(source, literate));
}

