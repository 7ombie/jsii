import { semicolon, newline } from "./strings.js"
import { Token, Header, Label } from "./objects.js"
import parse from "./parser.js"

export default function * generate(source, literate=false) {

    function * walk(statements) {

        for (const statement of statements) {

            if (statement instanceof Header || statement instanceof Label) yield indentation + statement.js(api);
            else yield indentation + statement.js(api) + semicolon;
        }
    }

    function block(statements) {

        const oldIndentation = indentation;

        indentation += "  ";

        if (statements instanceof Token) statements = [statements];

        const code = Array.from(walk(statements)).join(newline);

        indentation = oldIndentation;

        return newline + code + newline;
    }

    const api = {walk, block};

    let indentation = "";

    yield * walk(parse(source, literate));
}

