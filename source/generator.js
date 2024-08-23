import { semicolon } from "./strings.js"
import { Header } from "./objects.js"
import parse from "./parser.js"

export default function * generate(source, literate=false) {

    function * walk(statements) {

        for (const statement of statements) {

            if (statement instanceof Header) yield statement.js(api);
            else yield statement.js(api) + semicolon;
        }
    }

    const api = {walk};

    yield * walk(parse(source, literate));
}

