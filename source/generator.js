import { semicolon } from "./strings.js"
import { Header } from "./objects.js"
import parse from "./parser.js"

export default function * generate(source, literate=false) {

    function * iterate(statements) {

        for (const statement of statements) {

            if (statement instanceof Header) yield statement.generate(api);
            else yield statement.generate(api) + semicolon;
        }
    }

    const api = {iterate};

    yield * iterate(parse(source, literate));
}

