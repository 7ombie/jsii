import parse from "./parser.js"

export default function * generate(source, literate=false) {

    for (const statement of parse(source, literate)) yield statement.generate();
}

