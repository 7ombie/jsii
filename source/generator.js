import parse from "./parser.js"

export default function * (source, literate=false) {
  
    for (const statement of parse(source, literate)) {

        yield statement.generate();
    }
}

