import { Token } from "./objects.js"
import parse from "./parser.js"

export default function(source, literate) {

    const output = [];
    let indent = 0;

    function push(...args) {

        args.forEach(arg => output.push(arg));
    }

    function openPredicatedBlock(keyword, predicate) {

        // TODO: indent etc...
        push("\n", keyword, " (");
        predicate.write(api);
        push(") {");
    }

    function openUnconditionalBlock(keyword) {

        push("\n", keyword, " {");
    }

    function openBlock() {

        push("{");
    }

    function closeBlock(keyword) {

        push("}");
    }

    function indentStatements(statements) {

        // TODO: indent etc...

        if (statements instanceof Token) statements.write(api);
        else for (const statement of statements) { statement.write(api) }
    }

    const api = {
        push, openBlock, openPredicatedBlock, openUnconditionalBlock, closeBlock, indentStatements
    };

    for (const statement of parse(source, literate)) { statement.write(api); push("\n") }

    console.log(output.join(""));
}
