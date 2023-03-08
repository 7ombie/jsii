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

    function closeBlock(keyword) {

        push("}");
    }

    function indentStatements(statements) {

        // TODO: indent etc...
        for (const statement of statements) { statement.write(api) }
    }

    const api = {
        push, openPredicatedBlock, openUnconditionalBlock, closeBlock, indentStatements
    };

    for (const statement of parse(source, literate)) {

        // console.log(statement);
        statement.write(api);
        console.log(output.join(""));
    }
}
