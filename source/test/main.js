import { put, not } from "../core/helpers.js"
import { lex } from "../core/lexer.js"
import { parse } from "../core/parser.js"
import { write } from "../core/writer.js"
import { Token, In } from "../user/concrete.js"

window.lex = lex;
window.parse = parse;
window.write = write;
window.result = await fetch("source.lark");
window.source = await result.text();

console.log("SOURCE...\n---------\n\n" + source);

// for (const token of lex(source)) console.log("token:", token);
// for (const statement of parse(source, {dev: true})) console.log("statement:", statement);
for (const string of write(source, {dev: true})) console.log(string);

function test(statement) {

    for (const operand of statement.operands) {

        put("==>", operand);

        if (operand === false) put("!!!", statement);

        if (not(operand instanceof Token)) return false;

        if (test(operand) === false) return false;
    }

    return true;
}

for (const statement of parse(source, {dev: true})) {

    if (test(statement)) continue;
    else break;
}
