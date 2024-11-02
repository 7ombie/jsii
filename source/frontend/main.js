import { lex } from "../compiler/lexer.js"
import { parse } from "../compiler/parser.js"
import { write } from "../compiler/writer.js"
import { put } from "../compiler/helpers.js"

window.lex = lex;
window.parse = parse;
window.write = write;
window.result = await fetch("source.lark");
window.source = await result.text();

console.log("SOURCE...\n---------\n\n" + source);

// for (const token of lex(source)) put("token:", token);
for (const statement of parse(source, {dev: true})) put("statement:", statement);
for (const string of write(source, {dev: true})) put(string);

// function walk(statement) {

//     put("==>", statement);

//     for (const operand of statement) walk(operand);
// }

// for (const statement of parse(source, {dev: true})) walk(statement);
