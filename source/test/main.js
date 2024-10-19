import { lex } from "../core/lexer.js"
import { parse } from "../core/parser.js"
import { write } from "../core/writer.js"

window.lex = lex;
window.parse = parse;
window.write = write;
window.result = await fetch("source.lark");
window.source = await result.text();

console.log("SOURCE...\n---------\n\n" + source);

// for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source)) console.log("statement:", statement);
for (const string of write(source)) console.log(string);
