import lex from "./lexer.js"
import parse from "./parser.js"
import generate from "./generator.js"

let result = await fetch("source.lark");
let source = await result.text();

console.log(source);
// for (const token of lex(source)) console.log("token:", token);
// for (const statement of parse(source)) console.log("statement:", statement);
for (const string of generate(source)) console.log(string);
