import lex from "./lexer.js"
import parse from "./parser.js"








let source = `
-> x + y
=> x = y
(x, y, z) => x * y + z
foo => foo * 2
`;

console.log("source...\n", source);
console.log("output...");
for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
