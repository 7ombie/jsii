import lex from "./lexer.js"
import parse from "./parser.js"

let source = `
do {1; 2; 3}
a = {
    a: 1,
    foo: 2 + 3,
    [spam]: eggs,
    bar
}
`;

console.log("source...\n", source);
console.log("output...");
for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
