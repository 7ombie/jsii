import lex from "./lexer.js"
import parse from "./parser.js"








let source = `
new foo(x, y, z)
new foo
1 + 2 when x * y else spam + eggs
lambda x, y, args... { return x + y / args }
`;

console.log("source...\n", source);
console.log("output...");
// for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
