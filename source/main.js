import lex from "./lexer.js"
import parse from "./parser.js"








let source = `
o is packed
o is sealed
o is frozen
pack new foo(x, y, z)
seal new foo
freeze 1 + 2 when x * y else spam + eggs
lambda x, y, args... { return x + y / args }
`;

console.log("source...\n", source);
console.log("output...");
// for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
