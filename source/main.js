import lex from "./lexer.js"
import parse from "./parser.js"








let source = `
subclass Employee of Person {}
foo = subclass of Animal {}
foo || b
x **= 1
$elijah //= 45
foo.bar(spam, eggs % inc x)
dec x.y.z[a.b.c]
(x, y, ...z) => x and y or z
=> foo * 2
`;

console.log("source...\n", source);
console.log("output...");
for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
