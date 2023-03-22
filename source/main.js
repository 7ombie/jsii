import lex from "./lexer.js"
import parse from "./parser.js"








let source = `
class Foo {

    do until x > y { super.duper(x += 1) }
    private static foo, bar = 1
    private static async function sum of x, y { return await foo() }
}
`;

console.log("source...\n", source);
console.log("output...");
for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
