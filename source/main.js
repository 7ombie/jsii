import lex from "./lexer.js"
import parse from "./parser.js"








let source = `
function (foo) of x, y { return 1 }
function foo of x, y { return 1 }
function of x, y { return 1 }

throw new Foo()
dev if x { Y }
dev 1 + 2 * 3 + 3
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
