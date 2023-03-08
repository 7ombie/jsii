import lex from "./lexer.js"
import parse from "./parser.js"
// import write from "./writer.js"

let source = `
0xFF.foo
do async lambda { return x + 1 }
do lambda { return x + 1 }
async lambda { debug }
do pass
`;

console.log("source:", source + "\noutput...");
for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
// write(source);
