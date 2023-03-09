import lex from "./lexer.js"
import parse from "./parser.js"
import write from "./writer.js"

let source = ` # TODO: test the numbers (especially dots and 'onPoint')
0xE4.toString
`;

console.log("source:", source + "\noutput...");
write(source);

for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
