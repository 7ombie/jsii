import lex from "./lexer.js"
import parse from "./parser.js"
import write from "./writer.js"

let source = `
1.2.toString
do async lambda { return 1 }
`;

console.log("source:", source + "\noutput...");
write(source);

for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
