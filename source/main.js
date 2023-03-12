import lex from "./lexer.js"
import parse from "./parser.js"
import write from "./writer.js"

let source = `
(1 * 7, foo!bar, yield eggs)
lambda { return 1 }
end
`;

console.log("source...\n", source);

for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);

console.log("output...");
write(source);
