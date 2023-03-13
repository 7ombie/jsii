import lex from "./lexer.js"
import parse from "./parser.js"

let source = `
if x { return 0 }
else if y { return 1 }
else { return 2 }

(1, 2, 3 * 4, yield
    x
    *
    8
    )
foo!bar
(1, 2, foo!bar)
(,1,2 ,3,)
(,,,1,,,2,,,3,,,)
`;

console.log("source...\n", source);
console.log("output...");
for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
