import lex from "./lexer.js"
import parse from "./parser.js"








let source = `
do async generator sum of x, y {

    if x > y { return foo } else { return bar }

    if this!pass exit
    return await 1

    do {1; 2; 3}

    foo = {
        a: 1,
        foo: 2 + 3,
        [spam]: yield foo,
        bar
    }
}
`;

console.log("source...\n", source);
console.log("output...");
for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
