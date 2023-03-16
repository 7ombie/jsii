import lex from "./lexer.js"
import parse from "./parser.js"








let source = `

delete foo of spam.eggs
do async generator of x, y {

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

x is y
x is not y
x is of y
x is not of y
x in y
x not in y
x of y
type of x
`;

console.log("source...\n", source);
console.log("output...");
for (const token of lex(source)) console.log("token:", token);
for (const statement of parse(source, false)) console.log("statement:", statement);
