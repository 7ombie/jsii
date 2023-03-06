import tokenize from "./tokenizer.js"
import parse from "./parser.js"

let source = `
x + 1
spam + "foo" // 2
`;

for (const token of tokenize(source)) {

    const type = token.constructor.name;
    const locator = `(${Math.floor(token.location / 256)}:${token.location % 256})`;

    console.log("token:", `${type}${locator}`.padEnd(24), token);
}

for (const statement of parse(source)) console.log("statement:", statement);

// TODO: make line a col numbers zero indexed
