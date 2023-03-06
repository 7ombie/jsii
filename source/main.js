import tokenize from "./tokenizer.js"
import parse from "./parser.js"

let source = `
x + 1
spam + "foo" // 2
`;

for (const token of tokenize(source)) {

    // const { mass, pull, location, value } = token;

    // const type = token.constructor.name;
    // const locator = `[${Math.floor(location / 256)}:${token.location % 256}]`;
    // const weights = mass + pull ? `(${mass}, ${pull})` : ``;
    // const header = `${locator} ${type} ${weights}`.padEnd(32);

    console.log("token:", token);
}

for (const statement of parse(source)) console.log("statement:", statement);

// TODO: make line a col numbers zero indexed
