import lex from "./lexer.js"
import parse from "./parser.js"








let source = `
export default expression
export default function functionName { }
export default generator name { }
export default function { }

export { name1, nameN }
export { variable1 as name1, variable2 as name2, nameN }
export { variable1 as "string name" }
export { name1 as default }

export all from "module-name"
export all as name1 from "module-name"
export { name1, nameN } from "module-name"
export { import1 as name1, import2 as name2, nameN } from "module-name"
export { default, } from "module-name"
export { default as name1 } from "module-name"

import { "string name" as foo } from "module-name"
import foo, all as default from "./static/data.json" assert type: "json"
import "spam"
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
