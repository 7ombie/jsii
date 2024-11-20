import { lex } from "../compiler/lexer.js"
import { parse } from "../compiler/parser.js"
import { validate } from "../compiler/validator.js"
import { write } from "../compiler/writer.js"
import { put } from "../compiler/helpers.js"

const result = await fetch("source.lark");
const source = await result.text();
const underline = Array(128).fill("-").join("");
const child = document.createElement("child");

document.body.append(child);
put(`SOURCE...\n${underline}\n${source}\n${underline}`);

// for (const token of lex(source)) token.render(child);
for (const statement of validate(source, {dev: true})) statement.render(child);
window.scrollTo(0, document.body.scrollHeight);

// for (const token of lex(source)) put("Token:", token);
// for (const statement of parse(source, {dev: true})) put("Parse Tree:", statement);
// for (const statement of validate(source, {dev: true})) put("Statement:", statement);
for (const string of write(source, {dev: true})) put(string);
