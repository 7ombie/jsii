import { lex } from "../compiler/lexer.js"
import { parse } from "../compiler/parser.js"
import { validate } from "../compiler/validator.js"
import { write } from "../compiler/writer.js"
import { put } from "../compiler/helpers.js"

const result = await fetch("source.lark");
const source = await result.text();
const underline = Array(128).fill("-").join("");

put(`SOURCE...\n${underline}\n${source}${underline}`);

// for (const token of lex(source)) put("token:", token);
// for (const statement of parse(source, {dev: true})) put("basic statement:", statement);
// for (const statement of fix(source, {dev: true})) put(statement);
for (const string of write(source, {dev: true})) put(string);
