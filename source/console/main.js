import { lex } from "../compiler/lexer.js"
import { parse } from "../compiler/parser.js"
import { write } from "../compiler/writer.js"
import { fix } from "../compiler/fixer.js"
import { put } from "../compiler/helpers.js"

// window.lex = lex;
// window.parse = parse;
// window.write = write;

window.result = await fetch("source.lark");
window.source = await result.text();

// put("SOURCE...\n---------\n\n" + source);
// for (const token of lex(source)) put("token:", token);
// for (const statement of parse(source, {dev: true})) put("basic statement:", statement);
// for (const statement of fix(source, {dev: true})) put(statement);
for (const string of write(source, {dev: true})) put(string);
