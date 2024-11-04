import { parse } from "../compiler/parser.js"
import { put } from "../compiler/helpers.js"

export function * fix(source, {dev=false}={}) {

    function * walk(statements) {

        for (const statement of statements) { statement.fix(api, null); yield statement }
    }

    const api = {};

    yield * walk(parse(source, {dev}));
}
