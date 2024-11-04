import { parse } from "../compiler/parser.js"
import { put, Stack } from "../compiler/helpers.js"

export function * fix(source, {dev=false}={}) {

    function * walk(statements) {

        for (const statement of statements) { statement.fix(api, null); yield statement }
    }

    const api = {
        blockstack: new Stack(),
        loopstack: new Stack(),
        yieldstack: new Stack(),
        closurestack: new Stack(false)
    };

    yield * walk(parse(source, {dev}));
}
