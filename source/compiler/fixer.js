import { parse } from "../compiler/parser.js"
import { put, Stack } from "../compiler/helpers.js"

export function * fix(source, {dev=false}={}) {

    function * walk(statements) {

        for (const statement of statements) { statement.fix(api, null); yield statement }
    }

    const api = {
        yieldstack: new Stack(),        // looks for `yield` and `yield from` inside functions
        scopestack: new Stack({}),      // stores the namespaces that currently form the scope
        protostack: new Stack(true),    // tracks whether objects should infer null prototypes
        awaitstack: new Stack(true),    // tracks whether we're in an async function or onside
        paramstack: new Stack(false),   // tracks whether we're currently in a parameters list
        blockstack: new Stack(false),   // tracks whether we're in a block (within a function)
        loopstack:  new Stack(false),   // tracks whether we're in a loop (within in function)
        callstack:  new Stack(false),   // tracks whether we're in any kind of function at all
    };

    yield * walk(parse(source, {dev}));
}
