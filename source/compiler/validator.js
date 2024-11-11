import { parse } from "./parser.js"
import { put, Stack } from "./helpers.js"

export function * validate(source, {dev=false}={}) {

    function * walk(statements) { // internal helper

        /* This function takes the iterator generated by the parser, iterates over the statements
        it generates, and calls the `validate` method on each, before yielding it. The `validate`
        method is implemented by `Token` to iterate over its operands, calling their `validate`
        methods, walking the token recursively.

        Subclasses of `Token` can override `validate` to interpolate their own validation logic,
        both for *making* their state valid and for ensuring that *it is* valid, before invoking
        `validate` on each of their children. */

        for (const statement of statements) { statement.validate(api, null); yield statement }
    }

    const globals = Object.create(null);

    const api = {
        yieldstack: new Stack(),        // looks for `yield` and `yield from` inside functions
        scopestack: new Stack(globals), // stores the namespaces that currently form the scope
        awaitstack: new Stack(true),    // tracks whether we're in an async function or onside
        paramstack: new Stack(false),   // tracks whether we're currently in a parameters list
        blockstack: new Stack(false),   // tracks whether we're in a block (within a function)
        loopstack:  new Stack(false),   // tracks whether we're in a loop (within in function)
        callstack:  new Stack(false),   // tracks whether we're in any kind of function at all
    };

    yield * walk(parse(source, {dev}));
}
