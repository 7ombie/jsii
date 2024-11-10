/* This module exports a bunch of helper functions that generally convert instances of `Token`
subclasses to JavaScript strings. This is convenient, as it allows us to keep all the really
long string literals in one file. */

import { put, not, lark, iife } from "../compiler/helpers.js"

// internal helpers...

function larkOrObjectMethod(w, expression, method, fallback) {

    /* Take a reference to the Writer Stage API, an expression node and a method name as a string,
    as well as a fallback-expression as a string (for when the expression is `null` or `void`).
    Renders an expression that tries to call a Lark method of the given name, falling back to
    a static method of `Object` (of the same name), using the given fallback expression to
    ensure that the `Object` method invocation does not choke on `null` or `void.

    Note: This method will register a preamble and use a Lark register as required. */

    if (expression.safe) return `${expression.js(w)}?.ƥ${method}?.() ?? Object.${method}(${expression.js(w)} ?? ${fallback})`;

    const mainRegister = register();

    w.preamble(`let ${mainRegister}`);

    return `(${mainRegister}=${expression.js(w)})?.ƥ${method}?.() ?? Object.${method}(${mainRegister} ?? ${fallback})`;
}

const register = iife(function() {

    /* This IIFE returns a function that generates and returns incrementing Lark register names (as
    strings) for assigning to unsafe expressions, so they're safe to use more than once. */

    let counter = 0n; return () => lark(counter++);
});

// api functions...

export function for_of(assignees, iterable, block) {

    /* Take the three parts of a for-of loop (as strings), then render and return the loop. */

    return `for (const ${assignees} of ${iterable}) ${block}`
}

export function keys(w, expression) {

    /* Wrap `larkOrObjectMethod` with a version for `keys` invocations. */

    return larkOrObjectMethod(w, expression, "keys", "{__proto__: null}");
}

export function values(w, expression) {

    /* Wrap `larkOrObjectMethod` with a version for `values` invocations. */

    return larkOrObjectMethod(w, expression, "values", "[]");
}

export function is(w, value, type) {

    /* Takes a reference to the Writer Stage API, a value node and a type node, and uses them to
    render a Lark `is` typecheck expression, minimizing the need to parenthesize safe arguments. */

    if (value.safe && type.safe) return `${type.js(w)}?.ƥis?.(${value.js(w)}) ?? ${value.js(w)} instanceof ${type.js()}`;

    if (value.safe) {

        const typeRegister = register();

        w.preamble(`let ${typeRegister}`);

        return `(${typeRegister}=${type.js(w)})?.ƥis?.(${value.js(w)}) ?? ${value.js(w)} instanceof ${typeRegister}`;
    }

    if (type.safe) {

        const valueRegister = register();

        w.preamble(`let ${valueRegister}`);

        return `${type.js()}?.ƥis?.(${valueRegister}=${value.js(js)}) ?? ${valueRegister} instanceof ${type.js()}`;
    }

    const [valueRegister, typeRegister] = [register(), register()];

    w.preamble(`let ${valueRegister}, ${typeRegister}`);

    return `(${typeRegister}=${type.js(w)})?.ƥis?.(${valueRegister}=${value.js(w)}) ?? ${valueRegister} instanceof ${typeRegister}`;
}

export function is_not(w, value, type) {

    /* Just inverts the `is` helper above. */

    return `!(${is(w, value, type)})`;
}

export function is_equal(w, x, y) {

    /* Renders the `==` operator, while trying to minimize parenthesis. */

    return `${x.safe ? x.js(w) : `(${x.js(w)})` }.ƥisEqual(${y.js(w)})`;
}

export function is_not_equal(w, x, y) {

    /* Implements the `!=` operator by inverting the `==` helper above. */

    return x.safe ? `!${is_equal(w, x, y)}` : `!(${is_equal(w, x, y)})`;
}
