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

    const name = register();

    w.preamble(`let ${name}`);

    return `(${name}=${expression.js(w)})?.ƥ${method}?.() ?? Object.${method}(${name} ?? ${fallback})`;
}

const register = iife(function() {

    /* This IIFE returns a function that generates and returns incrementing Lark register names (as
    strings) for assigning to unsafe expressions, so they're safe to use more than once. */

    let counter = 0; return () => lark(counter++);
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

        const typeName = register();

        w.preamble(`let ${typeName}`);

        return `(${typeName}=${type.js(w)})?.ƥis?.(${value.js(w)}) ?? ${value.js(w)} instanceof ${typeName}`;
    }

    if (type.safe) {

        const valueName = register();

        w.preamble(`let ${valueName}`);

        return `${type.js()}?.ƥis?.(${valueName}=${value.js(js)}) ?? ${valueName} instanceof ${type.js()}`;
    }

    const [valueName, typeName] = [register(), register()];

    w.preamble(`let ${valueName}, ${typeName}`);

    return `(${typeName}=${type.js(w)})?.ƥis?.(${valueName}=${value.js(w)}) ?? ${valueName} instanceof ${typeName}`;
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


