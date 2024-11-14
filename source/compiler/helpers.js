/* This module defines and exports and bunch of unrelated stuff that is useful everywhere. */

export function lark(name) { return `ƥ${name}` } // prefix the argument with a lark character

export function iife(...args) {

    /* This helper replaces the dangling-dogballs operator. It takes a required callback, which is
    always the last argument. It also takes zero or more leading arguments, which are passed to the
    given function when its invoked. The result is returned. */

    return args.pop()(...args);
}

export function not(arg) { return ! arg } // replace the logical-not operator

export const put = console.log;

export class Stack extends Array {

    /* This implements a stack on top of `Array` that focuses on two properties, named `top` and
    `pop`. Assigning to `top` pushes the assigned value on to the stack, while reading from `top`
    references the value at the top of the stack (without any mutation). Referencing `pop` pops
    the stack, returning the popped value, while assiging to `pop` swaps the value on the top
    with the assigned value, returning the value that was removed from the stack. */

    constructor(...args) {

        /* Replace `Array.constructor` with a constructor that uses its arguments to initialize
        the array, without any special case for a single, numeric argument. */

        if (args.length === 1 && (typeof args[0] === "number" || args[0] instanceof Number)) {

            super();
            this.super.push(args[0]);

        } else super(...args);
    }

    get top() { return this.at(-1) }

    set top(value) { super.push(value) }

    get pop() { return super.pop() }

    set pop(value) { const result = this.pop; this.top = value; return result }

    get end() { return this.length - 1 }

    push() { throw new TypeError("Cannot 'push' to a 'Stack' instance") }
}
