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
    the stack, returning the popped value, while assiging to `pop` is a noop (the property is
    readonly).

    There is also a private `Map`, named `triggers` that maps keys to callbacks (as registered by
    the `on` method). The callbacks are invoked whenever changes to the stack make `top` equal to
    that callback's registered key.

    The constructor has also been overridden, so its arguments are *always* used to initialize the
    stack (removing the special-case that takes a length). */

    #triggers = new Map();

    constructor(...args) {

        /* Replace `Array.constructor` with a constructor that uses its arguments to initialize
        the array, without any special case for a single, numeric argument. */

        if (args.length === 1 && (typeof args[0] === "number" || args[0] instanceof Number)) {

            super();
            this.push(args[0]);

        } else super(...args);
    }

    get top() {

        /* Return the value that's on top of the stack (without mutating the stack). */

        return this.at(-1);
    }

    set top(value) {

        /* Push the rvalue to the stack, without returning anything (assigning evaluates to the
        value assigned). If the new `top` value is an event trigger, invoke its callback. */

        this.push(value);
        this.#triggers.get(value)?.();
    }

    get pop() {

        /* Pop the stack. If the new `top` value is an event trigger, invoke its callback. Either
        way, return the result of popping the stack. */

        const result = super.pop();

        this.#triggers.get(this.top)?.();

        return result;
    }

    set pop(value) {

        /* Pop the stack (using `this.pop` to trigger any registered events), then push the given
        `value`, before returning the result of popping the stack (swapping the top). */

        const result = this.pop;

        this.top = value;

        return result;
    }

    get end() { return this.length - 1 }

    on(value, callback) {

        /* Chainable method that takes a map key and callback, and uses the given key to store its
        callback in the private `triggers` map. The callback will be invoked (without arguments)
        whenever the `set top` or `get pop` methods leave the stack with a `top` value equal
        to the key value (whenever the current value becomes equal to a trigger value). */

        this.#triggers.set(value, callback);

        return this;
    }
}
