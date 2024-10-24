import { empty, space, openBrace, closeBrace, semicolon, newline } from "../core/ascii.js"
import { parse } from "../core/parser.js"
import { put } from "../core/helpers.js"

import { Token, Header, Label, Variable, Constant, NumberLiteral } from "../user/concrete.js"

export function * write(source, {dev=false}={}) {

    /* This function implements the Writer Stage. It is the entrypoint to the compiler. */

    function prefix(name) { // internal helper
    
        /* Take a value, prefix it with the Lark Character, and return the result as a string. This
        helper expects to be invoked on `String`s and (integer) `Number`s. */

        return `ƥ${name}`
    }

    function * walk(statements) { // internal helper

        /* Take a statement iterator, traverse it, and convert each statement to its corresponding
        JavaScript code, adding semi-colons as required. On each iteration, yield any preambles that
        were generated during the compilation of the statement, before yielding the JavaScript for
        the statement itself. */

        for (const statement of statements) if (statement.compile) {

            const terminated = statement instanceof Header || statement instanceof Label;
            const source = indentation + statement.js(api, true) + (terminated ? empty : semicolon);

            yield * preambles;
            yield source;

            preambles.length = 0;
        }
    }

    function writeBlock(statements) { // api function

        /* Take a a block of code, either as a statement iterator or a single statement token.
        Increase the indentation, `walk` the block and render each statement, before joining
        them with newlines and wrapping the result in curly braces. Finally, restore the
        previous indentation level, before returning the result. */

        const oldIndentation = indentation;

        indentation += space + space;

        if (statements instanceof Token) statements = [statements];

        const code = Array.from(walk(statements)).join(newline);

        indentation = oldIndentation;

        return openBrace + newline + code + newline + indentation + closeBrace;
    }

    function register(expression) { // api function

        /* Take an expression node and check if it's safe to evaluate more than once (either a
        simple, non-compound literal or a variable). If it is safe to reuse, expand it to JS,
        and return the resulting source. Otherwise, register the expression in a preamble,
        and return the register name instead.
        
        Note: Preambles are required when reusing operands in expressions (see the `in` and `of`
        infix-operators, for examples), as registers cannot be declared inside an expression. */

        if (expression.is(Variable, Constant, NumberLiteral)) return expression.js(api);

        const register = prefix(registerCounter++);

        preambles.push(`const ${register} = ${expression.js(api)};\n`);

        return register;
    }

    // gather the api functions and flags into the api object, initialize the internal state, then
    // walk the parse tree, yielding the results (as strings), one top-level statement at a time...

    const api = {register, write, writeBlock, dev};

    let indentation = empty;
    let registerCounter = 0;
    let preambles = [];

    yield * walk(parse(source, {dev}));
}

