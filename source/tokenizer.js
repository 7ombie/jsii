import {
    empty,
    space,
    newline,
    dot,
    pound,
    quote,
    openBrace,
    closeBrace,
    whitespace,
    deadspace,
    terminators,
    delimiters,
    digits,
    symbolics,
    wordInitials,
} from "./strings.js"

import {
    Word,
    Comment,
    Operator,
    Delimiter,
    Terminator,
    NumberLiteral,
    StringLiteral,
} from "./objects.js"

export default function * tokenize(source, literate=false) {

    /* This function implements the lexer as a collection of helper functions, which
    it shares with the `lex` generator methods of the classes exported by `objects.js`,
    so they can handle the specifics of parsing the given token type, yielding one or
    more tokens of that type. */

    function advance() {

        /* This function takes no arguments, and advances the state of the lexer by one
        character, checking the character is legal, before returning it, or `undefined`
        if the source has been exhausted (or the interpolation, when inside a string). */

        index += 1;
        character = source[index];
        next = source[index + 1];

        // return `undefined` if the token stream is exhausted...

        if (interpolating) {

            if (on(closeBrace) && nesting === 0) return undefined;

        } else if (character === undefined) return undefined;

        // check the character is valid before returning it...

        const code = character.charCodeAt();

        if (code > 31 && code < 127 || code === 10) return character;

        const charcode = "0" + code.toString(16).toUpperCase().slice(-2);

        throw new SyntaxError(`illegal character code (0x${charcode})`);
    }

    function gatherWhile(characters, token=undefined) {

        /* Gather while the character is in the given character set. */

        while (at(characters) && advance()) if (token) token.value += character;
    }

    function gatherUntil(characters, token=undefined) {

        /* Gather up to (and not including) the first given character. */

        while ((!at(characters)) && advance()) if (token) token.value += character;
    }

    function read() { return character }

    function on(characters) { return characters.includes(character) }

    function at(characters) { return characters.includes(next) }

    function align() { onside = index; line++ } // note the start of the current line

    function interpolate(mode) { interpolating = mode } // expose `interpolating` via the api

    function locate() {

        /* This helper is used to generate a location value that encodes the current
        line and column numbers within a single `Number` value, by first multipling
        the line number by 256, then adding the column number to the result. Both
        values are zero-indexed. */

        return line * 256 + (index - onside - 1);
    }

	function * gatherStream() {

        /* This generator function contains the main loop and branches that tokenize
        the source (recuring to parse token streams inside of string interpolations).
        The actual tokenization is handled by the respective `Token` subclasses. */

	    while (advance()) {

            const location = locate();

			if (literate && (index - 1 === onside) && (!on(whitespace))) {

				yield * Comment.lex(api, location);

			} else if (on(space)) {

                continue;

            } else if (on(terminators)) {

                if (on(newline)) align();

                yield * Terminator.lex(api, location);

				while (at(deadspace) && advance()) if (on(newline)) align();

            } else if (on(pound)) {

                yield * Comment.lex(api, location);

            } else if (on(quote)) {

                yield * StringLiteral.lex(api, location);

            } else if (on(digits.decimal) || on(dot) && at(digits.decimal)) {

                yield * NumberLiteral.lex(api, location);

            } else if (on(symbolics)) {

                yield * Operator.lex(api, location);

            } else if (on(wordInitials)) {

                yield * Word.lex(api, location);

            } else if (on(delimiters)) {

				if (interpolating) {

                    if (on(openBrace)) nesting++;
				    else if (on(closeBrace)) nesting--;
                }

                yield * Delimiter.lex(api, location);

            } else throw new SyntaxError(`unexpected character (${character})`);
        }

		if (!interpolating) yield * Terminator.lex(api, locate());
    }

    // copy the lexer api functions into the object that is passed to the prefix
    // and infix methods, then initialize the lexer state (which is shared by all
    // local functions, and persists across recursive invocations), before using
    // the main lexer function to yield the token stream one token at a time...

    const api = {
        on, at, read, advance, interpolate,
        gatherStream, gatherWhile, gatherUntil
    };

	let [character, next, interpolating] = [empty, empty, false];
    let [index, onside, line, nesting] = [-1, -1, 0, 0];

	yield * gatherStream();
}
