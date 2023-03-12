import {
    closeBrace,
    deadspace,
    delimiters,
    digits,
    dot,
    empty,
    newline,
    openBrace,
    pound,
    quote,
    space,
    symbolics,
    terminators,
    whitespace,
    wordInitials,
} from "./strings.js"

import {
    Delimiter,
    NumberLiteral,
    Operator,
    StringLiteral,
    Terminator,
    Word,
} from "./objects.js"

export default function * (source, literate=false) {

    /* This function implements the lexer as a collection of helper functions, which
    it shares with the `lex` generator methods of the classes exported by `objects.js`,
    so they can handle the specifics of parsing the given token type, yielding one or
    more tokens of that type. */

    function align() {

        /* This helper is used internally to update `lastNewline` to note the index
        of the most recent newline, before updating the line number. As newlines
        can be implied by semicolons, we need to check for a proper newline. */

        if (!on(newline)) return;

        lastNewline = index;
        line++;
    }

    function locate() {

        /* This helper is used internally to generate a location value that encodes
        the current line and column numbers within a single `Number`, by multipling
        the line number by 256, then adding the column number to the result.

        Note: The numbers are zero-indexed, limiting source files to 256 columns,
        and a few trillion lines. */

        return line * 256 + (index - lastNewline - 1);
    }

    function advance() { // api function

        /* This function takes no arguments, and advances the state of the lexer by one
        character, checking the character is legal, before returning it, or `undefined`
        if the source has been exhausted (or the interpolation, when inside a string). */

        index += 1;
        character = source[index];

        if (interpolating) {

            if (on(closeBrace) && nesting === 0) return undefined;

        } else if (character === undefined) return undefined;

        const code = character.charCodeAt();

        if (code > 31 && code < 127 || code === 10) return character;
        else throw new SyntaxError(`illegal character (${code})`);
    }

    function gatherWhile(characters, token=undefined) { // api function

        /* Gather while the next character is in the given character set. */

        while (at(characters) && advance()) if (token) token.value += character;
    }

    function gatherUntil(characters, token=undefined) { // api function

        /* Gather while the next character is not in the given character set. */

        while ((!at(characters)) && advance()) if (token) token.value += character;
    }

    function read() { // api function

        /* This function exposes the value of `character` via the API. */

        return character;
    }

    function on(characters) { // api function

        /* This function takes a string, representing a character or character set,
        and returns `true` if the current character is in the set, else `false`. */

        return characters.includes(character);
    }

    function at(characters) { // api function

        /* This function takes a string, representing a character or character set,
        and returns `true` if the *next character* is in the set, else `false`. */

        return characters.includes(source[index + 1]);
    }

    function peek(offset, characters) { // api function

        /* This function takes a string, representing a character or character set,
        and returns `true` if the *character following the next character* is in the
        set, else `false`. */

        return characters.includes(source[index + offset]);
    }

    function interpolate(mode) { // api function

        /* This function permits setting the value of `interpolating` via the API. */

        interpolating = mode;
    }

    function * gatherStream() { // api function

        /* This generator function contains the main loop and branches that tokenize
        the source (recuring to parse token streams inside of string interpolations).
        The actual tokenization is handled by the respective `Token` subclasses. */

        while (advance()) {

            const location = locate();

            if (literate && (index - 1 === lastNewline) && (!on(whitespace))) {

                lexer.gatherUntil(newline);

            } else if (on(space)) {

                continue;

            } else if (on(terminators)) {

                yield * Terminator.lex(api, location);

                do { align() } while (at(deadspace) && advance())

            } else if (on(quote)) {

                yield * StringLiteral.lex(api, location);

            } else if (on(digits.decimal) || on(dot) && at(digits.decimal)) {

                yield * NumberLiteral.lex(api, location);

            } else if (on(symbolics)) {

                yield * Operator.lex(api, location);

            } else if (on(wordInitials)) {

                yield * Word.lex(api, location);

            } else if (on(pound)) {

                yield * Comment.lex(api, location);

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

    const api = {
        advance, on, at, peek, read, interpolate, gatherWhile, gatherUntil, gatherStream
    };

    let [character, interpolating] = [empty, false];
    let [index, line, lastNewline, nesting] = [-1, 0, -1, 0];

    yield * gatherStream();
}
