import {
    closeParen,
    comma,
    delimiters,
    decimal,
    dot,
    empty,
    newline,
    openParen,
    pound,
    quote,
    space,
    symbolics,
    terminators,
    whitespace,
    wordInitials,
} from "../core/ascii.js"

import { LarkError } from "../core/error.js"
import { Operator, Terminator, Delimiter, Word } from "../user/concrete.js"
import { NumberLiteral, StringLiteral } from "../user/concrete.js"

export function * lex(source) {

    /* This function implements the lexer as a collection of helper functions, which
    it shares with the `lex` generator methods of the classes exported by `objects.js`,
    so they can handle the specifics of parsing the given token type, yielding one or
    more tokens of that type. */

    function advance() { // api function

        /* This function takes no arguments, and advances the state of the lexer by one
        character, checking the character is legal, before returning it, or `undefined`
        if the source has been exhausted (or the interpolation, when inside a string). */

        index += 1;
        character = source[index];

        if (interpolating) {

            if (on(closeParen) && nesting === 0) return undefined;

        } else if (character === undefined) return undefined;

        const code = character.charCodeAt();

        if (code > 31 && code < 127 || code === 10) return character;
        else throw new LarkError(`illegal character (${code})`, locate());
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

    function interpolate(mode=undefined) { // api function

        /* This function acts as a getter and setter, allowing the caller to set and
        query the value of `interpolating` via the API. When passed a bool, it acts
        as a setter (and returns nothing), while a call with no arguments queries
        the API (without changing the value). */

        if (mode === undefined) return interpolating;
        else interpolating = mode;
    }

    function terminate() { // api function

        /* Update `lastNewline` to track the index of the most recent newline, before
        updating the line number. Note: As new logical lines can be created by commas,
        the function must also check for a proper newline. */

        if (!on(newline)) return;

        lastNewline = index;
        line++;
    }

    function locate() { // api function

        /* Generate a location value that encodes the current line and column numbers within a
        single `Number`, by multipling the line number by 256, then adding the column number
        to the result.

        Note: The numbers are zero-indexed, limiting source files to 256 columns, and a few
        trillion lines. */

        return line * 256 + (index - lastNewline - 1);
    }

    function * gatherStream() { // api function

        /* This generator function contains the main loop and branches that tokenize
        the source (recuring to parse token streams inside of string interpolations).
        The actual tokenization is handled by the respective `Token` subclasses. */

        while (advance()) {
            
            if (on(space)) continue;
            
            const location = locate();

            if (on(terminators)) {

                yield * Terminator.lex(api, location);

                do { terminate() } while (at(comma + whitespace) && advance())

            } else if (on(quote)) {

                yield * StringLiteral.lex(api, location);

            } else if (on(decimal) || on(dot) && at(decimal)) {

                yield * NumberLiteral.lex(api, location);

            } else if (on(symbolics)) {

                yield * Operator.lex(api, location);

            } else if (on(wordInitials)) {

                yield * Word.lex(api, location);

            } else if (on(pound)) {

                gatherUntil(newline);

            } else if (on(delimiters)) {

                if (interpolating) {

                    if (on(openParen)) { nesting++ } else if (on(closeParen)) { nesting-- }
                }

                yield * Delimiter.lex(api, location);

            } else throw new LarkError(`unexpected character (${character})`, location);
        }

        yield * Terminator.lex(api, locate());
    }

    if (source instanceof Array) {

        // this block intervenes when an array of tokens is passed to the lexer (in practice,
        // via `parse`, which is called (recursively) by `StringLiteral.prefix` to parse the
        // tokens in string interpolations) as the `source` argument, allowing the parser to
        // parse tokens without change (as if they were just another source string)...

        yield * source; return;
    }

    // assuming the lexer was passed a string, initialize and run the lexer normally...

    const api = {
        on, at, advance,
        gatherWhile, gatherUntil, gatherStream,
        interpolate, locate, terminate,
        peek, read
    };

    let [character, interpolating] = [empty, false];
    let [index, line, lastNewline, nesting] = [-1, 0, -1, 0];

    yield * gatherStream();
}
