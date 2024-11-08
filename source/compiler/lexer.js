import { put, not } from "../compiler/helpers.js"
import { LarkError } from "../compiler/error.js"

import {
    closeParen,
    comma,
    delimiters,
    decimal,
    dot,
    empty,
    newline,
    pound,
    quote,
    space,
    operationals,
    terminators,
    whitespace,
    wordInitials
} from "../compiler/ascii.js"

import {
    Delimiter,
    NumberLiteral,
    Operator,
    StringLiteral,
    Terminator,
    Word,
} from "../language/tokens.js"

export function * lex(source, {dev=false}={}) {

    /* This function implements the lexer as a collection of helper functions, which it shares with
    the `lex` generator methods of the classes exported by `objects.js`, so they can handle the
    specifics of parsing the given token type, yielding one or more tokens of that type. */

    function advance(stride=1) { // api function

        /* This function takes an optional number of characters to advance by, defaulting to `1`,
        and advances that far. If the new character exists and is is legal, it's returned. If the
        source becomes exhausted, `undefined` is returned, and if the new character's illegal,
        an exception is raised. */

        index += stride;
        character = source[index];

        if (character === undefined) return undefined;

        const code = character.charCodeAt();

        if (code > 31 && code < 127 || code === 10) return character;
        else throw new LarkError(`illegal character (${code})`, locate());
    }

    function gatherWhile(characters, token=undefined) { // api function

        /* Gather while the next character is in the given character set. */

        while (at(characters) && advance()) if (token) token.value += character;

        return token;
    }

    function gatherUntil(characters, token=undefined) { // api function

        /* Gather while the next character is not in the given character set. */

        while ((not(at(characters))) && advance()) if (token) token.value += character;

        return token;
    }

    function read() { // api function

        /* This function exposes the value of `character` via the API. */

        return character;
    }

    function on(characters) { // api function

        /* This function takes a string, representing a character or character set, and returns
        `true` if the current character is in the set, else `false`. */

        return characters.includes(character);
    }

    function at(characters) { // api function

        /* This function takes a string, representing a character or character set, and returns
        `true` if the *next character* is in the set, else `false`. */

        return characters.includes(source[index + 1]);
    }

    function peek(offset, characters) { // api function

        /* This function takes a string, representing a character or character set, and returns
        `true` if the *character following the next character* is in the set, else `false`. */

        return characters.includes(source[index + offset]);
    }

    function terminate() { // api function

        /* Update `lastNewline` to track the index of the most recent newline, before updating the
        line number. Note: As new logical lines can be created by commas, the function must also
        check for a proper newline. */

        if (not(on(newline))) return;

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

    function * gather(interpolating=false) { // api function

        /* This generator function contains the main loop and branches that tokenize the source
        (recuring to parse token streams inside of string interpolations). The actual tokenization
        is handled by the respective `Token` subclasses. The function takes an optional bool that
        defaults to `false`. If `true`, the function lexes an interpolation (which continues up
        to the next closing paren). By default, it lexes a source file (the entire string). */

        while (character) {

            const location = locate();

            branches: if (on(space)) {

                break branches;

            } else if (on(terminators)) {

                yield * Terminator.lex(api, location);

                do { terminate() } while (at(comma + whitespace) && advance())

            } else if (on(quote)) {

                yield * StringLiteral.lex(api, location);

            } else if (on(decimal) || on(dot) && at(decimal)) {

                yield * NumberLiteral.lex(api, location);

            } else if (on(operationals)) {

                yield * Operator.lex(api, location);

            } else if (on(wordInitials)) {

                yield * Word.lex(api, location);

            } else if (on(pound)) {

                gatherUntil(newline);

            } else if (on(delimiters)) {

                if (interpolating && on(closeParen)) return; // interpolation exit point
                else yield * Delimiter.lex(api, location);

            } else throw new LarkError(`unexpected character (${character})`, location);

            advance();
        }

        yield * Terminator.lex(api, locate());                  // exit-point for source files
    }

    // gather the api functions and flags to form the lexer api object, then initialize all of the
    // variables to point to the character *prior* to the start of the source, then `advance` once
    // to ensure everything is initialized and validated correctly, before invoking `gather` and
    // yielding the resulting tokens, one by one...

    const api = {
        on, at, advance,
        gather, gatherWhile, gatherUntil,
        locate, terminate,
        peek, read,
        dev
    };

    let [character, index, line, lastNewline] = [empty, -1, 0, -1];

    advance(); yield * gather();
}
