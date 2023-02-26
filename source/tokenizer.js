import { quote, pound, dollar, bar, backslash, openBrace, closeBrace } from "./strings.js"
import { empty, space, newline, period, comma } from "./strings.js"
import { terminators, delimiters, qualifiers, bases } from "./strings.js"
import { digits, symbolics, whitespace, deadspace } from "./strings.js"
import { operators, keywords, reserves } from "./strings.js"
import { wordInitials, wordCharacters } from "./strings.js"

import { iife, not, put } from "./helpers.js"

export default function * tokenize(source, literate=false, script=false) {

	/* This generator takes a source string and yields its lexemes, one by one,
	in order, recursively handling string interpolations (to any depth). Note
	that qualifiers (like `is not` and `do async generator`) are not handled
	in this function, as the caller (`qualify`) can recursively classify
	and concatenate the individual lexemes together far more readily
	than in possible inside this generator. */

	function * gather(interpolating) {

		/* This generator emits the actual token stream. When it is called to
		to handle string interpolations, `interpolating` will be set to `true`,
		while it is always `false` outside of recursive invocations. */

		class Token {

			/* This class models a simple, unqualified token. The type is just
			a string, as it will often be mutated by the qualifier stage. We
			do not locate the end of the token, as that information is not
			conventionally used by JavaScript or source maps etc.*/

			constructor(type=empty, value=character) {

				 [this.type, this.value] = [type, value];
				 this.location = {line, column: index - onside};
			}
		}

	    function advance() {

			/* Advance the nonlocal `index`, `character` and `next` variables by one
			position, then validate `character` and return it, returning `undefined`
			if the source (or interpolation, if we're inside a string interpolation)
			is exhausted, and complaining if an illegal character is found.

			Note: The result of this function can be used as a predicate for the main
			loop to ensure the loop will always break. This idea is used by a number
			of `advance` functions across the codebase. */

			index += 1;
	        character = source[index];
	        next = source[index + 1];

			// check to see if we have exhausted the characters that `gather` was
			// called to tokenize, and return `undefined` if so...

			if (interpolating) {

				if (on(closeBrace) && nesting === 0) return undefined;

			} else if (character === undefined) return undefined;

			// now we know that there is another character, so attempt to validate
			// and return it, throwing an exception on an illegal character...

			const code = character.charCodeAt();

			if (code > 31 && code < 127 || code === 10) return character;

			const charcode = "0" + code.toString(16).toUpperCase().slice(-2);

			throw new SyntaxError(`illegal character code (0x${charcode})`);
	    }

		function on(characters) { return characters.includes(character) }

		function at(characters) { return characters.includes(next) }

		function onSOL() { return index - 1 === onside }

		function gatherWhile(characters) {

			/* Gather while the character is in the given character set. */

			while (at(characters) && advance()) token.value += character;
		}

		function gatherUntil(characters) {

			/* Gather up to (and not including) the first given character. */

			while (not(at(characters)) && advance()) token.value += character;
		}

		function terminate() {

			/* Check if we're on a newline. If so, update the nonlocal `line`
			and `onside` variables. Then, create and return a new terminator
			token (either way). */

			const endline = character === newline;

			if (endline) { onside = index; line++ }

			return new Token("terminator", endline ? "<LF>" : comma);
		}

		let token;

	    while (advance()) {

			// at this point, `index, `character` and `next` have been updated,
			// and we know that `character` is a legal character (and not EOF)...

			token = new Token();

			if (literate && onSOL() && not(on(whitespace))) {

				gatherUntil("\n");
				continue;
			}

	        if (on(space)) continue; // skip insignificant whitespace

	        if (on(terminators)) { // handle newlines and commas...

				yield terminate();

				while (at(deadspace) && advance()) if (on(terminators)) yield terminate();

				continue;
			}

			if (character === quote) { // handle string literals...

				// strings are gathered upto the closing quote, and recur
				// whenever an open brace is discovered, upto the closing
				// brace - the `value` is an array of strings and nested
				// arrays of tokens, representing interpolations...

				[token.type, token.value] = ["string-literal", [empty]];

				while (not(at(quote)) && advance()) {

					if (on(dollar) && at(openBrace)) { // handle string interpolation...

						advance();
						token.value.push(Array.from(gather(true)));
						token.value.push(empty);

					} else token.value[token.value.length - 1] += character;
				}

				advance(); // skip end-quote

			} else if (on(symbolics)) { // handle non-word operators...

				gatherWhile(symbolics);

				if (operators.includes(token.value)) token.type = "operator";
				else throw new SyntaxError(`unrecognized operator (${token.value})`);

			} else if (on(wordInitials)) { // handle words (generally)...

				// here, we just gather word tokens, without worrying if they are
				// keywords, operators, reserved etc, as the qualifier concatenates
				// subjects to qualifiers before classifying the result...

				token.type = "unclassified-word";

				gatherWhile(wordCharacters);

			} else if (on(digits.decimal)) { // handle all number literals...

				// check for a base-prefix, then gather the corresponding digits...

				if (on("0") && at(bases)) {

					token.value += advance();
					token.type = character.toLowerCase() === "x" ? "hexadecimal" : "binary";

				} else token.type = "decimal";

				gatherWhile(digits[token.type]);

				// complain if we found a base-prefix without any digits after it...

				if (token.value.length === 2 && token.type !== "decimal") {

					throw new SyntaxError("numeric prefix without digits");
				}

				// check for a dot - if found, and the type is decimal, gather and validate
				// the token as a float, else just use the current value as an integer...

				if (at(period) && token.type === "decimal") {

					token.value += advance();
					gatherWhile(digits[token.type]);
					token.type += "-float";

					if (character === period) throw new SyntaxError("fractions must be explicit");

				} else token.type += "-integer";

				token.type += "-number-literal";

			} else if (on(pound)) { // handle line comments...

				[token.type, token.value] = ["comment", empty];

				gatherUntil(newline);

				token.value = token.value.trim();

			} else if (character in delimiters) { // handle delimiters...

				token.type = delimiters[character];

				if (interpolating && on(openBrace)) nesting++;
				else if (interpolating && on(closeBrace)) nesting--;

			} else { // all else failed (for now)...

				throw new SyntaxError(`unexpected character (${character})`);

			} yield token;
	    }

		// ensure every (complete) token stream ends with at least one linefeed
		// terminator token, and always (finally) terminates with an EOF token...

		if (interpolating) return undefined;

		yield new Token("terminator", "<LF>");
		yield new Token("terminator", "<EOF>");
	}

	// create the variables that track across an entire source file, and return
	// a lexeme generator (as returned by the `gather` generator function)...

	let [character, next] = [empty, empty];
    let [index, onside, line, nesting] = [-1, -1, 1, 0];

	yield * gather(false);
}
