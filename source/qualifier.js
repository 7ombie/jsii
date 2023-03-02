import { operators, keywords, reserves, qualifiers, constants } from "./strings.js"
import { openingDelimiterTypes } from "./strings.js"
import tokenize from "./tokenizer.js"

import { iife, not, put } from "./helpers.js"

function upgradeWord(token) {

	/* Take a word type token (that may be qualified (like `not in`)) and
	type it to a keyword, operator, qualifier, reserved word or variable,
	then return it.

    Note: While this function may return a unfinished qualifier or reserved
    word, that is required, as we still accept those words as property names,
    so must wait until the parser establishes the context. */

	if (keywords.includes(token.value)) token.type = "key-word";
	else if (constants.includes(token.value)) token.type = "constant-word";
	else if (operators.includes(token.value)) token.type = "operator-word";
	else if (token.value in qualifiers) token.type = "qualifier-word";
	else if (reserves.includes(token.value)) token.type = "reserved-word";
	else token.type = "variable-name";

	return token;
}

export default function * qualify(source, literate=false, script=false) {

	/* This generator function takes a source string and yields its tokens,
	one by one, in order. The `tokenize` generator does most of the work, as
	it actually slices the source into individual lexemes. However, there are
	no (sane) schemes for (recursively) gathering and validating qualifiers,
	so we pass the lexemes through this generator that can look-ahead by one
	token to combine prefix tokens with the token or tokens that they prefix,
	forming single tokens (with values like `not in` and `do async lambda`)
	as required. */

	function advance() {

		/* Advance the token stream by one token, updating the nonlocal
		`token` and `next` variables, then return a reference to `token`,
		so the result is only truthy until we run out of tokens. */

		return ([token, next] = [next, tokens.next().value])[0];
	}

	function classify(token) {

        // if it is not a word token, it cannot be a qualifier...

		if (not(token.type.endsWith("-word"))) return token;

		// keep a reference to the current token, and grab the array of things
		// it can qualify (which may be `undefined` if there are none)...

		const [reference, suffixes] = [token, qualifiers[token.value]];

		if (suffixes?.includes(next.value)) {

			token.value = `${token.value} ${next.value}`;

			advance();

            return classify(upgradeWord(reference));

        } else if (suffixes?.includes(true) && openingDelimiterTypes.includes(next.type)) {

            token.type = `${token.value}-${next.type}`;
            token.value = `${token.value} ${next.value}`;

            advance();

            return reference;

		} else return upgradeWord(reference);
	}

	// prime the local variables, before checking and yielding each token in
	// the token stream (concatenating qualifiers along the way)...

	const tokens = tokenize(source, literate, script); let token, next;

	advance();

    while (advance()) yield classify(token);
}
