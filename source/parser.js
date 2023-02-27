import { comma, propertyTypes } from "./strings.js"
import tokenize from "./tokenizer.js"
import qualify from "./qualifier.js"
import objectify from "./objectifier.js"
import { iife, not, put } from "./helpers.js"

function * parse(source, literate=false, script=false) {

	function advance() { return ([token, next] = [next, tokens.next().value])[0] }

	function onComma() { return token.type === "terminator" && token.value === comma }

	function onCloseBracket() { return token.type === "close-bracket-delimiter" }

	function onCloseBrace() { return token.type === "close-brace-delimiter" }

	function onCloseParen() { return token.type === "close-paren-delimiter" }

	function onTerminator() { return token.type === "terminator" }

    function onProperty() { return propertyTypes.includes(token.type) }

	function onEOF() { return onTerminator() && token.value === "<EOF>" }

	function gather(mass) {

		/* This is the textbook Pratt parsing function. */

		let current, left;

		current = token;
		token = advance();
		left = current.prefix(context);

		while (mass < token.mass) {

	        current = token;
	        token = advance();
	        left = current.infix(left, context);
		}

	    return left;
	}

    function gatherToken() {

        /* This function gathers a single token (without calling `gether`),
        then advances the parser, and returns the token that was gathered. */

        const current = token;

        advance();

        return current;
    }

	function gatherExpression(mass=0) {

		/* This wraps the Pratt parsing function to ensure that the result
		is an expression, and not a formal statement. */

		const candidate = gather(mass);

		if (candidate.type !== "key-word") return candidate;
		else throw new SyntaxError("required an expression");
	}

	function gatherStatement(mass=0) {

		/* This wraps the Pratt parsing function to provide the API with a
		function that explicitly accepts any statement, formal or informal. */

		return gather(mass);
	}

	function gatherFormalStatement(mass=0) {

		/* This wraps the Pratt parsing function to ensure that the result
		a formal statement, and not just an expression. */

        const candidate = gather(mass);

		if (candidate.type === "key-word") return candidate;
		else throw new SyntaxError("required a formal statement");
	}

	function * gatherBlock(nested) {

		/* This is the (often) recursive, block-level parsing function that wraps
		the Pratt parser to implement a statement grammar with nested blocks. */

		if (nested && token.type !== "open-brace-delimiter") {

			yield gatherFormalStatement();

		} else while (advance()) {

			if (onEOF() && nested) throw new SyntaxError("end of file inside block");

			if (onEOF()) break;

			if (onCloseBrace() && nested) { advance(); return }

			if (onTerminator()) continue;

			yield gatherStatement();

			if (onTerminator()) continue;

			if (onCloseBrace() && nested) { advance(); return }

			throw new SyntaxError("required terminator was not found");
		}
	}

	const context = { // this is the api that is passed to `prefix` and `infix` methods
		advance,
        gatherToken,
		gatherExpression,
		gatherStatement,
		gatherFormalStatement,
		gatherBlock,
		onCloseBracket,
		onCloseBrace,
		onCloseParen,
		onTerminator,
        onProperty,
		onComma,
	};

	const tokens = objectify(source, literate, script);
	let token, next; advance();
	yield * gatherBlock(false);
}

const source = `
1 --1
`;

// for (const lexeme of tokenize(source)) put("lexeme:", lexeme);
for (const token of qualify(source, false)) put("token:", token);
// for (const object of objectify(source)) put("object:", object);
for (const node of parse(source)) console.log(node);
