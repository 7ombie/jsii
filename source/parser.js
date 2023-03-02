import { comma, propertyTypes } from "./strings.js"
import tokenize from "./tokenizer.js"
import qualify from "./qualifier.js"
import objectify from "./objectifier.js"
import { iife, not, put } from "./helpers.js"

function * parse(source, literate=false, script=false) {

    function gather(mass) { // internal use only

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

	function * gatherStatements(nested) { // internal use only

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

    // the following functions define the api that is passed to prefix and infix methods...

	function advance() { return ([token, next] = [next, tokens.next().value])[0] }

	function onEOF() { return onTerminator() && token.value === "<EOF>" }

    function onTerminator() { return token.type === "terminator" }

    function onComma() { return token.type === "comma-delimiter" }

    function onProperty() { return propertyTypes.includes(token.type) }

    function onCloseParen() { return token.type === "close-paren-delimiter" }

	function onCloseBrace() { return token.type === "close-brace-delimiter" }

	function onCloseBracket() { return token.type === "close-bracket-delimiter" }

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

	function gatherStatement() {

		/* This wraps the Pratt parsing function to provide the API with a
		function that explicitly accepts any statement, formal or informal. */

		return gather(0);
	}

	function gatherFormalStatement() {

		/* This wraps the Pratt parsing function to ensure that the result
		a formal statement, and not just an expression. */

        const candidate = gather(0);

		if (candidate.type === "key-word") return candidate;
		else throw new SyntaxError("required a formal statement");
	}

    function gatherGivenClause(clauses) {

        /* This wraps `gatherStatements` to convert the generator to an array,
        as none of the API methods are generators. */

        if (onTerminator() && next.type === "key-word" && clauses.includes(next.value)) advance();
        else if (not(token.type === "key-word" && clauses.includes(token.value))) return null;

        return gatherBlock();
    }

    function gatherBlock() {

        /* This wraps `gatherStatements` to convert the generator to an array,
        as none of the API methods are generators. */

        return [...gatherStatements(true)];
    }

	const context = { // this is the api that is passed to `prefix` and `infix` methods

        onComma,
		onTerminator,
        onProperty,
        onCloseParen,
		onCloseBracket,
		onCloseBrace,

        advance,
        gatherToken,
		gatherExpression,
		gatherStatement,
		gatherFormalStatement,
        gatherGivenClause,
		gatherBlock,
	};

	const tokens = objectify(source, literate, script);
	let token, next; advance();
	yield * gatherStatements(false);
}

const source = `
if x > y { 1 }
else if foo { 2 }
else { 3 }
else { 4 }
`;

//for (const lexeme of tokenize(source)) put("lexeme:", lexeme);
//for (const token of qualify(source, false)) put("token:", token);
//for (const object of objectify(source)) put("object:", object);
for (const node of parse(source)) console.log(node);
