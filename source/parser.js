import tokenize from "./tokenizer.js"
import qualify from "./qualifier.js"
import objectify from "./objectifier.js"
import { iife, not, put } from "./helpers.js"

function * parse(source, literate=false, script=false) {

    // the following functions are used internally to implement the api that is
    // exposed to the parsing methods in `objectify.js`...

    function gather(mass=0) {

		/* This function implements the Pratt parsing function. */

		let current, left;

		current = token;
		token = advance();
		left = current.prefix(api);

		while (mass < token.mass) {

	        current = token;
	        token = advance();
	        left = current.infix(left, api);
		}

	    return left;
	}

	function * gatherStatements(nested=true) {

		/* This is the (often) recursive, block-level parsing function that wraps
		the Pratt parser to implement a statement grammar with nested blocks. */

		if (nested && not(on("open-brace-delimiter"))) {

			yield gatherFormalStatement();

		} else while (advance()) {

			if (onEOF() && nested) throw new SyntaxError("end of file inside block");

			if (onEOF()) break;

			if (on("close-brace-delimiter") && nested) { advance(); return }

			if (on("terminator")) continue;

			yield gatherStatement();

			if (on("terminator")) continue;

			if (on("close-brace-delimiter") && nested) { advance(); return }

			throw new SyntaxError("required terminator was not found");
		}
	}

    function onEOF() { return on("terminator") && token.value === "<EOF>" }

    function isKeyword(candidate) { return candidate.type === "key-word" }

    function gatherDeadspace() { while (on("terminator") && at("terminator")) advance() }

    // the following functions define the api that is passed to prefix and infix methods...

    function on(...types) { return types.includes(token.type) }

    function at(...types) { return types.includes(next.type) }

    function advance() { return ([token, next] = [next, tokens.next().value])[0] }

    function gatherToken() {

        /* This function gathers a single token, advances the parser, then
        returns the token (get `current`, then `advance`). */

        const current = token;

        advance();

        return current;
    }

	function gatherExpression() {

		/* This wraps the Pratt parsing function to ensure that the result
		is an expression (not a formal statement). */

		const candidate = gather();

		if (not(isKeyword(candidate))) return candidate;
		else throw new SyntaxError("required an expression");
	}

	function gatherStatement() {

		/* This wraps the Pratt parsing function to directlyany statement, formal or informal. */

		return gather();
	}

	function gatherFormalStatement() {

		/* This wraps the Pratt parsing function to ensure that the result
		is a formal statement (not an expression). */

        const candidate = gather();

		if (isKeyword(candidate)) return candidate;
		else throw new SyntaxError("required a formal statement");
	}

    function gatherGivenClause(...clauses) {

        /* This function takes any number of strings that specify the keywords for
        one or more clauses that can follow the block that was already parsed by
        the caller. If a given clause is found, `gatherBlock` is used to parse
        and return it, else the function returns `null`.

        Note: A clause can follow its preceeding statement on the same line, if
        the preceeding statement uses a braced block. */

        gatherDeadspace();

        if (on("terminator") && at("key-word") && clauses.includes(next.value)) advance();
        else if (not(on("key-word") && clauses.includes(token.value))) return null;

        return gatherBlock();
    }

    function gatherBlock() {

        /* This wraps `gatherStatements` to convert the generator to an array,
        as none of the API methods are generators. */

        return [...gatherStatements(true)];
    }

	const api = {
        on, at,
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
if x.not > y.else { 1 }





else if foo { 2 }
else { 3 }
else { 4 }
`;

//for (const lexeme of tokenize(source)) put("lexeme:", lexeme);
//for (const token of qualify(source, false)) put("token:", token);
//for (const object of objectify(source)) put("object:", object);
for (const node of parse(source)) console.log(node);
