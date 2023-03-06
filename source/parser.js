import { OpenBrace, CloseBrace, Keyword, Terminator, EOF } from "./objects.js"
import tokenize from "./tokenizer.js"

export default function * parse(source, literate=false) {

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
	        left = current.infix(api, left);
		}

	    return left;
	}

	function * gatherStatements(nested=true) {

		/* This is the (often) recursive, block-level parsing function that wraps
		the Pratt parser to implement a statement grammar with nested blocks. */

		if (nested && (!on(OpenBrace))) {

			yield gatherFormalStatement();

		} else while (advance()) {

			if (on(EOF) && nested) throw new SyntaxError("end of file inside block");

			if (on(EOF)) break;

			if (on(CloseBrace) && nested) { advance(); return }

			if (on(Terminator)) continue;

			yield gatherStatement();

			if (on(Terminator)) continue;

			if (on(CloseBrace) && nested) { advance(); return }

			throw new SyntaxError("required terminator was not found");
		}
	}

    // the following functions define the api that is passed to prefix and infix methods...

    function on(type) { return token instanceof type }

    function at(type) { return next instanceof type }

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

		if (!(candidate instanceof Keyword)) return candidate;
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

		if (candidate instanceof Keyword) return candidate;
		else throw new SyntaxError("required a formal statement");
	}

    function gatherGivenClause(...clauses) {

        /* This function takes any number of strings that specify the keywords for
        one or more clauses that can follow the block that was already parsed by
        the caller. If a given clause is found, `gatherBlock` is used to parse
        and return it, else the function returns `null`.

        Note: A clause can follow its preceeding statement on the same line, if
        the preceeding statement uses a braced block. */

        if (on(Terminator) && at(Keyword) && clauses.includes(next.value)) advance();
        else if (!(on(Keyword) && clauses.includes(token.value))) return null;

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

	const tokens = tokenize(source, literate);

	let token, next;

    advance();

    yield * gatherStatements(false);
}
