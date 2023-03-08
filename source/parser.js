import {
    CloseBrace,
    EOF,
    Keyword,
    OpenBrace,
    Operator,
    Terminator,
    Word,
} from "./objects.js"

import lex from "./lexer.js"

export default function * (source, literate=false) {

    /* This generator implements the parser stage, and yields an AST node for each
    top-level statement. Like the lexer stage, the specifics of parsing any given
    grammar are left to the corresponding token object in `objects.js`.

    The parser API object collects references to a subset of the functions defined
    below, and every token has `prefix` and `infix` methods that take a reference
    to the parser API object. The `infix` methods also take a reference to the
    lefthand production as a second argument.

    The `source` string and `literate` flag are passed along to the lexer stage. */

    function gather(RBP=0) {

		/* This function implements Pratt's Algorithm. */

		let current, result;

		current = token;
		token = advance();
		result = current.prefix(api);

		while (RBP < token.LBP) {

	        current = token;
	        token = advance();
	        result = current.infix(api, result);
		}

	    return result;
	}

	function * gatherStatements(nested=true) {

		/* This is the (often) recursive, block-level parsing function that wraps
		the Pratt parser to implement a statement grammar that replaces ASI with
        LIST (Linewise Implicit Statement Termiation). */

		while (advance()) {

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

    function advance() { // api function

        /* Advance the token stream by one token, then return a reference to the
        newly current token. */

        return ([token, next] = [next, tokens.next().value])[0];
    }

    function on(type) { // api function

        /* Take a `Token` subclass, and return `true` if the current token is an
        instance of that class (or any subclass), else `false`. */

        return token instanceof type;
    }

    function at(type) { // api function

        /* Take a `Token` subclass, and return `true` if the *next token* is an
        instance of that class (or any subclass), else `false`. */

        return next instanceof type;
    }

    function gatherProperty() { // api function

        /* This function gathers a single property token, which can be a variable
        name or any kind of word (keyword, operator name, reserved word etc). If
        the token is a property, the function advances the parser, then returns
        the token (which is noted before advancing), complaining otherwise. */

        if (on(Word) || on(Operator) && token.named) {

            const current = token;

            advance();

            return current;

        } else throw new SyntaxError("dot without word");
    }

	function gatherExpression() { // api function

		/* This wraps the Pratt parsing function to ensure that the result is an
        expression (not a formal statement). */

		const candidate = gather();

		if (candidate.expression) return candidate;
		else throw new SyntaxError("required an expression");
	}

	function gatherStatement() { // api function

		/* This wraps the Pratt parsing function to make it part of the API. It is
        used to gather any statement, formal or informal. */

		return gather();
	}

    function check(stopcheck, onmatch, fallback=false) {

        /* This function allows statements (like `return` and `break`) that can only
        appear in specific contexts to establish whether they are in a valid context.

        The process walks the `stack` of integer block types backwards (upwards), and
        calls the `stopcheck` callback on each type to establish whether to stop yet.
        If the loop stops, the `onmatch` callback is passed the type that was stopped
        on, and the result is returned. The `fallback` bool is returned if the stack
        is exhausted without a match.

        The block types are enumerated as follows:

            + loop blocks = -1
            + simple blocks = 0
            + function blocks = 1
            + generator blocks = 2
            + async generator blocks = 3
            + async function blocks = 4
            + class blocks = 5

        It is always possible to establish the validity of a statement by walking to
        the last related statement, then checking if it is in a given range, falling
        back to a bool if no related statement was found. */

        for (let i = stack.length - 1; i >= 0; i--) {

            if (stopcheck(stack[i])) return onmatch(stack[i]);
        }

        return fallback;
    }

	function gatherFormalStatement() {

		/* This wraps the Pratt parsing function to ensure that the result is a
        formal statement (not an expression). */

        const candidate = gather();

		if (candidate instanceof Keyword) return candidate;
		else throw new SyntaxError("required a formal statement");
	}

    function gatherBlock(type) { // api function

        function validate(statement) {

            if (statement.validate(api)) return statement;
            else throw new SyntaxError("invalid statement in context");
        }

        let result;

        stack.push(type);

        if (on(OpenBrace)) result = [...gatherStatements()].map(validate);
        else result = validate(gatherFormalStatement());

        stack.pop();

        return result;
    }

	const api = {
        advance,
        at,
        check,
		gatherBlock,
		gatherExpression,
        gatherProperty,
		gatherStatement,
        on,
	};

    const [stack, tokens] = [[2], lex(source, literate)];

	let token, next;

    advance();

    yield * gatherStatements(false);
}
