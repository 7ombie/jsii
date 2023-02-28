import { empty, space, masses, delimiters } from "./strings.js"
import { prefixOperators, infixOperators, dotOperators } from "./strings.js"
import { iife, not, put } from "./helpers.js"
import qualify from "./qualifier.js"

class ASTNode {

	constructor(token) {

		this.type = token.type;
		this.value = token.value;
		this.location = token.location;
		this.mass = masses.infix[token.value] ?? 0;
		this.pull = masses.prefix[token.value] ?? this.mass;
	}

	prefix() { throw new SyntaxError("unexpected token in prefix position") }

	infix() { throw new SyntaxError("unexpected token in infix position") }
}

class Delimiter extends ASTNode {}
class Terminator extends ASTNode {}

class Terminal extends ASTNode {

	prefix(context) { return this }
}

class Comment extends Terminal {}
class Constant extends Terminal {}
class Identifier extends Terminal {}
class NumberLiteral extends Terminal {}
class StringLiteral extends Terminal {}

class PassLike extends Terminal {

	static keywords = ["pass", "debug"];
}

class BreakLike extends ASTNode {

	static keywords = ["break", "continue"];

	prefix(context) {

		if (context.onTerminator()) { this.label = null; return this }

        this.label = context.gatherExpression();

		if (this.label.type === "variable-name") return this;
		else throw new SyntaxError("labels must be plain identifiers");
	}
}

class ReturnLike extends ASTNode {

	static keywords = ["return", "yield", "yield from"];

	prefix(context) {

		if (context.onTerminator()) this.right = null;
		else this.right = context.gatherExpression(0);

		return this;
	}
}

class Block extends ASTNode {

	static keywords = ["do", "else"];

	prefix(context) {

		this.statements = [...context.gatherBlock(true)];

		return this;
	}
}

class PredicatedBlock extends ASTNode {

	static keywords = ["if", "else if", "while", "until", "unless"];

	prefix(context) {

		this.predicate = context.gatherExpression();
		this.statements = [...context.gatherBlock(true)];

		return this;
	}
}

class CompoundLiteral extends ASTNode {

	/* This abstract base class models array, object and expression literals. */

	constructor(token) {

		super(token);

		const qualified = this.value.length !== 1;

		this.items = new Array();
		this.qualifier = qualified ? this.value.slice(0, -1).trim() : empty;
	}
}

class SequentialLiteral extends CompoundLiteral {

	/* This abstract base class extends the class for compund literals to
	implement the specifics of parsing comma-separated expressions (array
	literals and parenthesized expressions etc). */

	prefix(context, check) {

		while (not(check())) {

			if (context.onComma()) {

				this.items.push(null);
				context.advance();
				continue;
			}

			this.items.push(context.gatherExpression());

			if (check()) break;
			else if (context.onComma()) context.advance();
			else throw new SyntaxError("required comma not found");
		}

		context.advance(); // drop closing paren or bracket that `check` already validated

        return this;
	}
}

class ExpressionLiteral extends SequentialLiteral {

	prefix(context) { return super.prefix(context, context.onCloseParen) }
}

class ArrayLiteral extends SequentialLiteral {

	prefix(context) { return super.prefix(context, context.onCloseBracket) }
}


class PrefixOperation extends ASTNode {

	static operators = prefixOperators;

	prefix(context) {

		[this.left, this.right] = [null, context.gatherExpression(this.pull)];

		return this;
	}
}

class InfixOperation extends ASTNode {

	static operators = infixOperators;

	infix(left, context) {

		[this.left, this.right] = [left, context.gatherExpression(this.mass)];

		return this;
	}
}

class OmnifixOperation extends ASTNode {

	static operators = prefixOperators.filter(operator => infixOperators.includes(operator));

	prefix(context) {

		[this.left, this.right] = [null, context.gatherExpression(this.pull)];

		return this;
	}

	infix(left, context) {

		[this.left, this.right] = [left, context.gatherExpression(this.mass)];

		return this;
	}
}

class ExponentiationOperation extends ASTNode {

	infix(left, context) {

		[this.left, this.right] = [left, context.gatherExpression(this.mass - 1)];

		return this;
	}
}

class DotOperation extends ASTNode {

	infix(left, context) {

        if (context.onProperty()) {

            this.left = left;
            this.right = context.gatherToken();
            this.right.type = "property-name";

            return this;
        }

        throw new SyntaxError("property access requires a property name");
	}
}

export default function * classify(source, literate=false, script=false) {

	for (const token of qualify(source, literate, script)) {

		const { type, value } = token;

		if (type === "variable-name") yield new Identifier(token);
		else if (type === "constant-word") yield new Constant(token);
		else if (type === "string-literal") yield new StringLiteral(token);
		else if (type.endsWith("-number-literal")) yield new NumberLiteral(token);
		else if (type === "comment") yield new Comment(token);
		else if (type.endsWith("-delimiter")) {

			if (type.includes("open-bracket")) yield new ArrayLiteral(token);
			// else if (type.includes("open-brace")) yield new ObjectLiteral(token); 		// TODO: implement
 			else if (type.includes("open-paren")) yield new ExpressionLiteral(token);
			else yield new Delimiter(token);

		} else if (type.includes("operator")) {

			if (value === "**") yield new ExponentiationOperation(token);
			else if (dotOperators.includes(value)) yield new DotOperation(token);
			else if (OmnifixOperation.operators.includes(value)) yield new OmnifixOperation(token);
			else if (PrefixOperation.operators.includes(value)) yield new PrefixOperation(token);
			else if (InfixOperation.operators.includes(value)) yield new InfixOperation(token);
			else throw SyntaxError("unimplemented operator");

		} else if (type === "key-word") {

            if (Block.keywords.includes(value)) yield new Block(token);
			else if (PredicatedBlock.keywords.includes(value)) yield new PredicatedBlock(token);
			else if (ReturnLike.keywords.includes(value)) yield new ReturnLike(token);
            else if (BreakLike.keywords.includes(value)) yield new BreakLike(token);
            else if (PassLike.keywords.includes(value)) yield new PassLike(token);
            else throw SyntaxError("unimplemented keyword");

		} else yield new Terminator(token);
	}
}
