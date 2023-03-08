import {
    bases,
    closeBrace,
    closeBracket,
    closeParen,
    colon,
    comma,
    constants,
    digits,
    dollar,
    dot,
    empty,
    keywords,
    newline,
    openBrace,
    openBracket,
    openParen,
    operators,
    quote,
    reserved,
    semicolon,
    space,
    symbolics,
    wordCharacters,
} from "./strings.js"

const LOOPBLOCK = -1;
const SIMPLEBLOCK = 0;
const FUNCTIONBLOCK = 1;
const GENERATORBLOCK = 2;
const ASYNCGENERATORBLOCK = 3;
const ASYNCFUNCTIONBLOCK = 4;
const CLASSBLOCK = 5;

class Token {

    /* This is the internal, abstract base class for all other token classes. */

    LBP = 0;
    expression = false;

	constructor(location, value=empty) {

        this.location = location;
        this.value = value;
    }

	prefix() { console.log(this); throw new SyntaxError(`invalid prefix`) }

	infix() { console.log(this); throw new SyntaxError(`invalid infix`) }

    validate() { return true }
}

class Terminal extends Token {

    /* This is the abstract base class used by all terminal tokens. */

	prefix(_) { return this }
}

export class Terminator extends Terminal {

    /* This is the abstract base class for the various statement terminators. It is also
    used by the lexer to tokenize terminators. */

    static * lex(lexer, location) {

        if (lexer.on(newline)) yield new LineFeed(location, "<LF>");
        else if (lexer.on(semicolon)) yield new Semicolon(location, semicolon);
        else yield new EOF(location, "<EOF>");
    }
}

export class NumberLiteral extends Terminal {

    /* This is the concrete class for all number literals. It is also used by the
    lexer for number tokenization. */

    expression = true;

    static * lex(...args) { yield new NumberLiteral(...args) }

    constructor(lexer, location) {

        let type;

        super(location, lexer.read());

        if (lexer.on("0") && lexer.at(bases)) {

            this.value += lexer.advance();
            type = lexer.on("xX") ? "hexadecimal" : "binary";

        } else type = "decimal";

        lexer.gatherWhile(digits[type], this);

        if (type !== "decimal" && this.value.length === 2) {

            throw new SyntaxError(`${type} prefix without digits`);
        }

        if (type === "decimal" && lexer.at(dot) && lexer.peek(+2, digits.decimal)) {

            this.value += lexer.advance();
            lexer.gatherWhile(digits[type], this);
        }
    }
}

export class StringLiteral extends Terminal {

    /* This is the concrete class for all string literals. It is also used by the lexer
    for (recursively) tokenizing strings and their interpolations. */

    expression = true;

    static * lex(...args) { yield new StringLiteral(...args) }

    constructor(lexer, location) {

        super(location, [empty]);

        while ((!lexer.at(quote)) && lexer.advance()) {

            if (lexer.on(dollar) && lexer.at(openBrace)) {

                lexer.advance();
                lexer.interpolate(true);

                this.value.push([...lexer.gatherStream()]);
                this.value.push(empty);

                lexer.interpolate(false);

            } else this.value[this.value.length - 1] += lexer.read();
        }

        lexer.advance();
    }
}

export class Delimiter extends Terminal {

    /* This is the abstract base class for all delimiters, and the class used by the lexer
    for tokenizing delimiters. */

    static * lex(lexer, location) {

        /* Yield a single delimiter token and exit. */

        const value = lexer.read();

        switch (value) {
            case colon: yield new Colon(location, value); break;
            case comma: yield new Comma(location, value); break;
            case openParen: yield new OpenParen(location, value); break;
            case openBracket: yield new OpenBracket(location, value); break;
            case openBrace: yield new OpenBrace(location, value); break;
            case closeParen: yield new CloseParen(location, value); break;
            case closeBracket: yield new CloseBracket(location, value); break;
            case closeBrace: yield new CloseBrace(location, value); break;
        }
    }
}

class SuffixDelimiter extends Delimiter {

    LBP = 17;
    expression = true;
}

export class Word extends Terminal {

    /* This is the abstract base class for every type of word and name. It is also used
    by the lexer tokenization. */

    static * lex(lexer, location) {

        let value = lexer.read();

        while (lexer.at(wordCharacters)) value += lexer.advance();

        if (keywords.includes(value)) yield Keyword.subclass(location, value);
        else if (operators.includes(value)) yield Operator.subclass(location, value);
        else if (constants.includes(value)) yield new Constant(location, value);
        else if (reserved.includes(value)) yield new Reserved(location, value);
        else yield new Variable(location, value);
    }

    write(writer) { writer.push(this.value) }
}

export class Keyword extends Word {

    /* This abstract base class is used by all of the keyword classes. It is also used by
    the `Word` class for subclassing keywords. */

    static subclass(location, value) {

        switch (value) {

            case "return": return new Return(location, value);
            case "yield": return new Yield(location, value);
            case "from": return new From(location, value);

            case "do": return new Do(location, value);
            case "async": return new Async(location, value);
            case "await": return new Await(location, value);

            case "debug": return new Debug(location, value);
            case "pass": return new Pass(location, value);

            case "let": return new Let(location, value);
            case "var": return new Var(location, value);

            case "break": return new Break(location, value);
            case "continue": return new Continue(location, value);

            case "if": return new If(location, value);
            case "else": return new Else(location, value);

            case "unless": return new Unless(location, value);
            case "while": return new While(location, value);
            case "until": return new Until(location, value);

            case "for": return new For(location, value);

            case "lambda": return new LambdaStatement(location, value);
            case "function": return new FunctionStatement(location, value);
            case "generator": return new GeneratorStatement(location, value);
        }
    }
}

class Statement extends Keyword {}

class OptionalExpressionStatement extends Keyword {

    /* This is the abstract base class for statements that are follow their keyword with
    an optional expression (`return` and `yield`). */

    prefix(parser) {

        if (parser.on(Terminator)) this.expression = null;
        else this.expression = parser.gatherExpression();

        return this;
    }
}

class ExitStatement extends Keyword {}

class PredicatedBlock extends Keyword {

    /* This is the abstract base class for the predicated blocks, `while`, `unless` and
    `until`. It is not used by `if` or `else if`, as they are special-cases with their
    own logic. */
}

class FunctionalBlock extends Keyword {

    /* This is the abstract base class for functional blocks, including the `lambda`,
    `function` and `generator` blocks, but not including the arrow grammars. This applies
    to function statements/expressions, whether they are asynchronous, IIFEs, neither or
    both. */

    expression = true;
}

export class Operator extends Token {

    /* This is the abstract base class for all of the operator subclasses. It is used by the
    lexer to gather an unbroken sequence of our symbolic operator characters, split it into
    individual operators (see `slice`), then yield the operators as concrete tokens, one
    at a time. */

    RBP = 0;
    named = false;
    expression = true;

    static slice(value, offset=1) {

        /* Slice an unbroken sequence of symbolic operator characters into an array of
        individual operators, greedily, from left to right. */

        if (!value) return [];

        if (operators.includes(value)) return [value];

        if (offset > value.length) throw new SyntaxError(`invalid operator (${value})`);

        const [start, end] = [value.slice(0, -offset), value.slice(-offset)];

        if (operators.includes(start)) return [start, ...Operator.slice(end)];
        else return Operator.slice(value, offset + 1);
    }

    static subclass(location, value) {

        /* Take a value and location, and use them to instantiate and return the appropriate
        operator instance. */

        switch (value) {
            case "+": return new Plus(location, value);
            case "-": return new Minus(location, value);
            case "*": return new Star(location, value);
            case "/": return new Slash(location, value);
            case ".": return new Dot(location, value);
            case "!": return new Bang(location, value);
            case "?": return new Ask(location, value);
            case "<": return new Lesser(location, value);
            case ">": return new Greater(location, value);
            case "<=": return new NotGreater(location, value);
            case ">=": return new NotLesser(location, value);
            case "//": return new Floor(location, value);
            case "**": return new Raise(location, value);
            case "->": return new SkinnyArrow(location, value);
            case "=>": return new FatArrow(location, value);
            case "??": return new Nullish(location, value);
            case "is": return new Is(location, value);
            case "in": return new In(location, value);
            case "not": return new Not(location, value);
        }
    }

    static * lex(lexer, location) {

        /* This API method yields as many operators as it can gather. */

        let values = lexer.read();

        while (lexer.at(symbolics)) values += lexer.advance();

        for (const value of Operator.slice(values)) {

            yield Operator.subclass(location, value);

            location += value.length;
        }
    }

    write(writer) {

        /* This provides a generic writer method for prefix and infix operators, (prefix
        operators have null `left` attributes) */

        const separator = this.left ? space : empty;

        this.left?.write(writer);
        writer.push(separator, this.value, separator);
        this.right.write(writer);
    }
}

class InfixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as infix
    operators. */

	infix(parser, left) {

		this.left = left;
        this.right = parser.gatherExpression(this.LBP);

		return this;
	}
}

class DotOperator extends Operator {

    /* This base class provides functionality for dot-operators, which must have a property
    name (which can be any type of word token) on the rightside. */

    LBP = 17;

    infix(parser, left) {

		this.left = left;
        this.right = parser.gatherProperty();

        return this;
	}
}

class PrefixDotOperator extends DotOperator {

    /* This base class extends the dot-operator class with functionality specific to the bang
    and ask operators, which are also bitwise prefix operators. */

    prefix(parser) {

        this.left = null;
        this.right = parser.gatherExpression(this.RBP);

        return this;
    }
}

class RightAssociativeOperator extends Operator {

    /* This is the base class for right-associative infix operators (this is only currently
    used for exponentiation). */

    infix(parser, left) {

		this.left = left;
        this.right = parser.gatherExpression(this.LBP - 1);

		return this;
	}
}

class GeneralOperator extends Operator {

    /* This is the base class for operators that have prefix and infix forms. */

    prefix(parser) {

        this.left = null;
		this.right = parser.gatherExpression(this.RBP);

        return this;
	}

	infix(parser, left) {

		this.left = left;
        this.right = parser.gatherExpression(this.LBP);

		return this;
	}
}

/// These are the concrete token classes that actually appear in token streams...

class Ask extends PrefixDotOperator {

    /* Implements the `?` operator, which compiles to a `Math.clz32` invocation in a prefix
    context, and `?.` in an infix context. */

    LBP = 17;
    RBP = 14;
}

class Async extends Keyword {

    /* Implements the `async` qualifier, used to prefix the `lambda`, `function` and
    `generator` keywords to define async versions. */

    expression = true;

    prefix(parser) {

        if (parser.on(FunctionalBlock)) this.statements = parser.gatherStatement();
        else throw new SyntaxError("async qualifier without valid subject");

        return this;
    }
}

class Await extends Keyword {

    /* Implements the `await` prefix operator, used to await promises, generally and
    in for-loops. */

    expression = true;

    static blocks = [ASYNCGENERATORBLOCK, ASYNCFUNCTIONBLOCK];

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is an asynchronous function block, and `false` otherwise. If nothing functional
        is found, the validation *succeeds* (note the third argument), as top-level-await
        is permitted (modular source is assumed, but not required). */

        return parser.check(t => t > SIMPLEBLOCK, t => Await.blocks.includes(t), true);
    }
}

class Bang extends PrefixDotOperator {

    /* Implements the `!` operator, which compiles to the bitwise `~` operator in a prefix
    context, and `.#` in an infix context. */

    LBP = 17;
    RBP = 14;
}

class Break extends ExitStatement {

    /* Implements the `break` statement, just like JavaScript. */
}

export class CloseBrace extends Delimiter {

    /* Implements the `}` delimiter, used for closing blocks, object expressions and
    destructured assignees. */
}

class CloseBracket extends Delimiter {

    /* Implements the `]` delimiter, used for closing array expressions and destructured
    assignees. */
}

class CloseParen extends Delimiter {

    /* Implements the `)` delimiter, used for closing group expressions, arrow params and
    function invocations. */
}

class Colon extends Delimiter {

    /* Implements the `:` delimiter, used for delimiting key values from their assigned values
    in object expressions. */
}

class Comma extends Delimiter {

    /* Implements the `,` delimiter, used for delimiting expressions in groups, invocations and
    compound literals, as well as params, declarations etc. */
}

class Constant extends Word {

    /* Implements constant words, used for named numbers (`Infinity` and `NaN`), as well as
    magic variables (`this`, `super`, `arguments`, `random`, `null`, `void`, `true`, `false`
    and `global`). */

    expression = true;
}

class Continue extends ExitStatement {

    /* Implements the `continue` statement, just like JavaScript. */
}

class Debug extends Statement {

    /* Implements the `debug` statement, which compiles to `debugger`. */
}

class Do extends Keyword {

    /* Implements the `do` keyword, which can prefix a block to create a block statement,
    or prefix `async`, `lambda`, `function` or `generator` to create an IIFE. */

    prefix(parser) {

        if (parser.on(Async) || parser.on(FunctionalBlock)) {

            this.statements = parser.gatherStatement();

        } else this.statements = parser.gatherBlock(SIMPLEBLOCK);

        return this;
    }
}

class Dot extends DotOperator {}

export class EOF extends Terminator {}

class FatArrow extends GeneralOperator {

    LBP = 2;
    RBP = 2;
}

class Floor extends InfixOperator {

    LBP = 12;
}

class For extends Keyword {}

class From extends Keyword {}

class FunctionStatement extends FunctionalBlock {}

class GeneratorStatement extends FunctionalBlock {}

class Greater extends InfixOperator {

    LBP = 9;
}

class If extends Keyword {

    /* This concrete token class is used by if-blocks and extended by else-blocks, which both
    have a `clause` attribute that (recursively) refers to the following else clause, or `null`
    when no following else-clause is present. */

    prefix(parser) {

        this.predicate = parser.gatherExpression();
        this.statements = parser.gatherBlock(SIMPLEBLOCK);
        this.clause = this.gatherOptionalElseClause(parser);

        return this;
    }

    write(writer) {

        writer.openPredicatedBlock("if", this.predicate);
        writer.indentStatements(this.statements);
        writer.closeBlock();

        this.clause?.write(writer);
    }

    gatherOptionalElseClause(parser) {

        /* This helper (which is also used by `Else`) takes a reference to the parser API and
        attempts to gather an else-clause (accounting for terminators being optional before
        clauses). It returns the else-clause when successful, and `null` otherwise. */

        if (parser.on(Else)) return parser.gatherBlock(SIMPLEBLOCK);

        if (parser.on(Terminator) && parser.at(Else)) {

            parser.advance();
            return parser.gatherBlock(SIMPLEBLOCK);
        }

        return null;
    }
}

class Else extends If {

    /* This concrete token class extends the `If` class to handle `else` and `else if` clauses,
    recuring in the later case (using `super.prefix`). In the former case, the `predicate` and
    `clause` attributes are set to `null` before gathering a block. */

    prefix(parser) {

        if (parser.on(If)) { parser.advance(); return super.prefix(parser) }

        this.clause = null;
        this.predicate = null;
        this.statements = parser.gatherBlock(SIMPLEBLOCK);

        return this;
    }

    write(writer) {

        if (this.predicate) {

            writer.openPredicatedBlock("else if", this.predicate);
            writer.indentStatements(this.statements);
            writer.closeBlock();

            this.clause?.write(writer);

        } else {

            writer.openUnconditionalBlock("else");
            writer.indentStatements(this.statements);
            writer.closeBlock();
        }
    }
}

class LambdaStatement extends FunctionalBlock {

    prefix(parser) {

        this.statements = parser.gatherBlock(FUNCTIONBLOCK);

        return this;
    }
}

class In extends InfixOperator {

    LBP = 17;
    named = true;
}

class Is extends InfixOperator {

    LBP = 8;
    named = true;
}

class Lesser extends InfixOperator {

    LBP = 9;
}

class Let extends Keyword {}

class LineFeed extends Terminator {}

class Minus extends GeneralOperator {

    LBP = 14;
    RBP = 11;
}

class Not extends GeneralOperator {

    LBP = 9;
    RBP = 14;
    named = true;
}

class NotGreater extends InfixOperator {

    LBP = 9;
}

class NotLesser extends InfixOperator {

    LBP = 9;
}

class Nullish extends InfixOperator {

    LBP = 3;
    RBP = 14;
}

export class OpenBrace extends Delimiter {}

class OpenBracket extends SuffixDelimiter {}

class OpenParen extends SuffixDelimiter {}

class Pass extends Statement {}

class Plus extends GeneralOperator {

    LBP = 14;
    RBP = 11;
}

class Raise extends RightAssociativeOperator {

    LBP = 13;
}

class Reserved extends Word {

    prefix(parser) { throw new SyntaxError("reserved word") }

    infix(parser) { throw new SyntaxError("reserved word") }
}

class Return extends OptionalExpressionStatement {

    validate(parser) {

        return parser.check(t => t > SIMPLEBLOCK, t => t < CLASSBLOCK);
    }
}

class Semicolon extends Terminator {}

class SkinnyArrow extends GeneralOperator {

    LBP = 2;
    RBP = 2;
}

class Slash extends InfixOperator {

    LBP = 12;
}

class Star extends InfixOperator {

    LBP = 12;
}

class Unless extends PredicatedBlock {}

class Until extends PredicatedBlock {}

class Var extends Keyword {}

class Variable extends Word {

    expression = true;
}

class While extends PredicatedBlock {}

class Yield extends OptionalExpressionStatement {

    LBP = 2;
    expression = true;

    static blocks = [GENERATORBLOCK, ASYNCGENERATORBLOCK];

    validate(parser) {

        return parser.check(t => t > SIMPLEBLOCK, t => Yield.blocks.includes(t));
    }
}
