import {
    empty,
    newline,
    dot,
    comma,
    colon,
    semicolon,
    quote,
    dollar,
    openParen,
    openBracket,
    openBrace,
    closeParen,
    closeBracket,
    closeBrace,
    bases,
    digits,
    reserved,
    keywords,
    constants,
    symbolics,
    operators,
    wordCharacters,
} from "./strings.js"

class Token {

    /* This is the internal, abstract base class for all other token classes. */

    mass = 0;
    value = empty;

	constructor(location) { this.location = location }

	prefix() { console.log(this); throw new SyntaxError(`invalid prefix`) }

	infix() { console.log(this); throw new SyntaxError(`invalid infix`) }
}

class Terminal extends Token {

    /* This is the abstract base class used by all terminal tokens. */

	prefix(_) { return this }
}

export class Terminator extends Terminal {

    /* This is used as the concrete class for statement terminator tokens. */

    static * lex(lexer, location) {

        if (lexer.on(newline)) yield new LineFeed(location);
        else if (lexer.on(semicolon)) yield new Semicolon(location);
        else yield new EOF(location);
    }
}

export class Comment extends Terminator {

    /* This is the concrete class for line comments, which can terminate statements,
    just like a newline. */

    static * lex(...args) { yield new LineComment(...args) }

    constructor(lexer, location) {

        super(location);
        lexer.gatherUntil(newline, this);
    }
}

export class NumberLiteral extends Terminal {

    /* This is the concrete class for all number literals. It is also used by the
    lexer for number tokenization. */

    static * lex(...args) { yield new NumberLiteral(...args) }

    constructor(lexer, location) {

        let type;

        super(location);
        this.value = lexer.read();

        if (lexer.on("0") && lexer.at(bases)) {

            this.value += lexer.advance();
            type = lexer.on("xX") ? "hexadecimal" : "binary";

        } else type = "decimal";

        lexer.gatherWhile(digits[type], this);

        if (type !== "decimal" && this.value.length === 2) {

            throw new SyntaxError(`${type} prefix without digits`);
        }

        if (lexer.at(dot) && type === "decimal") {

            this.value += lexer.advance();
            lexer.gatherWhile(digits[type], this);

            if (lexer.on(dot)) throw new SyntaxError("implicit fraction");
        }

    }
}

export class StringLiteral extends Terminal {

    /* This is the concrete class for all string literals. It is also used by the
    lexer for (recursively) tokenizing strings and their interpolations. */

    static * lex(...args) { yield new StringLiteral(...args) }

    constructor(lexer, location) {

        super(location);
        this.value = [empty];

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

    /* This is the abstract base class for all delimiters, and the class used
    by the lexer for tokenizing delimiters. */

    static * lex(lexer, location) {

        /* Yield a single delimiter token and exit. */

        const value = lexer.read();

        switch (value) {
            case colon: yield new Colon(location); break;
            case comma: yield new Comma(location); break;
            case openParen: yield new OpenParen(location); break;
            case openBracket: yield new OpenBracket(location); break;
            case openBrace: yield new OpenBrace(location); break;
            case closeParen: yield new CloseParen(location); break;
            case closeBracket: yield new CloseBracket(location); break;
            case closeBrace: yield new CloseBrace(location); break;
        }
    }
}

class SuffixDelimiter extends Delimiter { mass = 17 }

export class Word extends Terminal {

    /* This is the abstract base class for every type of word and name. It is also
    used by the lexer tokenization. */

    constructor(value, location) {

        super(location);
        this.value = value;
    }

    static * lex(lexer, location) {

        let value = lexer.read();

        while (lexer.at(wordCharacters)) value += lexer.advance();

        if (keywords.includes(value)) yield Keyword.subclass(value, location);
        else if (constants.includes(value)) yield new Constant(value, location);
        else if (operators.includes(value)) yield new Operator(value, location);
        else if (reserved.includes(value)) yield new Reserved(value, location);
        else yield new Variable(value, location);
    }
}

export class Keyword extends Word {

    /* This abstract base class is used by all of the keyword classes. It is also
    used by the `Word` class for subclassing keywords. */

    static subclass(...args) {

        switch (args[0]) {

            case "return": return new Return(...args);
            case "yield": return new Yield(...args);
            case "from": return new From(...args);

            case "do": return new Do(...args);
            case "async": return new Async(...args);
            case "await": return new Await(...args);

            case "debug":
            case "pass": return new Command(...args);

            case "let": return new Let(...args);
            case "var": return new Var(...args);

            case "break":
            case "continue": return new Branch(...args);

            case "if": return new If(...args);
            case "else": return new Else(...args);

            case "unless":
            case "while":
            case "until": return new Predicated(...args);

            case "for": return new For(...args);

            case "lambda": return new Lambda(...args);

            case "function":
            case "generator": return new Functional(...args);
        }
    }
}

export class Operator extends Token {

    /* This is the abstract base class for all of the operator subclasses.
    It is used by the lexer to gather an unbroken sequence of our symbolic
    operator characters, split it into individual operators (see `slice`),
    then yield the operators as concrete tokens, one at a time. */

    pull = 0;

    static slice(value, offset=1) {

        /* Slice an unbroken sequence of symbolic operator characters into
        an array of individual operators, greedily, from left to right. */

        if (!value) return [];

        if (operators.includes(value)) return [value];

        if (offset > value.length) throw new SyntaxError(`invalid operator (${value})`);

        const [start, end] = [value.slice(0, -offset), value.slice(-offset)];

        if (operators.includes(start)) return [start, ...Operator.slice(end)];
        else return Operator.slice(value, offset + 1);
    }

    static subclass(value, location) {

        /* Take a value and location, and use them to instantiate and
        return the appropriate operator instance. */

        switch (value) {
            case "+": return new Plus(location);
            case "-": return new Minus(location);
            case "*": return new Star(location);
            case "/": return new Slash(location);
            case ".": return new Dot(location);
            case "!": return new Bang(location);
            case "?": return new Ask(location);
            case "<": return new Lesser(location);
            case ">": return new Greater(location);
            case "<=": return new NotGreater(location);
            case ">=": return new NotLesser(location);
            case "//": return new Floor(location);
            case "**": return new Raise(location);
            case "->": return new SkinnyArrow(location);
            case "=>": return new FatArrow(location);
            case "??": return new Nullish(location);
            case "is": return new Is(location);
            case "in": return new In(location);
            case "not": return new Not(location);
        }
    }

    static * lex(lexer, location) {

        /* This API method yields as many operators as it can gather. */

        let values = lexer.read();

        while (lexer.at(symbolics)) values += lexer.advance();

        for (const value of Operator.slice(values)) {

            yield Operator.subclass(value, location);

            location += value.length;
        }
    }
}

class PrefixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as
    prefix operators. */

    prefix(parser) {

		this.right = parser.gatherExpression(this.pull);

        return this;
	}
}

class InfixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as
    infix operators. */

	infix(parser, left) {

		this.left = left;
        this.right = parser.gatherExpression(this.mass);

		return this;
	}
}

class DotOperator extends Operator {

    /* This base class provides functionality for dot-operators, which must have
    a property name (which can be any type of word token) on the rightside. */

    infix(parser, left) {

        // TODO: this should gather a property (not an expression)

		this.left = left;
        this.right = parser.gatherExpression(this.mass);

		return this;
	}
}

class PrefixDotOperator extends Operator {

    /* This base class extends the dot-operator class with functionality specific
    to the bang and ask operators, which are also bitwise prefix operators. */

    // TODO: implement ask and bang disambiguation-operators
}

class RightAssociativeOperator extends Operator {

    /* This is the base class for right-associative infix operators (this is only
    currently used for exponentiation). */

    infix(parser, left) {

		this.left = left;
        this.right = parser.gatherExpression(this.mass - 1);

		return this;
	}
}

class GeneralOperator extends Operator {

    /* This is the base class for operators that have prefix and infix forms. */

    prefix(parser) {

		this.right = parser.gatherExpression(this.pull);

        return this;
	}

	infix(parser, left) {

		this.left = left;
        this.right = parser.gatherExpression(this.mass);

		return this;
	}
}

/// These are the concrete token classes that actually appear in token streams...

export class Semicolon extends Terminator {}
export class LineFeed extends Terminator {}
export class EOF extends Terminator {}

export class LineComment extends Comment {}

export class Comma extends Delimiter {}
export class Colon extends Delimiter {}

export class OpenParen extends SuffixDelimiter {}
export class OpenBracket extends SuffixDelimiter {}
export class OpenBrace extends Delimiter {}
export class CloseParen extends Delimiter {}
export class CloseBracket extends Delimiter {}
export class CloseBrace extends Delimiter {}

export class Constant extends Word {}
export class Variable extends Word {}
export class Reserved extends Word {}

export class If extends Keyword {}
export class Else extends Keyword {}
export class For extends Keyword {}
export class Do extends Keyword {}
export class Branch extends Keyword {}
export class Lambda extends Keyword {}
export class Async extends Keyword {}
export class Await extends Keyword {}
export class Functional extends Keyword {}
export class Predicated extends Keyword {}
export class Return extends Keyword {}
export class Yield extends Keyword {}
export class From extends Keyword {}
export class Command extends Keyword {}
export class Let extends Keyword {}
export class Var extends Keyword {}

export class Is extends DotOperator { mass = 8 }
export class In extends DotOperator { mass = 17 }
export class Plus extends GeneralOperator { mass = 14; pull = 11 }
export class Minus extends GeneralOperator { mass = 14; pull = 11 }
export class Not extends GeneralOperator { mass = 9; pull = 14 }
export class Dot extends DotOperator { mass = 17 }
export class Ask extends PrefixDotOperator { mass = 17; pull = 14}
export class Bang extends PrefixDotOperator { mass = 17; pull = 14 }
export class Nullish extends PrefixOperator { mass = 3; pull = 14 }
export class Lesser extends InfixOperator { mass = 9 }
export class Greater extends InfixOperator { mass = 9 }
export class NotLesser extends InfixOperator { mass = 9 }
export class NotGreater extends InfixOperator { mass = 9 }
export class Star extends InfixOperator { mass = 12 }
export class Slash extends InfixOperator { mass = 12 }
export class Floor extends InfixOperator { mass = 12 }
export class Raise extends RightAssociativeOperator { mass = 13 }
export class SkinnyArrow extends GeneralOperator { mass = 2; pull = 2 }
export class FatArrow extends GeneralOperator { mass = 2; pull = 2 }
