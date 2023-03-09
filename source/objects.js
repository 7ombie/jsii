import {
    alphas,
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
    RBP = 0;
    operands = [];
    expression = false;

    constructor(location, value=empty) {

        this.location = location;
        this.value = value;
    }

    // root class default non-implementations...

    prefix(parser) { throw new SyntaxError("invalid prefix") }

    infix(parser) { throw new SyntaxError("invalid infix") }

    // root class default implementations...

    validate(parser) { return true }

    write(writer) { writer.push(this.value) }

    // generic helpers...

    push(...args) {

        /* This helper is used by `prefix` and `infix` methods to push zero or more
        operands to the operands array for the current instance. The method returns
        a reference to `this`, as its caller will invariably need to do so too. */

        args.forEach(arg => this.operands.push(arg));

        return this;
    }
}

class Terminal extends Token {

    /* This is the internal, abstract base class used by all terminal tokens. */

    prefix(_) { return this }
}

export class Terminator extends Terminal {

    /* This is the base class for the various statement terminators. It is also used by the
    lexer to tokenize terminators. */

    static * lex(lexer, location) {

        if (lexer.on(newline)) yield new LineFeed(location, "<LF>");
        else if (lexer.on(semicolon)) yield new Semicolon(location, semicolon);
        else yield new EOF(location, "<EOF>");
    }
}

export class NumberLiteral extends Terminal {

    /* This is the concrete class for all number literals. It is also used by the lexer for
    number tokenization. */

    expression = true;

    static * lex(...args) { yield new NumberLiteral(...args) }

    constructor(lexer, location) {

        let type;

        super(location, lexer.read());

        // first, check for a leading zero (which is invalid unless it starts a base prefix),
        // then (either way) figure out the type (binary, decimal or hexadecimal)...

        if (lexer.on("0")) {

            if (lexer.at(bases)) {

                this.value += lexer.advance();
                type = lexer.on("xX") ? "hexadecimal" : "binary";

            } else throw new SyntaxError("cannot start decimal with leading zeroes");

        } else type = "decimal";

        // gather as many digits as possible (from the appropriate set)...

        lexer.gatherWhile(digits[type], this);

        // ensure that any base prefix is followed by at least one digit...

        if (type !== "decimal" && this.value.length === 2) {

            throw new SyntaxError(`${type} prefix without digits`);
        }

        // now we have the integer part, use the `onPoint` method to establish whether or
        // not the parser is now at a decimal point, and if so, gather it and the digits
        // that will follow it...

        if (this.onPoint(lexer, type)) {

            this.value += lexer.advance();
            lexer.gatherWhile(digits[type], this);
        }
    }

    onPoint(lexer, type) {

        /* This helper method takes a reference to the Lexer API, and the type of number
        currently being parsed (by the `constructor`) as a string ("binary", "decimal" or
        "hexadecimal"). It returns `true` if the next character should be interpreted as
        a decimal point, and `false` otherwise.

        This is less trivial than it seems, as the grammar permits leading dots (ie `.5`),
        but not trailing dots (ie `5.`), to allow methods to be invoked on number literals
        directly (without parens). */

        return (
            type === "decimal"      &&
            this.value[0] !== dot   &&
            lexer.at(dot)           &&
            lexer.peek(+2, digits.decimal)
        );
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

        /* Yield a single delimiter token, unless it is a close brace, which must be pre-
        fixed by an implicit terminator token. This allows methods that check for the end
        of their respective grammars using `parser.on(Terminator)` to still function when
        the statement was actually terminated by a curly brace ending a block.

        Note: An extra terminator before a close brace will never invalidate (or validate)
        anything (terminators are completely ignored inside object expressions, and blocks
        can always contain extra lines at the end. */

        const value = lexer.read();

        switch (value) {

            case colon: yield new Colon(location, value); break;
            case comma: yield new Comma(location, value); break;

            case openParen: yield new OpenParen(location, value); break;
            case closeParen: yield new CloseParen(location, value); break;

            case openBracket: yield new OpenBracket(location, value); break;
            case closeBracket: yield new CloseBracket(location, value); break;

            case openBrace: yield new OpenBrace(location, value); break;
            case closeBrace:

                yield new Terminator(location, "<CB>");
                yield new CloseBrace(location, value); break;
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

        if (!parser.on(Terminator)) this.push(parser.gatherExpression());

        return this;
    }
}

class BranchStatement extends Keyword {

    /* This is the abstract base class for statements that are used to exit loops (`break`
    and `continue`), each accepting an optional label. */

    prefix(parser) {

        if (!parser.on(Terminator)) this.push(parser.gatherVariable());

        return this;
    }

    validate(parser) {

        /* Walk the block stack, ignoring simple blocks, to check if the first non-simple
        block is a loop block. If so, this statement is valid (return `true`), and not
        otherwise (return `false`). */

        return parser.walk($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }

    write(writer) {

        const [ label ] = this.operands;

        writer.push(this.value);

        if (label) writer.push(space, label.value);
    }
}

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

    get named() { return alphas.includes(this.value[0]) } // is operator valid as a property

    write(writer) {

        /* This provides a generic writer method for prefix and infix operators, (prefix
        operators have null `left` attributes) */

        const [ first, second ] = this.operands;

        if (second) {

            writer.push(this.value, space);
            first.write(writer);

        } else {

            first.write(writer);
            writer.push(space, this.value, space);
            second.write(writer);
        }
    }
}

class InfixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as infix
    operators. */

    infix(parser, left) { return this.push(left, parser.gatherExpression(this.LBP)) }
}

class DotOperator extends Operator {

    /* This base class provides functionality for dot-operators, which must have a property
    name (which can be any type of word token) on the rightside. */

    LBP = 17;

    infix(parser, left) { return this.push(left, parser.gatherProperty()) }

    write(writer) {

        /* This provides a writer method for dot operators (which do not normally have any
        whitespace before or after them). */

        const [ left, right ] = this.operands;

        if (left instanceof NumberLiteral) {

            writer.push(openParen);
            left.write(writer);
            writer.push(closeParen);

        } else left.write(writer);

        writer.push(this.value);
        right.write(writer);
    }
}

class PrefixDotOperator extends DotOperator {

    /* This base class extends the dot-operator class with functionality specific to the bang
    and ask operators, which are also bitwise prefix operators. */

    prefix(parser) { return this.push(parser.gatherExpression(this.RBP)) }
}

class RightAssociativeOperator extends Operator {

    /* This is the base class for right-associative infix operators (this is only currently
    used for exponentiation). */

    infix(parser, left) { return this.push(left, parser.gatherExpression(this.LBP - 1)) }
}

class GeneralOperator extends Operator {

    /* This is the base class for operators that have prefix and infix forms. */

    prefix(parser) { return this.push(parser.gatherExpression(this.RBP)) }

    infix(parser, left) { return this.push(left, parser.gatherExpression(this.LBP)) }
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

        if (parser.on(FunctionalBlock)) return this.push(parser.gatherStatement());
        else throw new SyntaxError("async qualifier without valid subject");
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
        is valid (unlike all other other such cases). */

        return parser.walk($ => $ > SIMPLEBLOCK, $ => Await.blocks.includes($), true);
    }
}

class Bang extends PrefixDotOperator {

    /* Implements the `!` operator, which compiles to the bitwise `~` operator in a prefix
    context, and `.#` in an infix context. */

    LBP = 17;
    RBP = 14;
}

class Break extends BranchStatement {

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

    write(writer) {

        if (this.value === "void") writer.push("undefined");
        else if (this.value === "random") writer.push("Math.random()");
        else writer.push(this.value);
    }
}

class Continue extends BranchStatement {

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

            this.push(parser.gatherStatement());

        } else this.push(parser.gatherBlock(SIMPLEBLOCK));

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

        this.push(parser.gatherExpression(), parser.gatherBlock(SIMPLEBLOCK));
        this.push(this.gatherOptionalElseClause(parser));

        return this;
    }

    write(writer) {

        const [ predicate, statements, clause ] = this.operands;

        writer.openPredicatedBlock("if", predicate);
        writer.indentStatements(statements);
        writer.closeBlock();

        if (clause) clause.write(writer);
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

        if (parser.on(If)) {

            parser.advance();

            return super.prefix(parser);

        } else return this.push(null, parser.gatherBlock(SIMPLEBLOCK), null);
    }

    write(writer) {

        const [ predicate, statements, clause ] = this.operands;

        if (predicate) {

            writer.openPredicatedBlock("else if", predicate);
            writer.indentStatements(statements);
            writer.closeBlock();

            if (clause) clause.write(writer);

        } else {

            writer.openUnconditionalBlock("else");
            writer.indentStatements(statements);
            writer.closeBlock();
        }
    }
}

class LambdaStatement extends FunctionalBlock {

    prefix(parser) { return this.push(parser.gatherBlock(FUNCTIONBLOCK)) }
}

class In extends InfixOperator {

    LBP = 17;
}

class Is extends InfixOperator {

    LBP = 8;
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

        /* Climb the block stack till something functional is found, then return `true` if
        it is anything other than a class block, and `false` if it is one. */

        return parser.walk($ => $ > SIMPLEBLOCK, $ => $ < CLASSBLOCK);
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

export class Variable extends Word {

    expression = true;
}

class While extends PredicatedBlock {}

class Yield extends OptionalExpressionStatement {

    LBP = 2;
    expression = true;

    static blocks = [GENERATORBLOCK, ASYNCGENERATORBLOCK];

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is a block for a generator function, and `false` otherwise. */

        return parser.walk($ => $ > SIMPLEBLOCK, $ => Yield.blocks.includes($));
    }
}
