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

export class Token {

    /* This is the abstract base class for all other token classes.

    Our token classes define (or inherit) *token-wise grammars*. This includes a single token
    grammar (used for tokenization), as well as a prefix grammar, an infix grammar or both
    (used for parsing). Therefore, the implementation makes no distinction between its
    *tokens* and its *AST nodes*. They are the same objects (from instantiation).

    The lexer, parser and writer are relatively small, simple functions that each define some
    local state and a high-level, declarative API, based around a handful of functions that
    share that state, and are collected into an object to form an API that can be passed
    around freely.

    This module exports a set of token classes that define the methods that are invoked by the
    lexer, parser and writer stages. Those methods take a reference to the relevant API object,
    which they then use to implement the specifics of their respective grammars in a simple,
    declarative style. This permits a very modular implementation, with textbook OOP token
    classes, operated on by closure-heavy stages. */

    LBP = 0;
    RBP = 0;
    operands = [];
    expression = false;

    constructor(location, value=empty) {

        /* This constructor takes the parameters that combine with the four defaults defined
        above to create the complete set of six properties that all instances of every token
        subclass use (we do not change the shape of the tokens during the parser stage). */

        this.location = location;
        this.value = value;
    }

    // root class default non-implementations...

    prefix(parser) { throw new SyntaxError("invalid prefix") }

    infix(parser) { throw new SyntaxError("invalid infix") }

    // root class default implementations...

    validate(parser) { return true }

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

    /* This is the base class for the various statement terminators, and is used directly
    for the implicit terminators that preceed closing braces. It is also used by the lexer
    to tokenize terminators, and by the parser to classify them. */

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

        super(location, lexer.read());

        // establish the `base`, and account for a prefix when present...

        let base;

        if (lexer.on("0") && lexer.at(bases)) {

            this.value += lexer.advance();
            base = lexer.on("xX") ? "hexadecimal" : "binary";

        } else base = "decimal";

        // gather as many digits as possible from the appropriate set, then reference
        // the value (so far) and its first character (ready for validation)...

        lexer.gatherWhile(digits[base], this);

        const [value, first] = [this.value, this.value[0]];

        // validate the value so far, by requiring that it does not start with `zeroes`,
        // unless it starts with a base prefix, or it is just a single zero, as well as
        // checking that it is not `empty` (it is not just a a base prefix without any
        // significant digits)...

        const zeroes = base === "decimal" && first === "0" && value !== "0";
        const empty = base !== "decimal" && value.length === 2;

        if (zeroes) throw new SyntaxError("cannot start decimal with leading zeroes");
        else if (empty) throw new SyntaxError(`${base} prefix without digits`);

        // now that the value has been validated, if a decimal point is currently `legal`,
        // given the value so far, and `present` in the source, which requires that at
        // least one digit immediately follows the dot, gather the dot and any digits
        // that follow it...

        const legal = base === "decimal" && first !== dot;
        const present = lexer.at(dot) && lexer.peek(+2, digits.decimal);

        if (legal && present) {

            this.value += lexer.advance();
            lexer.gatherWhile(digits[base], this);
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

        /* Yield a single delimiter token, unless it is a close brace, which must be pre-
        fixed by an implicit terminator token. This allows methods that check for the end
        of their respective grammars using `parser.on(Terminator)` to still function when
        the statement was actually terminated by a curly brace ending a block.

        Note: An extra terminator before a close brace will never invalidate (or validate)
        anything (object expressions and blocks can always contain extra lines at the end,
        and terminators are completely ignored inside object expressions anyway). */

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
}

class PrefixDotOperator extends DotOperator {

    /* This base class extends the dot-operator class with functionality specific to the bang
    and ask operators, which are also bitwise prefix operators. */

    prefix(parser) { return this.push(parser.gatherExpression(this.RBP)) }
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

    /* Implements the `await` prefix operator, used to await promises.

    Note: For-await-in loops are handled by `For`. */

    expression = true;

    static blocks = [ASYNCGENERATORBLOCK, ASYNCFUNCTIONBLOCK];

    prefix(parser) { return this.push(parser.gatherExpression()) }

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

export class Comma extends Delimiter {

    /* Implements the `,` delimiter, used for delimiting expressions in groups, invocations and
    compound literals, as well as params, declarations etc. */
}

class Constant extends Word {

    /* Implements constant words, used for named numbers (`Infinity` and `NaN`), as well as
    magic variables (`this`, `super`, `arguments`, `random`, `null`, `void`, `true`, `false`
    and `global`). */

    expression = true;
}

class Continue extends BranchStatement {

    /* Implements the `continue` statement, just like JavaScript. */
}

class Debug extends Keyword {

    /* Implements the `debug` statement, which compiles to `debugger`. */
}

class Do extends Keyword {

    /* Implements the `do` keyword, which can prefix a block to create a block statement,
    or prefix `async`, `lambda`, `function` or `generator` to create an IIFE. */

    prefix(parser) {

        if (parser.on(Async) || parser.on(FunctionalBlock)) {

            this.expression = true;
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

    /* This concrete token class implements `if` statements. */

    prefix(parser) {

        this.push(parser.gatherExpression(), parser.gatherBlock(SIMPLEBLOCK));

        return this;
    }
}

class Else extends Keyword {

    /* This concrete token class implements `else` and `else if` clauses. */

    prefix(parser) {

        if (parser.on(If)) return this.push(parser.gatherStatement());
        else return this.push(null, parser.gatherBlock(SIMPLEBLOCK));
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

export class LineFeed extends Terminator {}

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

class OpenParen extends SuffixDelimiter {

    prefix(parser) {

        this.operands = parser.gatherCompoundExpression(CloseParen);

        return this;
    }
}

class Pass extends Keyword {}

class Plus extends GeneralOperator {

    LBP = 14;
    RBP = 11;
}

class Raise extends InfixOperator {

    LBP = 13;

    infix(parser, left) {

        /* Pratt parsers deduct `1` from the left binding power (when passing it along to
        recursive invocations) to implement operators with right-associativity. In our case,
        this rule only applies to the exponentiation operator. */

        return this.push(left, parser.gatherExpression(this.LBP - 1));
    }
}

class Reserved extends Word {

    prefix(parser) { throw new SyntaxError("reserved word") }

    infix(parser) { throw new SyntaxError("reserved word") }
}

class Return extends Keyword {

    prefix(parser) {

        /* Gather an expession, if the `return` is not followed by a terminator. */

        if (!parser.on(Terminator)) this.push(parser.gatherExpression());

        return this;
    }

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

class Yield extends Keyword {

    LBP = 2;
    expression = true;

    static blocks = [GENERATORBLOCK, ASYNCGENERATORBLOCK];

    prefix(parser) {

        /* Gather an expession if the `yield` keyword is not followed by a terminator, a
        comma or a closing paren, bracket or brace (terminating the current statement or
        expression).

        Note: Yield-expressions are the only case of a valid expression that ends with an
        optional operand. Allowing for this is kind of clunky, but we only need to handle
        it once (and this is not something we can (remotely) justify redesigning). */

        const done = parser.on(Terminator, Comma, CloseParen, CloseBracket, CloseBrace);

        if (!done) this.push(parser.gatherExpression());

        return this;
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is a block for a generator function, and `false` otherwise. */

        return parser.walk($ => $ > SIMPLEBLOCK, $ => Yield.blocks.includes($));
    }
}
