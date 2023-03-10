import {
    alphas,
    bases,
    closeBrace,
    closeBracket,
    closeParen,
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

export class ParserError extends SyntaxError {

    /* This concrete class is used for all syntax errors across the parser stages. */

    constructor(message, location) {

        /* Take a message string and a location `Number`, unpack the zero-indexed line and
        column numbers, then increment them, before using them to create a complete error
        message, deleting the contents of the `stack` and customizing the error name. */

        const line = Math.floor(location / 256) + 1;
        const column = location % 256 + 1;
        const locator = `[${line}:${column}]`;

        super();

        this.stack = [];
        this.name = "JSIIError";
        this.message = `${message} ${locator}`;
    }
}

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

    prefix(_) {

        throw new ParserError("invalid prefix", this.location);
    }

    infix(_) {

        throw new ParserError("invalid infix", this.location);
    }

    validate(_) {

        return false;
    }

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
}

export class Terminator extends Terminal {

    /* This is the base class for the various statement-terminators, and is used directly
    for the implicit terminators that preceed closing braces. It is also used by the lexer
    to tokenize terminators, and by the parser to classify them. */

    static * lex(lexer, location) {

        if (lexer.on(newline)) yield new LineFeed(location, "<LF>");
        else if (lexer.on(semicolon)) yield new Semicolon(location, semicolon);
        else yield new EOF(location, "<EOF>");
    }
}

export class NumberLiteral extends Terminal {

    /* This is the concrete class for all number-literals. It is also used by the lexer for
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

        if (zeroes) throw new ParserError("leading zeroes are invalid", location);
        else if (empty) throw new ParserError(`${base} prefix without digits`, location);

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

    prefix(_) { return this }

    validate(_) { return true }
}

export class StringLiteral extends Terminal {

    /* This is the concrete class for all string-literals. It is also used by the lexer
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

    prefix(_) { return this }

    validate(_) { return true }
}

export class Delimiter extends Terminal {

    /* This is the abstract base class for all delimiters, and the class used by the lexer
    for tokenizing delimiters. */

    static * lex(lexer, location) {

        const value = lexer.read();

        switch (value) {

            case comma: yield new Comma(location, value); break;

            case openParen: yield new OpenParen(location, value); break;
            case closeParen: yield new CloseParen(location, value); break;

            case openBracket: yield new OpenBracket(location, value); break;
            case closeBracket: yield new CloseBracket(location, value); break;

            case openBrace: yield new OpenBrace(location, value); break;
            case closeBrace: yield new CloseBrace(location, value); break;
        }
    }
}

export class Closer extends Delimiter {

    /* This is an abstract base class for closing parens, brackets and braces. It is used
    by the parser to check for closing tokens. */
}

class Caller extends Delimiter {

    /* This is an abstract base class for delimiters that also define a infix grammar (as
    well as starting compound expressions with their prefix grammars). In practice, this
    implies opening parens and brackets. */

    LBP = 17;
    expression = true;

    validate(_) { return true }
}

export class Word extends Terminal {

    /* This is the abstract base class for every type of word and name. It is also used
    by the lexer word-tokenization. */

    static * lex(lexer, location) {

        let value = lexer.read();

        while (lexer.at(wordCharacters)) value += lexer.advance();

        if (keywords.includes(value)) yield Keyword.subclass(location, value);
        else if (operators.includes(value)) yield Operator.subclass(location, value);
        else if (constants.includes(value)) yield new Constant(location, value);
        else if (reserved.includes(value)) yield new Reserved(location, value);
        else yield new Variable(location, value);
    }

    validate(_) { return true }
}

export class Keyword extends Word {

    /* This abstract base class is used by all of the keyword classes. It is also used by
    the `Word` class for subclassing keywords. */

    static subclass(location, value) {

        switch (value) {

            case "async": return new Async(location, value);
            case "await": return new Await(location, value);
            case "break": return new Break(location, value);
            case "continue": return new Continue(location, value);
            case "debug": return new Debug(location, value);
            case "delete": return new Delete(location, value);
            case "do": return new Do(location, value);
            case "else": return new Else(location, value);
            case "exit": return new Exit(location, value);
            case "for": return new For(location, value);
            case "from": return new From(location, value);
            case "function": return new FullFunction(location, value);
            case "generator": return new Generator(location, value);
            case "if": return new If(location, value);
            case "lambda": return new LambdaStatement(location, value);
            case "let": return new Let(location, value);
            case "pass": return new Pass(location, value);
            case "return": return new Return(location, value);
            case "unless": return new Unless(location, value);
            case "until": return new Until(location, value);
            case "var": return new Var(location, value);
            case "wait": return new Wait(location, value);
            case "while": return new While(location, value);
            case "yield": return new Yield(location, value);
        }
    }
}

class BranchStatement extends Keyword {

    /* This is the abstract base class for statements that are used to branch from loops
    (`break` and `continue`), which accept an optional (`Variable`) label. */

    prefix(parser) {

        /* If the parser is on a valid label, this method gathers it, continuing without
        operands otherwise. */

        if (parser.on(Variable)) this.push(parser.gatherVariable());

        return this;
    }

    validate(check) {

        /* Walk the block stack, ignoring simple blocks, to check if the first non-simple
        block is a loop block. If so, this statement is valid (return `true`), and not
        otherwise (return `false`). */

        return check($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }
}

class CommandStatement extends Keyword {

    /* This is the abstract class for keywords that are followed by a required, arbitrary
    expression (`await`, `delete` etc). */

    prefix(parser) {

        return this.push(parser.gatherExpression());
    }
}

class ReturningStatement extends Keyword {

    validate(check) {

        /* Climb the block stack till something functional is found, then return `true`
        if it is anything other than a class block, and `false` if it is one. */

        return check($ => $ > SIMPLEBLOCK, $ => $ < CLASSBLOCK);
    }
}

class YieldingStatement extends Keyword {

    LBP = 2
    expression = true;

    static blocks = [GENERATORBLOCK, ASYNCGENERATORBLOCK];

    validate(check) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is a block for a generator function, else `false`. */

        return check($ => $ > SIMPLEBLOCK, $ => Yield.blocks.includes($));
    }
}

export class Header extends Keyword {

    /* This is the abstract base class for statements that have a block. It is used by the
    parser to implement LIST, which permits a statement to follow another statement on the
    same line (without a semicolon), if the preceding statement ends with a block. */
}

class PredicatedBlock extends Header {

    /* This is the abstract base class for the predicated blocks (`if`, `else if`, `while`,
    `unless` and `until`). */
}

class Functional extends Header {

    /* This is the abstract base class for functional blocks, including the `lambda`,
    `function` and `generator` blocks, but not including the arrow grammars. This applies
    to function statements/expressions, whether they are asynchronous, IIFEs, neither or
    both. */

    LBP = 1;
    expression = true;

    handlePrefix(parser, prefix, doType, asyncType, full) {

        /* This helper method is used to handle qualifiers `do` or `async` (or both). */

        let blockType;

        if (prefix instanceof Do) blockType = doType;
        else if (prefix instanceof Async) blockType = asyncType;
        else throw new ParserError("invalid function qualifier", prefix.location);

        this.push(prefix);

        if (full) return this.gatherFullHeader(parser, blockType);
        else return gatherLambdaHeader(parser, blockType);
    }

    gatherFullHeader(parser, blockType) {

        /* This helper method is used by function and generator parsing methods to gather
        their optional names, optional parameters and required bodies. */

        this.push(parser.on(Variable) ? parser.gatherVariable() : null);

        if (parser.on(Of)) {

            parser.advance();

            return this.gatherLambdaHeader(parser, blockType);

        } else return this.push([], parser.gatherBlock(blockType));
    }

    gatherLambdaHeader(parser, blockType) {

        /* This helper method is used by various functional parsing methods to gather their
        optional parameters and required bodies. */

        return this.push(parser.gatherParameters(), parser.gatherBlock(blockType));
    }
}

export class Operator extends Token {

    /* This is the abstract base class for all of the operator subclasses. It is used by the
    lexer to gather an unbroken sequence of our symbolic operator characters, split it into
    individual operators (see `slice`), then yield the operators as concrete tokens, one
    at a time. */

    RBP = 0;
    expression = true;

    static slice(value, offset, location) {

        /* Slice an unbroken sequence of symbolic operator characters into an array of
        individual operators, greedily, from left to right. */

        if (!value) return [];

        if (operators.includes(value)) return [value];

        if (offset > value.length) throw new ParserError(`invalid operator (${value})`, location);

        const [start, end] = [value.slice(0, -offset), value.slice(-offset)];

        if (operators.includes(start)) return [start, ...Operator.slice(end, 1, location)];
        else return Operator.slice(value, offset + 1, location);
    }

    static subclass(location, value) {

        /* Take a value and location, and use them to instantiate and return the appropriate
        operator instance. */

        switch (value) {
            case ":": return new Colon(location, value);
            case "=": return new Assign(location, value);
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
            case "of": return new Of(location, value);
        }
    }

    static * lex(lexer, location) {

        /* This API method yields as many operators as it can gather. */

        let values = lexer.read();

        while (lexer.at(symbolics)) values += lexer.advance();

        for (const value of Operator.slice(values, 1, location)) {

            yield Operator.subclass(location, value);

            location += value.length;
        }
    }

    validate(_) { return true }

    get named() {

        /* This computed property is used by the parser to estbalish whether a given
        operator doubles as a valid property or not. */

        return alphas.includes(this.value[0]);
    }
}

class InfixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as infix
    operators. */

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP));
    }
}

class DotOperator extends Operator {

    /* This base class provides functionality for dot-operators, which must have a property
    name (which can be any type of word token) on the rightside. */

    LBP = 17;

    infix(parser, left) {

        return this.push(left, parser.gatherProperty());
    }
}

class PrefixDotOperator extends DotOperator {

    /* This base class extends the dot-operator class with functionality specific to the bang
    and ask operators, which are also bitwise prefix operators. */

    RBP = 14;

    prefix(parser) {

        return this.push(parser.gatherExpression(this.RBP));
    }
}

class GeneralOperator extends Operator {

    /* This is the base class for operators that have prefix and infix forms. */

    prefix(parser) {

        return this.push(parser.gatherExpression(this.RBP));
    }

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP));
    }
}

class Ask extends PrefixDotOperator {

    /* This concrete class implements the `?` operator, which compiles to a `Math.clz32`
    invocation in a prefix context, and `?.` in an infix context. */
}

class Assign extends InfixOperator {

    /* This concrete class implements the `=` assignment operator. */

    LBP = 2;
}

class Async extends Keyword {

    /* This concrete class implements the `async` qualifier, used to prefix the `lambda`,
    `function` and `generator` keywords to define asynchronous versions. */

    LBP = Infinity;
    expression = true;

    prefix(parser) {

        /* This method checks that the next token is valid, and if so, returns this token,
        so it can be passed to the next construct as a `left` argument, else complaining. */

        if (parser.on(Functional)) return this;
        else throw new ParserError("required a function", this.location);
    }

    infix(_, left) {

        /* This method allows `async` to be prefixed by `do`. */

        if (left instanceof Do) return this.push(left);
        else throw new ParserError("required a function", this.location);
    }
}

class Await extends CommandStatement {

    /* This concrete class implements the `await` operator, used to await promises. */

    LBP = 14;
    expression = true;

    static blocks = [ASYNCGENERATORBLOCK, ASYNCFUNCTIONBLOCK];

    validate(check) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is an asynchronous function block, and `false` otherwise. If nothing functional
        is found, the validation *succeeds* (note the third argument), as top-level-await
        is valid (unlike all other other such cases). */

        return check($ => $ > SIMPLEBLOCK, $ => Await.blocks.includes($), true);
    }
}

class Bang extends PrefixDotOperator {

    /* This concrete class implements the `!` operator, which compiles to the bitwise `~`
    operator in a prefix context, and `.#` in an infix context. */
}

class Break extends BranchStatement {

    /* This concrete class implements the `break` statement, just like JavaScript. */
}

export class CloseBrace extends Closer {

    /* This concrete class implements the `}` delimiter, used for closing blocks, object
    expressions and destructured assignees. */
}

class CloseBracket extends Closer {

    /* This concrete class implements the `]` delimiter, used for closing array expressions
    and destructured assignees. */
}

class CloseParen extends Closer {

    /* This concrete class implements the `)` delimiter, used for closing group expressions,
    arrow params and function invocations. */
}

class Colon extends InfixOperator {

    /* This concrete class implements the `:` pseudo-operator, used for delimiting key-value
    pairs in object expressions. */

    LBP = 1;
}

export class Comma extends Delimiter {

    /* This concrete class implements the `,` delimiter, used for delimiting expressions in
    groups, invocations and compound literals, as well as params, declarations etc. */
}

class Constant extends Word {

    /* This concrete class implements constant words, used for named numbers (`Infinity` and
    `NaN`), as well as magic variables (`this`, `super`, `arguments`, `random`, `null`, `void`,
    `true`, `false` and `global`). */

    expression = true;

    prefix(_) { return this }
}

class Continue extends BranchStatement {

    /* This concrete class implements the `continue` statement, just like JavaScript. */
}

class Debug extends Keyword {

    /* This concrete class implements the `debug` statement, which compiles to `debugger`. */

    prefix(_) { return this }
}

class Delete extends CommandStatement {

    /* This concrete class implements the `delete` operator, exactly like JavaScript. */

    LBP = 14;
    expression = true;
}

class Do extends Header {

    /* This concrete class implements the `do` keyword, which prefixes blocks to create
    block statements, or prefixes `async`, `lambda`, `function` or `generator` to create
    an IIFE. */

    prefix(parser) {

        /* If the next token is valid, just return this token, so it can be passed along
        as a `left` argument. Functions (and `Async`) implement `infix` grammars to take
        this kind of prefix as an argument. */

        if (parser.on(Async) || parser.on(Functional)) this.expression = true;
        else this.push(parser.gatherBlock(SIMPLEBLOCK));

        return this;
    }
}

class Dot extends DotOperator {

    /* This concrete class implements the dot operator (`.`), which is used for property
    access. The points in number literals do not use this class. */
}

class Else extends PredicatedBlock {

    /* This concrete token class implements `else` and `else if` clauses. */

    prefix(parser) {

        if (parser.on(If)) return this.push(parser.gather());
        else return this.push(parser.gatherBlock(SIMPLEBLOCK));
    }
}

export class EOF extends Terminator {

    /* This concrete class implements the implicit End Of File token that is appended
    to every token stream, and used by the parser to check for the end of the file. */
}

class Exit extends ReturningStatement {

    /* This concrete class implements the `exit` statement, which is used for returning from
    a function without a value (instead of writing `return void`). */

    prefix(_) { return this }
}

class FatArrow extends GeneralOperator {

    /* This concrete class implements the fat arrow function operator (`=>`). */

    LBP = 2;
    RBP = 2;
}

class Floor extends InfixOperator {

    /* This concrete class implements the floor division operator (`//`). */

    LBP = 12;
}

class For extends Header {}

class From extends Keyword {}

class FullFunction extends Functional {

    /* This is the concrete class for function statements, which are also expressions. */

    prefix(parser) {

        /* This method parses functions without any prefix. */

        return this.gatherFullHeader(parser, FUNCTIONBLOCK);
    }

    infix(parser, prefix) {

        /* This method allows functions to be prefixed by `do`, `async` or `do async`. */

        return this.handlePrefix(parser, prefix, FUNCTIONBLOCK, ASYNCFUNCTIONBLOCK, true);
    }
}

class Generator extends Functional {

    /* This is the concrete class for generator statements, which are also expressions. */

    prefix(parser) {

        /* This method parses generators without any prefix. */

        return this.gatherFullHeader(parser, GENERATORBLOCK);
    }

    infix(parser, prefix) {

        /* This method allows generators to be prefixed by `do`, `async` or `do async`. */

        return this.handlePrefix(parser, prefix, GENERATORBLOCK, ASYNCGENERATORBLOCK, true);
    }
}

class Greater extends InfixOperator {

    /* This concrete class implements the greater-than operator (`>`). */

    LBP = 9;
}

class If extends PredicatedBlock {

    /* This concrete class implements if-statements. */

    prefix(parser) {

        return this.push(parser.gatherExpression(), parser.gatherBlock(SIMPLEBLOCK));
    }
}

class In extends InfixOperator {

    /* This concrete class implements the in-operator. It is also used by the not-in-operator
    (handled by `Not`), and for-in-loops (handled by `For`). */

    LBP = 17;
}

class Is extends InfixOperator {

    /* This concrete class implements the is-operator and is-not-operator. */

    LBP = 8;
}

class LambdaStatement extends Functional {

    /* This is the concrete class for lambda statements, which are also expressions. */

    prefix(parser) {

        /* This method parses lambdas without any prefix. */

        return this.gatherLambdaHeader(parser, FUNCTIONBLOCK);
    }

    infix(parser, prefix) {

        /* This method allows lambdas to be prefixed by `do`, `async` or `do async`. */

        return this.handlePrefix(parser, prefix, FUNCTIONBLOCK, ASYNCFUNCTIONBLOCK, false);
    }
}

class Lesser extends InfixOperator {

    LBP = 9;
}

class Let extends Keyword {}

export class LineFeed extends Terminator {

    prefix(parser) {

        throw new ParserError("unexpected newline", this.location);
    }

    infix(parser) {

        throw new ParserError("unexpected newline", this.location);
    }
}

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

class Of extends InfixOperator {

    LBP = 18;

    infix(parser, left) {

        if (left instanceof Variable) return this.push(left, parser.gatherExpression());
        else throw new ParserError("unexpected of-operator", this.location);
    }
}

export class OpenBrace extends Delimiter {

    expression = true;

    prefix(parser) {

        /* Gather an object expression (updating the operands array in place). */

        return this.push(...parser.gatherCompoundExpression(CloseBrace));
    }

    validate(_) { return true }
}

class OpenBracket extends Caller {

    prefix(parser) {

        /* Gather an array expression (updating the operands array in place). */

        return this.push(...parser.gatherCompoundExpression(CloseBracket));
    }
}

class OpenParen extends Caller {

    prefix(parser) {

        /* Gather a group expression (updating the operands array in place). */

        return this.push(...parser.gatherCompoundExpression(CloseParen));
    }
}

class Pass extends Keyword {

    /* This concrete class implements the `pass` keyword, used to create an explicitly
    empty statement. */

    prefix(_) { return this }
}

class Plus extends GeneralOperator {

    LBP = 14;
    RBP = 11;
}

class Raise extends InfixOperator {

    /* Pratt parsers deduct `1` from the left binding power (when passing it along to
    recursive invocations) to implement operators with right-associativity. In our case,
    this rule only applies to the exponentiation operator, implemented here. */

    LBP = 13;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP - 1));
    }
}

class Reserved extends Word {

    /* This class implements reserved words, which always make it as far as the parser,
    as they are valid property names (so only invalid in any other context). */

    prefix(parser) {

        throw new ParserError(`reserved word (${this.value})`, this.location);
    }

    infix(parser) {

        throw new ParserError(`reserved word (${this.value})`, this.location);
    }
}

class Return extends ReturningStatement {

    prefix(parser) {

        /* Gather an expression (which is required in our dialect of JavaScript). */

        return this.push(parser.gatherExpression());
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

    prefix(_) { return this }
}

class Wait extends YieldingStatement {

    /* This concrete class implements the `wait` statement, which is used for yielding from
    a generator without a value (instead of writing `yield void`). */

    prefix(_) { return this }
}

class While extends PredicatedBlock {}

class Yield extends YieldingStatement {

    LBP = 2;
    expression = true;

    prefix(parser) {

        /* Gather an expression (which is required in our dialect of JavaScript), unless the
        parser is on `from`. In which case, gather the keyword, *then* gather the (required)
        expression, collecting both as operands. */

        if (parser.on(From)) return this.push(parser.advance(true), parser.gatherExpression());
        else return this.push(parser.gatherExpression());
    }
}
