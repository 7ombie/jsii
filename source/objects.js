import {
    alphas,
    backslash,
    bases,
    binary,
    closeBrace,
    closeBracket,
    closeParen,
    comma,
    constants,
    decimal,
    dot,
    empty,
    hexadecimal,
    keywords,
    newline,
    openBrace,
    openBracket,
    openParen,
    operators,
    quote,
    reserved,
    symbolics,
    wordCharacters,
} from "./strings.js"

const LOOPBLOCK = -1;                // while, until, for
const SIMPLEBLOCK = 0;               // if, else, unless, do
const FUNCTIONBLOCK = 1;             // lambda, function
const GENERATORBLOCK = 2;            // generator
const ASYNCGENERATORBLOCK = 3;       // async generator
const ASYNCFUNCTIONBLOCK = 4;        // async function
const CLASSBLOCK = 5;                // class

export class ParserError extends SyntaxError {

    /* This concrete class is used for all syntax errors across the parser stage. */

    constructor(message, location) {

        /* Take a message string and a location `Number`, unpack the zero-indexed line and
        column numbers, then increment them, before using them to create a complete error
        message, deleting the contents of the `stack` and customizing the error name. */

        const line = Math.floor(location / 256) + 1;
        const column = location % 256 + 1;
        const locator = `[${line}:${column}]`;

        super();

        this.stack = [];
        this.name = "ParserError";
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

    prefix(_) {

        /* This method allows every token to be parsed in a prefix position, just throwing an
        exception, unless the token defines (or inherits) something else. */

        throw new ParserError("invalid prefix", this.location);
    }

    infix(_) {

        /* This method allows every token to be parsed in an infix position, just throwing an
        exception, unless the token defines (or inherits) something else. */

        throw new ParserError("invalid infix", this.location);
    }

    validate(_) {

        /* This method allows every token to be validated (based on which types of blocks it is
        nested within), always failing, unless this method is overridden. */

        return false;
    }

    generate() {

        /* This method (which is often overridden by a subclass) is used by the code generator
        stage to convert the token into the corresponding JavaScript source. */

        return this.value;
    }

    push(...args) {

        /* This helper is used by `prefix` and `infix` methods to push zero or more operands to
        the operands array for the current instance. The method returns a reference to `this`,
        as its caller will invariably need to do so too. */

        args.forEach(arg => this.operands.push(arg));

        return this;
    }

    at(index) {

        /* This helper makes indexing operands a little bit easier. */

        return this.operands[index];
    }

    js(operand) {

        /* This helper takes an index for an operand, generates its JS source, and returns the
        result as a string. */

        return this.operands[operand].generate();
    }
}

class Terminal extends Token {

    /* This abstract base class is used by all terminal tokens (including the various words,
    literals and terminators). */
}

export class Terminator extends Terminal {

    /* This is the base class for the various statement-terminators (line feeds, commas and the
    implicit End Of File token inserted at the end of every token stream). It is also used by
    the lexer to tokenize terminators, and by the parser to classify them. */

    static * lex(lexer, location) {

        if (lexer.on(newline)) yield new LineFeed(location, "<LF>");
        else if (lexer.on(comma)) yield new Comma(location, comma);
        else yield new EOF(location, "<EOF>");
    }
}

export class NumberLiteral extends Terminal {

    /* This is the concrete class for all number-literals. It is also used by the lexer for
    number tokenization. */

    expression = true;

    static * lex(...args) { yield new NumberLiteral(...args) }

    constructor(lexer, location) {

        /* This constructor tokenizes a number literal, ensuring that dots are only including
        where they are valid (permitting number literals to be followed by a dot operator). */

        super(location, lexer.read());

        // establish the base and reference the appropriate digit-set...

        let digits;

        if (lexer.on("0") && lexer.at(bases)) {

            this.value += lexer.advance();
            digits = lexer.on("xX") ? hexadecimal : binary;

        } else digits = decimal;

        // gather as many digits as possible from the appropriate set...

        lexer.gatherWhile(digits, this);

        // validate the value so far, by requiring that it does not start with `zeroes`,
        // unless it starts with a base prefix, or it is just a single zero, as well as
        // checking that it is not `empty` (it is not just a a base prefix without any
        // significant digits)...

        const [value, first] = [this.value, this.value[0]];
        const zeroes = digits === decimal && first === "0" && value !== "0";
        const empty = digits !== decimal && value.length === 2;

        if (zeroes) throw new ParserError("leading zeroes are invalid", location);

        if (empty) throw new ParserError("incomplete base-prefix", location);

        // now that the value has been validated, if a decimal point is currently `legal`,
        // given the value so far, and `present` in the source, which requires that at
        // least one decimal digit immediately follows the dot, gather the dot along
        // with the digits that follow it...

        const legal = digits === decimal && first !== dot;
        const present = lexer.at(dot) && lexer.peek(+2, decimal);

        if (legal && present) {

            this.value += lexer.advance();
            lexer.gatherWhile(digits, this);
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

            if (lexer.on(backslash) && lexer.at(openParen)) {

                let previousState = lexer.interpolate();

                lexer.advance();
                lexer.interpolate(true);

                this.value.push([...lexer.gatherStream()]);
                this.value.push(empty);

                lexer.interpolate(previousState);

            } else this.value[this.value.length - 1] += lexer.read();
        }

        lexer.advance();
    }

    prefix(parser) {

        for (const [index, section] of Object.entries(this.value)) if (section instanceof Array) {

            const expression = this.value[index] = [...parser.parse(section)];

            for (const subexpression of expression) if (!subexpression.expression) {

                const message = "string interpolations can only contain expressions";

                throw new ParserError(message, subexpression.location);
            }
        }

        return this
    }

    validate(_) { return true }
}

export class Delimiter extends Terminal {

    /* This is the abstract base class for all delimiters, and the class used by the lexer
    for tokenizing delimiters. */

    static * lex(lexer, location) {

        const value = lexer.read();

        switch (value) {

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
        else if (constants.includes(value)) yield Constant.subclass(location, value);
        else if (reserved.includes(value)) yield new Reserved(location, value);
        else yield new Variable(location, value);
    }

    validate(_) { return true }
}

class Constant extends Word {

    /* This is the abstract base class for constant words, used for named numbers (like
    `Infinity` and `NaN`), as well as magic variables (`this`, `default`, `arguments`,
    `random`, `void`, `true` etc). It is also used by the `Word` class for sub-
    classing constants. */

    expression = true;

    prefix(_) { return this }

    static subclass(location, value) {

        switch (value) {

            case "all": return new AllConstant(location, value);
            case "arguments": return new ArgumentsConstant(location, value);
            case "default": return new DefaultConstant(location, value);
            case "false": return new FalseConstant(location, value);
            case "global": return new GlobalConstant(location, value);
            case "Infinity": return new InfinityConstant(location, value);
            case "NaN": return new NaNConstant(location, value);
            case "null": return new NullConstant(location, value);
            case "random": return new RandomConstant(location, value);
            case "super": return new SuperConstant(location, value);
            case "this": return new ThisConstant(location, value);
            case "true": return new TrueConstant(location, value);
            case "void": return new VoidConstant(location, value);
        }
    }
}

export class Keyword extends Word {

    /* This abstract base class is used by all of the keyword classes. It is also used by
    the `Word` class for subclassing keywords. */

    static subclass(location, value) {

        switch (value) {

            case "assert": return new Assert(location, value);
            case "async": return new Async(location, value);
            case "await": return new Await(location, value);
            case "break": return new Break(location, value);
            case "class": return new Class(location, value);
            case "continue": return new Continue(location, value);
            case "debug": return new Debug(location, value);
            case "delete": return new Delete(location, value);
            case "dev": return new Dev(location, value);
            case "do": return new Do(location, value);
            case "else": return new Else(location, value);
            case "export": return new Export(location, value);
            case "for": return new For(location, value);
            case "from": return new From(location, value);
            case "function": return new FullFunction(location, value);
            case "generator": return new Generator(location, value);
            case "if": return new If(location, value);
            case "import": return new Import(location, value);
            case "lambda": return new Lambda(location, value);
            case "let": return new Let(location, value);
            case "local": return new Local(location, value);
            case "pass": return new Pass(location, value);
            case "private": return new Private(location, value);
            case "return": return new Return(location, value);
            case "static": return new Static(location, value);
            case "subclass": return new Subclass(location, value);
            case "throw": return new Throw(location, value);
            case "unless": return new Unless(location, value);
            case "until": return new Until(location, value);
            case "var": return new Var(location, value);
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
}

class CommandStatement extends Keyword {

    /* This is the abstract class for keywords that are followed by a required, arbitrary
    expression (`await`, `delete` etc). */

    prefix(parser) {

        return this.push(parser.gatherExpression());
    }
}

class Declaration extends Keyword {

    /* This abstract base class implements `let` and `var` declarations. */

    prefix(parser) {

        this.push(parser.gatherExpression());

        while (parser.on(Comma)) {

            parser.advance();

            this.push(parser.gatherExpression());
        }

        return this;
    }
}

class ClassDeclaration extends Declaration {

    /* This is the abstract base class for static and public declarations. */

    prefix(parser) {

        /* This method will (correctly) gather any kind of function when present, and will
        parse a regular declaration otherwise. */

        if (parser.on(Functional, Async)) return this.push(parser.gather());
        else return super.prefix(parser);
    }

    validate(parser) {

        /* This method checks that the statement is directly contained by the body of a
        class. */

        return parser.check($ => true, $ => $ === CLASSBLOCK);
    }
}

export class Header extends Keyword {

    /* This is the abstract base class for statements that have a block. It is used by the
    parser to implement LIST, which permits a statement to follow another statement on the
    same line (without a semicolon), if the preceding statement ends with a braced block
    (not just an expression wrapped in braces). */
}

class PredicatedBlock extends Header {

    /* This is the abstract base class for the predicated blocks (`if`, `else if`, `while`,
    `unless` and `until`). */

    prefix(parser) {

        return this.push(parser.gatherExpression(), parser.gatherBlock(this.constructor.block));
    }
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
        their optional names, optional parameters and required bodies (and supports
        computed properties, using parenthesis (instead of brackets)). */

        // first, handle the name (which may be computed), if present...

        if (parser.on(Variable)) {

            this.push(parser.gatherVariable());

        } else if (parser.on(OpenParen)) {

            parser.advance();
            this.push(parser.gatherCompoundExpression(CloseParen));

        } else this.push(null);

        // now, handle the optional parameters and required function body...

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
            case ":": return new Label(location, value);
            case "=": return new Assign(location, value);
            case "+": return new Plus(location, value);
            case "-": return new Minus(location, value);
            case "*": return new Star(location, value);
            case "/": return new Slash(location, value);
            case "%": return new Modulo(location, value);
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
            case "&": return new AND(location, value);
            case "|": return new OR(location, value);
            case "||": return new XOR(location, value);
            case "<<": return new LSHIFT(location, value);
            case ">>": return new RSHIFT(location, value);
            case ">>>": return new ARSHIFT(location, value);
            case "==": return new Equal(location, value);
            case "!=": return new NotEqual(location, value);
            case "+=": return new AssignPlus(location, value);
            case "-=": return new AssignMinus(location, value);
            case "*=": return new AssignStar(location, value);
            case "/=": return new AssignSlash(location, value);
            case "%=": return new AssignModulo(location, value);
            case "//=": return new AssignFloor(location, value);
            case "**=": return new AssignRaise(location, value);
            case "&=": return new AssignAND(location, value);
            case "|=": return new AssignOR(location, value);
            case "||=": return new AssignXOR(location, value);
            case "<<=": return new AssignLSHIFT(location, value);
            case ">>=": return new AssignRSHIFT(location, value);
            case ">>>=": return new AssignARSHIFT(location, value);
            case "...": return new Spread(location, value);
            case "and": return new And(location, value);
            case "as": return new As(location, value);
            case "freeze": return new Freeze(location, value);
            case "frozen": return new Frozen(location, value);
            case "is": return new Is(location, value);
            case "in": return new In(location, value);
            case "new": return new New(location, value);
            case "not": return new Not(location, value);
            case "of": return new Of(location, value);
            case "or": return new Or(location, value);
            case "pack": return new Pack(location, value);
            case "packed": return new Packed(location, value);
            case "seal": return new Seal(location, value);
            case "sealed": return new Sealed(location, value);
            case "when": return new When(location, value);
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

class PrefixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as prefix
    operators. */

    prefix(parser) {

        return this.push(parser.gatherExpression(this.RBP));
    }
}

class InfixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as infix
    operators. */

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP));
    }

    generate() {

        return `${this.js(0)} ${this.value} ${this.js(1)}`;
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

class ArrowOperator extends GeneralOperator {

    /* This is the abstract base class for the arrow operators. Both operators support prefix
    and infix grammars, which are right-associative. */

    LBP = 2;
    RBP = 1;

    infix(parser, left) {

        /* This method overrides the inherited version to ensure that the `left` parameter
        (when present) is wrapped in parens, and that the operator is right-associative. */

        const message = "arrow function parameters must be parenthesized";

        if (left instanceof OpenParen) {

            return this.push(left, parser.gatherExpression(this.RBP));

        } else throw new ParserError(message, left.location);
    }
}

class AssignmentOperator extends InfixOperator {

    /* This is the abstract base class used by all assignment operators, which are right-
    associative. */

    LBP = 2;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP - 1));
    }
}

/// THE CONCRETE TOKEN CLASSES...

class AllConstant extends Constant {

    /* This concrete class implements the `all` constant, used instead of an asterisk
    in import and export statements. */
}

class ArgumentsConstant extends Constant {

    /* This concrete class implements the `arguments` constant. */
}

class And extends InfixOperator {

    /* This concrete class implements the logical `and` operator, which compiles to `&&`. */

    LBP = 4;
}

class AND extends InfixOperator {

    /* This concrete class implements the `&` infix operator (bitwise AND). */

    LBP = 7;
}

class ARSHIFT extends InfixOperator {

    /* This concrete class implements the `>>>` infix operator (bitwise arithmetic-
    shift-right). */

    LBP = 10;
}

class As extends InfixOperator {

    /* This concrete class implements the `as` operator, used in import and export statements
    for assignments and reassignments (like in JavaScript). */

    LBP = 1;
}

class Ask extends PrefixDotOperator {

    /* This concrete class implements the `?` operator, which compiles to a `Math.clz32`
    invocation in a prefix context, and `?.` in an infix context. */
}

class Assert extends Keyword {

    /* This conrete class provides the assert-keyword, currently only used within import
    assertions. */
}

class Assign extends AssignmentOperator {

    /* This concrete class implements the assignment infix operator (`=`). */
}

class AssignAND extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the bitwise AND
    operator (`&=`). */
}

class AssignARSHIFT extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the arithmetic
    right-shift operator (`>>>=`). */
}

class AssignFloor extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the floor-division
    operator (`//=`). */
}

class AssignPlus extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the addition
    operator (`+=`). */
}

class AssignLSHIFT extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the left-shift
    operator (`<<=`). */
}

class AssignMinus extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the subtraction
    operator (`-=`). */
}

class AssignModulo extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the modulo
    operator (`%=`). */
}

class AssignOR extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the bitwise OR
    operator (`|=`). */
}

class AssignRaise extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the exponentiation
    operator (`**=`). */
}

class AssignRSHIFT extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the right-shift
    operator (`>>=`). */
}

class AssignSlash extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the sane-division
    operator (`/=`). */
}

class AssignStar extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the multiplication
    operator (`-=`). */
}

class AssignXOR extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the bitwise XOR
    operator (`||=`). */
}

class Async extends Keyword {

    /* This concrete class implements the `async` qualifier, used to prefix the `lambda`,
    `function` and `generator` keywords to define asynchronous versions.

    The left binding power must be (at least) equal to `lambda`, `function` and `generator`,
    else the `Async` instance will not be passed to the `infix` method of the function that
    follows it when the `Async` instance is prefixed by a `Do` instance.

    Note; Both `Async` and `Do` are implemented so the qualifier is passed to the function
    that follows it. The implementation of operators that use qualifiers just gather the
    qualifier as an operand, but functions need to know if they have an `async` prefix
    to know which block-type they have (which determines which statements the block
    can contain). */

    LBP = 1;
    expression = true;

    prefix(parser) {

        /* This method checks that the next token is valid, and if so, returns this token,
        so it can be passed to the next construct as a `left` argument, else complaining. */

        if (parser.on(Functional)) return this;
        else throw new ParserError("unexpected async qualifier", this.location);
    }

    infix(parser, left) {

        /* This method allows `async` to be prefixed by `do` (and only `do`), pushing the
        `Do` instance to the `operands` array, before returning `this`, so it can be passed
        to the `infix` method of the next token. If the prefix is not `do`, or if the next
        token is not functional, an exception is raised. */

        if (left instanceof Do && parser.on(Functional)) return this.push(left);
        else throw new ParserError("unexpected async qualifier", this.location);
    }
}

class Await extends CommandStatement {

    /* This concrete class implements the `await` operator, used to await promises. */

    LBP = 14;
    expression = true;

    static blocks = [ASYNCGENERATORBLOCK, ASYNCFUNCTIONBLOCK];

    prefix(parser) {

        /* This method gathers await-expressions. */

        return this.push(parser.gatherExpression(this.LBP));
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is an asynchronous function block, and `false` otherwise. If nothing functional
        is found, the validation *succeeds* (note the third argument), as top-level await
        is valid (unlike all other other such cases). */

        return parser.check($ => $ > SIMPLEBLOCK, $ => Await.blocks.includes($), true);
    }
}

class Bang extends PrefixDotOperator {

    /* This concrete class implements the `!` operator, which compiles to the bitwise `~`
    operator in a prefix context, and `.#` in an infix context. */
}

class Break extends BranchStatement {

    /* This concrete class implements the `break` statement, which is just like JS. */

    validate(parser) {

        /* Labled break-statements are valid inside any control-flow block, so we can
        simply check the immediate context to establish that it is not the body of
        anything functional, while also ensuring the label is active.

        Unlabeled break-statements are only valid inside a loop, though they may be
        nested inside simple blocks within the loop. Therefore, we handle unlabled
        breaks by climbing the stack, ignoring any simple blocks, and checking
        that the first non-simple block is a loop. */

        if (this.operands.length) { // labeled break...

            let label = this.operands[0];

            if (parser.label(label) === null) {

                throw new ParserError(`undefined label '${label.value}'`, label.location);
            }

            return parser.check($ => true, $ => $ < FUNCTIONBLOCK);

        } else return parser.check($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }
}

class Class extends Header {

    /* This concrete class implements the `class` statement, which cannot extend anything,
    as we use `subclass` for that. */

    expression = true;

    prefix(parser) {

        /* Gather a class, with its optional name and required body. */

        if (parser.on(OpenBrace)) return this.push(parser.gatherBlock(CLASSBLOCK));

        return this.push(parser.gatherVariable(), parser.gatherBlock(CLASSBLOCK));
    }
}

export class CloseBrace extends Closer {

    /* This concrete class implements the `}` delimiter, used for closing blocks, object
    expressions and destructured assignees. */
}

export class CloseBracket extends Closer {

    /* This concrete class implements the `]` delimiter, used for closing array expressions
    and destructured assignees. */
}

class CloseParen extends Closer {

    /* This concrete class implements the `)` delimiter, used for closing group expressions,
    arrow params and function invocations. */
}

class Label extends InfixOperator {

    /* This concrete class implements the `:` pseudo-operator, used for delimiting key-value
    pairs in object expressions, and for labels on blocks. */

    LBP = 1;

    infix(parser, prefix) {

        /* If this colon is part of a label, update the parser to note whether the label is
        bound to a loop statement or a simple block statement. Then, gather the statement,
        before nullifying the label. Otherwise, just treat the colon like a normal infix
        operator. */

        if (parser.on(Do, Else, For, If, Unless, Until, While)) { // a label...

            if (parser.label(prefix) === null) parser.label(prefix, parser.on(For, Until, While));
            else throw new ParserError("cannot reassign an active label", prefix.location);

            this.expression = false;
            this.push(prefix, parser.gather());

            parser.label(prefix, null);

        } else super.infix(parser, prefix); // a key-value pair

        return this;
    }
}

export class Comma extends Terminator {

    /* This concrete class implements the `,` delimiter, used for delimiting expressions in
    groups, invocations and compound literals, as well as params, declarations etc. */
}

class Continue extends BranchStatement {

    /* This concrete class implements the `continue` statement, just like JavaScript. */

    validate(parser) {

        /* A continue-statement is only valid inside a loop (whether the statement has a
        label or not), though it may be nested inside simple blocks within the loop.
        
        If there is a label, ensure it is active and bound to a loop, then in either case,
        climb the stack, ignoring simple blocks, and check that the first non-simple block
        is a loop. */

        if (this.operands.length) { // labeled continue...

            let label = this.operands[0];
            let state = parser.label(label);

            if (state === null) throw new ParserError("undefined label", label.location);
            else if (state === false) throw new ParserError("must continue a loop", label.location);
        }

        return parser.check($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }
}

class Debug extends Keyword {

    /* This concrete class implements the `debug` statement, which compiles to `debugger`. */

    prefix(_) { return this }
}

class DefaultConstant extends Constant {

    /* This concrete class implements the `default` constant, used by import and export
    statements. */
}

class Delete extends CommandStatement {

    /* This concrete class implements the `delete` operator, exactly like JavaScript. */

    LBP = 14;
    expression = true;
}

class Dev extends Keyword {

    /* This concrete class implements the `dev` qualifier, which can prefix any statement,
    formal or informal, ensuring it will only be included in the compiled output when the
    parser is in `devmode`. */

    prefix(parser) {

        /* This method gathers anything valid as a single operand. */

        return this.push(parser.gather());
    }
}

class Do extends Header {

    /* This concrete class implements the `do` keyword, which prefixes blocks to create
    block statements, or prefixes `async`, `lambda`, `function` or `generator` to create
    an IIFE.

    Note: See `Async` for more information on why this is implemented the way it is. */

    prefix(parser) {

        /* If the next token is `async` or something functional, make this instance valid
        as an expression, then return this token, so it can be passed along as a `left`
        argument to the `infix` method of the next token. Otherwise, gather a control
        flow block (without becoming a valid expression). */

        if (parser.on(Async, Functional)) this.expression = true;
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

        /* This method gathers an else-clause or an else-if clause, with the latter having
        a predicate before its block. */

        if (parser.on(If)) return this.push(parser.gather());
        else return this.push(parser.gatherBlock(SIMPLEBLOCK));
    }
}

class Equal extends InfixOperator {

    /* This concrete class implements the `==` operator, which compiles to `Object.is`. */

    LBP = 8;
}

export class EOF extends Terminator {

    /* This concrete class implements the implicit End Of File token that is appended
    to every token stream, and used by the parser to check for the end of the file. */
}

class Export extends Keyword {

    /* This conrete class implements the export-statement, with its various grammars. */

    prefix(parser) {

        if (parser.on(DefaultConstant)) {

            return this.push(parser.advance(true), parser.gatherExpression());
        }

        this.push(parser.gatherExpression());

        if (parser.on(From)) this.push(parser.advance(true), parser.gatherExpression());

        return this;
    }
}

class FalseConstant extends Constant {

    /* This concrete class implements the `false` constant. */
}

class FatArrow extends ArrowOperator {

    /* This concrete class implements the fat-arrow function operator (`=>`). */
}

class Floor extends InfixOperator {

    /* This concrete class implements the floor division operator (`//`). */

    LBP = 12;
}

class For extends Header {

    /* This concrete class implements for-in loops (old school for-loops have not been
    designed yet, but will be added in some form. */

    prefix(parser) {

        /* This method parses a for-in-loop. It must be careful when gathering the param,
        as the `in` keyword is also an infix operator. */

        this.push(parser.gatherAssignee());

        if (parser.on(In, From)) this.push(parser.advance(parser.on(In) ? In : From));
        else throw new ParserError("incomplete for-loop", this.location);

        return this.push(parser.gatherExpression(), parser.gatherBlock(LOOPBLOCK));
    }
}

class Freeze extends PrefixOperator {

    /* This concrete class implements the `freeze` operator, which compiles to an invocation
    of `Object.freeze`. */

    RBP = 1;
}

class From extends Keyword {

    /* This concrete class implements the `from` keyword, used by import and export
    statements. */
}

class Frozen extends Operator {

    /* This concrete class implements the `frozen` operator, used by `Is` to implement the
    `is frozen` suffix operation, which compiles to an `Object.isFrozen` invocation. */
}

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

class GlobalConstant extends Constant {

    /* This concrete class implements the `global` constant, which compiles to
    `globalThis`. */
}

class Greater extends InfixOperator {

    /* This concrete class implements the greater-than operator (`>`). */

    LBP = 9;
}

class If extends PredicatedBlock {

    /* This concrete class implements if-statements. */

    static block = SIMPLEBLOCK;
}

class Import extends Keyword {

    /* This conrete class implements the import-statement, with its various grammars. */

    prefix(parser) {

        this.push(parser.gatherExpression());

        if (parser.on(Comma)) this.push(parser.advance(true), parser.gatherExpression());

        if (parser.on(From)) this.push(parser.advance(true), parser.gatherExpression());

        if (parser.on(Assert)) this.push(parser.advance(true), parser.gatherExpression());

        return this;
    }
}

class In extends InfixOperator {

    /* This concrete class implements the in-operator (`in`). */

    LBP = 8;
}

class InfinityConstant extends Constant {

    /* This concrete class implements the `Infinity` floating-point constant. */
}

class Is extends GeneralOperator {

    /* This concrete class implements the `is`, `is not`, `is packed`, `is sealed`,
    `is frozen`, `is not packed`, `is not sealed` and `is not frozen` operators. */

    LBP = 8;

    infix(parser, left) {

        this.push(left);

        if (parser.on(Not)) { this.push(parser.advance(true)) }

        if (parser.on(Packed, Sealed, Frozen)) return this.push(parser.advance(true));
        else return this.push(parser.gatherExpression(this.LBP));
    }

    generate() {

        if (this.at(1) instanceof Not) {

            if (this.at(2) instanceof Packed) return `Object.isExtensible(${this.js(0)})`;
            if (this.at(2) instanceof Sealed) return `!(Object.isSealed(${this.js(0)}))`;
            if (this.at(2) instanceof Frozen) return `!(Object.isFrozen(${this.js(0)}))`;

            return `!(${this.js(0)} instanceof ${this.js(2)})`;
        }

        if (this.at(1) instanceof Packed) return `!(Object.isExtensible(${this.js(0)}))`;
        if (this.at(1) instanceof Sealed) return `Object.isSealed(${this.js(0)})`;
        if (this.at(1) instanceof Frozen) return `Object.isFrozen(${this.js(0)})`;

        return `${this.js(0)} instanceof ${this.js(1)}`;
    }
}

class Lambda extends Functional {

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

    /* This is the concrete class for the less-than-operator (`<`). */

    LBP = 9;
}

class Let extends Declaration {

    /* This concrete class implements let-statements, which compile to const-statements. */
}

export class LineFeed extends Terminator {

    /* This concrete class implements the line feed characters, used to define newlines,
    which may or may not be significant, depending on the current LIST state, which is
    maintained by the parser implicitly (removing line feed instances from the token
    stream as required). */

    prefix(parser) {

        throw new ParserError("unexpected newline", this.location);
    }

    infix(parser) {

        throw new ParserError("unexpected newline", this.location);
    }
}

class Local extends ClassDeclaration {

    /* This contrete class implements local-statements, used inside classes. */
}

class LSHIFT extends InfixOperator {

    /* This concrete class implements the `<<` infix operator (bitwise zero-shift-left). */

    LBP = 10;
}

class Minus extends GeneralOperator {

    /* This concrete class implements the unary and binary minus operators (`-`). */

    LBP = 14;
    RBP = 11;
}

class Modulo extends InfixOperator {

    /* This concrete class implements the modulo infix operator (`%`). */

    LBP = 12;
}

class NaNConstant extends Constant {

    /* This concrete class implements the `NaN` floating-point constant. */
}

class New extends PrefixOperator {

    /* This concrete class implements the new-operator, which copies JavaScript, including
    the way `new` applies to invocations as a special-case of expression. */

    RBP = 17;
}

class Not extends GeneralOperator {

    /* This concrete class implements the `not` prefix operator, and the `not in` infix
    operator. */

    LBP = 9;
    RBP = 14;

    infix(parser, left) {

        if (parser.on(In)) {

            return this.push(left, parser.advance(true), parser.gatherExpression(this.LBP));

        } else throw new ParserError("unexpected not-operator", this.location);
    }
}

class NotEqual extends InfixOperator {

    /* This concrete class implements the `!=` operator, which compiles to `!Object.is`. */

    LBP = 8;
}

class NotGreater extends InfixOperator {

    /* This concrete class implements our not-greater-than (less-than-or-equal-to) infix
    operator (`<=`). */

    LBP = 9;
}

class NotLesser extends InfixOperator {

    /* This concrete class implements our not-less-than (greater-than-or-equal-to) infix
    operator (`>=`). */

    LBP = 9;
}

class NullConstant extends Constant {

    /* This concrete class implements the `null` constant. */
}

class Nullish extends GeneralOperator {

    /* This concrete class implements the infix nullish operator (`??`). It also handles
    pairs of clz32-operators (`?`) in a prefix position (as a disambiguation). */

    LBP = 3;    // the infix precedence applies to the nullish operator
    RBP = 14;   // the prefix precedence applies to clz32 (and equals bitwise-not)
}

class Of extends InfixOperator {

    /* This concrete class implements the `of` infix operator. This operator must be pre-
    fixed by a variable name (which it binds to unconditionally). Semantically, it compiles
    to a function invocation (as in *f of x*), using a function defined by the language. In
    practice however, the functions are always inlined to an expression denoted by the name
    on the left, applied to the (arbitrary) expression on the right.

    Note: The language does not include any kind of runtime or preamble, so having a way to
    effectively include some degree of runtime functionality is very valuable. This is why
    the of-operator was included, despite having no obvious, direct, JavaScript analog
    (it also compliments our in-operator and for-in-loops very naturally). */

    LBP = Infinity;

    infix(parser, left) {

        if (left instanceof Variable) return this.push(left, parser.gatherExpression(8));
        else throw new ParserError("unexpected of-operator", this.location);
    }
}

class Or extends InfixOperator {

    /* This concrete class implements the logical `or` operator, which compiles to `||`. */

    LBP = 3;
}

class OR extends InfixOperator {

    /* This concrete class implements the `|` infix operator (bitwise OR). */

    LBP = 5;
}

export class OpenBrace extends Delimiter {

    /* This concrete class implements the open-brace delimiter, which is used for blocks
    and object expressions. */

    expression = true;

    prefix(parser) {

        /* This method gathers an object expression, which will be validated later. */

        this.operands = parser.gatherCompoundExpression(CloseBrace);

        return this;
    }

    validate(_) { return true }
}

export class OpenBracket extends Caller {

    /* This concrete class implements the open-bracket delimiter, which is used for array
    expressions and bracket-notation. */

    prefix(parser) {

        /* This method gathers an array expression, which will be validated later. */

        this.operands = parser.gatherCompoundExpression(CloseBracket);

        return this;
    }

    infix(parser, left) {

        /* This method gathers bracket-notation, which will be validated later. */

        return this.push(left, this, ...parser.gatherCompoundExpression(CloseBracket));
    }
}

class OpenParen extends Caller {

    /* This concrete class implements the open-paren delimiter, which is used for group
    expressions and invocations. */

    prefix(parser) {

        /* This method gathers a group expression, which will be validated later. */

        this.operands = parser.gatherCompoundExpression(CloseParen);

        return this;
    }

    infix(parser, left) {

        /* This method gathers a function call, which will be validated later. */

        return this.push(left, this, ...parser.gatherCompoundExpression(CloseParen));
    }
}

class Pack extends PrefixOperator {

    /* This concrete class implements the `pack` operator, which compiles to an invocation
    of `Object.preventExtensions`. */

    RBP = 1;
}

class Packed extends Operator {

    /* This concrete class implements the `packed` operator, used by `Is` to implement the
    `is packed` and `id not packed` suffix operators, which compile to expressions using
    `Object.isExtensible` (which equates to is-not-packed). */
}

class Pass extends Keyword {

    /* This concrete class implements the `pass` keyword, used to create an explicitly
    empty statement. */

    prefix(_) { return this }
}

class Plus extends GeneralOperator {

    /* This concrete class implements the `+` operator (prefix and infix). */

    LBP = 11;   // this is the precedence of the infix operator
    RBP = 14;   // this is the precedence of the prefix operator
}

class Private extends ClassDeclaration {

    /* This contrete class implements private-statements, used inside classes. */

    prefix(parser) {

        if (parser.on(Static, Local)) return this.push(parser.gather());
        else throw new ParserError("incomplete private-declaration", this.location);
    }
}

class Raise extends InfixOperator {

    /* This concrete class implements the exponentiation infix operator, which is right-
    associative. */

    LBP = 13;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP - 1));
    }
}

class RandomConstant extends Constant {

    /* This concrete class implements the `random` constant, which always evaluates to a
    random `Number` between `0` and `1`, compiling to a `Math.random` invocation. */
}

class Reserved extends Word {

    /* This class implements reserved words, which always make it as far as the parser,
    as they are valid property names (so only invalid in any other context). */

    prefix(_) {

        throw new ParserError(`reserved word (${this.value})`, this.location);
    }

    infix(_) {

        throw new ParserError(`reserved word (${this.value})`, this.location);
    }
}

class Return extends Keyword {

    /* This concrete class implements the `return` keyword, which is followed by an
    optional expression. */

    prefix(parser) {

        if (parser.on(Terminator, Closer)) return this;
        else return this.push(parser.gatherExpression());
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true`
        if it is anything other than a class block, and `false` if it is one. */

        return parser.check($ => $ > SIMPLEBLOCK, $ => $ < CLASSBLOCK);
    }
}

class RSHIFT extends InfixOperator {

    /* This concrete class implements the `>>` infix operator (bitwise zero-shift-right). */

    LBP = 10;
}

class Seal extends PrefixOperator {

    /* This concrete class implements the `seal` operator, which compiles to an invocation
    of `Object.seal`. */

    RBP = 1;
}

class Sealed extends Operator {

    /* This concrete class implements the `sealed` operator, used by `Is` to implement the
    `is sealed` suffix operation, which compiles to an `Object.isSealed` invocation. */
}

class SkinnyArrow extends ArrowOperator {

    /* This concrete class implements the skinny-arrow function operator (`->`). */
}

class Slash extends InfixOperator {

    /* This concrete class implements the slash operator (`/`), used for (sane) division. */

    LBP = 12;
}

class Spread extends Operator {

    /* This concrete class implements the spread suffix-operator (`...`). */

    LBP = 2;

    infix(_, left) {

        return this.push(left);
    }
}

class Star extends InfixOperator {

    /* This concrete class implements the star operator (`*`), used for multiplication. */

    LBP = 12;
}

class Static extends ClassDeclaration {

    /* This contrete class implements static-statements, used inside classes. */
}

class Subclass extends Header {

    /* This concrete class implements the `subclass` statement, which is used to extend one
    class with another (like `extends` in JavaScript). */

    expression = true;

    prefix(parser) {

        if (!parser.on(Of)) this.push(parser.gatherVariable());

        if (parser.on(Of)) {

            parser.advance();

            return this.push(parser.gatherExpression(), parser.gatherBlock(CLASSBLOCK));

        } else throw new ParserError("incomplete subclass", parser.advance(true).location);
    }
}

class SuperConstant extends Constant {

    /* This concrete class implements the `super` constant. */
}

class ThisConstant extends Constant {

    /* This concrete class implements the `this` constant. */
}

class Throw extends CommandStatement {

    /* This concrete class implements the `throw` statement. */
}

class TrueConstant extends Constant {

    /* This concrete class implements the `true` constant. */
}

class Unless extends PredicatedBlock {

    /* This concrete class implements the `unless` keyword, which compiles to an if-not
    construct (without any else-clauses). */

    static block = SIMPLEBLOCK;
}

class Until extends PredicatedBlock {

    /* This concrete class implements the `until` keyword, which compiles to a while-not
    construct. */

    static block = LOOPBLOCK;
}

class Var extends Declaration {

    /* This concrete class implements var-statements, which compile to let-statements. */
}

export class Variable extends Word {

    /* This concrete class implements variable names, as a kind of word. */

    expression = true;

    prefix(_) { return this }
}

class VoidConstant extends Constant {

    /* This concrete class implements the `void` constant, which compiles to `undefined`. */
}

class When extends Operator {

    /* This concrete class implements the when-else ternary operator (`x when y else z`),
    which is right-associative. */

    LBP = 2;

    infix(parser, left) {

        this.push(left, parser.gatherExpression());

        if (parser.on(Else)) parser.advance();
        else throw new ParserError("incomplete when-operation", this.location);

        return this.push(parser.gatherExpression(this.LBP - 1));
    }
}

class While extends PredicatedBlock {

    /* This concrete class implements the `while` keyword. */

    static block = LOOPBLOCK;
}

class XOR extends InfixOperator {

    /* This concrete class implements the `||` infix operator (XOR). */

    LBP = 6;
}

class Yield extends Keyword {

    /* This concrete class implements the `yield` keyword, which can begin a formal statement,
    while also being a valid prefix operator (potentially, at the same time). */

    LBP = 2;
    expression = true;

    static blocks = [GENERATORBLOCK, ASYNCGENERATORBLOCK];

    prefix(parser) {

        /* Gather an optional expression, unless the parser is on `from`. In which case, gather
        the keyword, *then* gather a required expression. */

        if (parser.on(From)) return this.push(parser.advance(true), parser.gatherExpression());
        else if (parser.on(Terminator, Closer)) return this;
        else return this.push(parser.gatherExpression());
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is a block for a generator function, else `false`. */

        return parser.check($ => $ > SIMPLEBLOCK, $ => Yield.blocks.includes($));
    }

}
