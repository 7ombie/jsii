/* This module defines `Token` and all of its subclasses, forming a single token hierarchy that
implements all of the language specifics. */

import { constants, keywords, operators, reserved } from "./spellings.js"
import { put, not, iife } from "../core/helpers.js"
import { LarkError } from "../core/error.js"

import {
    LOOPBLOCK,
    SIMPLEBLOCK,
    FUNCTIONBLOCK,
    ASYNCFUNCTIONBLOCK,
    CLASSBLOCK,
} from "../core/blocktypes.js"

import {
    alphas,
    asterisk,
    backslash,
    backtick,
    bases,
    binary,
    closeBrace,
    closeBracket,
    closeParen,
    comma,
    decimal,
    dollar,
    dot,
    empty,
    hexadecimal,
    lowers,
    newline,
    openBrace,
    openBracket,
    openParen,
    quote,
    slash,
    space,
    symbolics,
    wordCharacters,
} from "../core/ascii.js"
import { parse } from "../core/parser.js"

/* -------------------------------------------------------------------------------------------- */
/* -------------------------------{ THE ABSTRACT TOKEN CLASSES }------------------------------- */
/* -------------------------------------------------------------------------------------------- */

export class Token extends Array {

    /* This is the abstract base class for all other token classes. Internally, Lark reuses the
    same classes for the tokens (in the token stream) and the AST nodes (in the abstract syntax
    tree). This approach makes Pratt parsing a lot easier, and permits a single OOP hierarchy.

    Tokens subclass `Array`, allowing each token to be both a token and a node (an array of zero
    or more tokens), forming as AST where child tokens are their parent's operands.

    Everything a token instance needs is defined by this class, with useful defaults, including
    various properties, the API methods that are required by the compiler stages, and a couple
    of helper methods that are only used internally (by the API methods).

    Subclasses occasionally add helper methods specific to the class, but do not change the shape
    of their instances.

    The API methods (`token`, `prefix`, `infix`, `validate` and `js`) each take a reference to the
    corresponding API object (`lexer`, `parser` or `writer`) as their first argument, and are
    individually documented below (within the default implementations).

    Every token defines or inherits `LBP` and `RBP` properties (short for *left-binding-power*
    and *right-binding-power* respectively), refered to hereafter as *LBP* and *RBP*.

    LBP is required by the Parser Stage API, and is used by Pratt's Algorithm to implement operator
    precedence. RBP is only used within this object hierarchy (it is not required by any API).

    Tokens that should not have any precedence (plain keywords, terminals, terminators etc) must
    have an LBP of zero, so they can just inherit their binding-powers from this class.

    Tokens that implement an `infix(parser)` method (infix, suffix and mixfix operators) must set
    LBP to the precedence of that operator, and will reuse the same value in any recursive calls
    to the Parser Stage API (when gathering any righthand operands), except for right-associtive
    operators, which pass `LBP - 1` instead.

    Tokens that implement a `prefix(parser)` method (prefix operators) are not *required* to set
    a binding-power property, but will need to pass the precedence of the operator in recursive
    calls to the Parser Stage API. RBP is used to store that binding-power, as there are many
    tokens that implement both prefix operators and infix/suffix operators.

    The `notes` property is an initially empty array of strings that subclasses use to note any
    variations. This is useful, as some tokens implement many different statements or operators.

    The `expression` property is set to `true` by anything that is a valid expression, and left
    `false` otherwise. The property is required by the Parser API. */

    LBP = 0;
    RBP = 0;
    notes = [];
    expression = false;

    constructor(location, value=empty) {

        /* This constructor takes and initializes the two remaining properties (`location` and
        `value`) that combine with the four properties declared already to create the complete
        set of properties that all token instances require.

        The `location` and `value` properties are provided by the Lexer Stage. The `location` is
        the location in the source file, encoded as a single integer `Number`, while `value` is
        the text of the token. */

        super();

        this.location = location;
        this.value = value;
    }

    static * token(lexer) { // api method

        /* This method instantiates tokens, based on the source. It is the only API method that
        is invoked by the Lexer Stage. It is static as it wraps the constructor method to allow
        it to yield zero or more tokens (potentially including instances of a subclass) to the
        token stream. This naturally requires that `token(lexer)` is generator too.

        This method is invariably redefined by the lower classes, so the default implementation
        only contains this docstring. */
    }

    get spelling() {

        /* This API property specifies the JavaScript spelling for the given token, defaulting
        to the Lark spelling of the same token. */

        return this.value;
    }

    prefix(parser, context=undefined) { // api method

        /* This method takes a reference to the Parser API and an optional `context`, ignores both
        and throws an exception. However, tokens that implement prefix-grammars override this with
        their own implementations that parse the token stream (pushing any operands to the token),
        before returning the resulting AST node (which will normally just be `this`).

        API methods can optionally pass a `context` (as well as a binding-power) when recursively
        invoking Parser API functions. When provided, the context gets passed (as the second arg)
        to the `prefix` method of the following token. This is required by tokens like `Do` and
        `Async` to let the tokens they qualify know the context (which is required by the token
        being qualified, so it can validate its blocks, which may contain statements (`return`
        or `await`, for example) that are only valid when specific qualifiers apply. */

        throw new LarkError("invalid token (in prefix position)", this.location);
    }

    infix(parser, left) { // api method

        /* This method is very similar to `prefix` above, except that it is invoked on tokens in
        the infix position (with something before them), and takes its `left` operand (as an AST
        node) instead of a context. The default implementation just throws an exception. */

        throw new LarkError("invalid token (in infix position)", this.location);
    }

    validate(parser) { // api method

        /* This method takes a reference to the Parser API (mainly to access the `check` function,
        which is used to validate the statement, based on the types of blocks it is nested within.
        This is used by statements like `return`, `break`, `yield` and `await`, which are invalid
        unless they are nested within specific types of blocks.

        The method returns `true` if the statement is valid, and `false` otherwise. This default
        implementation makes tokens invalid in any context. */

        return false;
    }

    js(writer) { // api method

        /* This method takes a reference to the Writer API, and is used to generate the JavaScript
        output for the token, which it returns as a string, defaulting to the `spelling` property.

        Note: This method does not need to add semi-colons to statements (that require them), as
        that is handled automatically by the Writer Stage. */

        return this.spelling;
    }

    push(...args) { // internal helper

        /* This chainable helper overrides the `push` method inherited from `Array`. This version
        accepts any number of values to push, and returns `this`. */

        args.forEach(arg => super.push(arg));

        return this;
    }

    note(...args) { // internal helper

        /* This chainable helper complements `push`, and is used to push zero or more notes to the
        `notes` array. */

        args.forEach(arg => this.notes.push(arg));

        return this;
    }

    is(...Classes) { // internal helper

        /* This helper takes any number of token subclasses, and returns `true` if `this` is an
        instance of one of the classes (or one of their subclasses), and `false` otherwise. */

        for (const Class of Classes) if (this instanceof Class) return true;

        return false;
    }
}

export class Terminal extends Token {

    /* This is the base class for all terminal tokens (including the various words, literals and
    terminators). */
}

export class Terminator extends Terminal {

    /* This is the base class for the various statement-terminators (line-feeds, commas and the
    end-of-file token (see `EOF`) implicitly inserted at the end of every token stream). It is
    imported by the Lexer Stage to tokenize terminators, as well as the Parser Stage, which
    uses it to classify them. */

    static * lex(lexer, location) {

        /* Instantiate and yield the type that corresponds to the current character (which will be
        one of `LineFeed`, `Comma` or `EOF`). */

        if (lexer.on(newline)) yield new LineFeed(location, "<LF>");
        else if (lexer.on(comma)) yield new Comma(location, comma);
        else yield new EOF(location, "<EOF>");
    }
}

export class Delimiter extends Terminal {

    /* This is the abstract base class for all delimiters. The class is also imported by the
    Lexer Stage for tokenizing delimiters. */

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

export class Opener extends Delimiter {

    /* This is an abstract base class for opening parens, brackets and braces. Subclasses implement
    grouped expressions, invocations, array literals, object literals and bracket notation. */

    check({proto, skip = 0, singular, plain}={}) {

        /* This helper is used to traverse, validate and modify the operands that represent the
        items inside the compound-expressions that are implemented by its subclasses. It takes a
        hash of arguments that specify which rules to apply, and which parameters to use, before
        returning a hash of results, currently only containing `{prototyped: Bool}`:

            + `proto Boolean`: When enabled, `__proto__` keys get converted to regular strings, so
               they have no special meaning, and `as Prototype` expressions are noted (and later
               expanded to JavaScript `__proto__: Prototype` pairs by their `js(writer)` methods).
               If exactly one such expression is noted, the `prototyped` boolean result is set to
               `true`. If more than one expression gets expanded, the helper complains instead.
               The value of `proto` defaults to `false` (so none of the above applies by default).
            + `skip Number`: When present, the check starts from (and includes) the indexed operand.
            + `plain Boolean`: When truthy, the helper checks that all of the relevant operands are
               plain expressions (without labels), complaining otherwise. Defaults to `false`.
            + `singular Boolean`: When truthy, the helper ensures that there is only one relevant
               operand. Defaults to `false`.

        Note: Only top-level operands are checked (as children handle their own operands). */

        const operands = this.slice(skip);

        // next, if the `singular` rule applies, check that there is only one relevant operand...

        if (singular) switch (operands.length) {
            case 1: break;
            case 0: throw new LarkError("expected an expression", this.location);
            default: throw new LarkError("unexpected expression", operands[1].location);
        }

        // next, if either of the `plain` or `proto` rules apply, traverse the operands, and
        // enforce whichever rules apply to each operand...

        let prototyped = false;

        if (plain || proto) for (const operand of operands) {

            if (operand.is(Label)) { // key-value pairs...

                // do not permit key-value pairs when the `plain` rule applies...

                if (plain) throw new LarkError("unexpected label", operand.location);

                // when the `proto` rule applies and the key is `__proto__`, wrap it in quotes...

                if (proto && operand.at(0).is(Variable) && operand.at(0).value === "__proto__") {

                    operand.at(0).value = quote + operand.at(0).value + quote;
                }

            } else if (proto && operand.is(As)) { // `as Type` expressions...

                // when the `proto` rule applies, disallow more than one as-expression (within a
                // given compound expression), updating `prototyped` to internally track whether
                // an as-expression was encountered, and so the caller will know (externally)
                // not to infer a `null` prototype...

                if (prototyped) throw new LarkError("superfluous prototype", operand.location);
                else prototyped = true;
            }
        }

        // finally, instantiate and return the results hash...

        return {__proto__: null, prototyped};
    }

    js(writer, opener, closer) {

        /* Take a reference to the writer stage API, an opening string and a closing string, and use
        them to output a compound expression, optionally prefixed by an expression. This is used by
        the `OpenParen` and `OpenBracket` classes to write grouped expressions, invocations, array
        literals and bracketed notation. */

        const join = operands => operands.map(operand => operand.js(writer)).join(comma + space);

        if (this.notes.includes("infix")) {

            return this.at(0).js(writer) + opener + join(this.at(1)) + closer;

        } else return opener + join(this.at(0)) + closer;
    }
}

export class Closer extends Delimiter {

    /* This is an abstract base class for closing parens, brackets and braces. It is also imported
    by the Parser Stage to check for closing tokens. */
}

export class Caller extends Opener {

    /* This is an abstract base class for delimiters that also define a infix grammar (as well as
    starting compound expressions with their prefix grammars). In practice, this implies opening
    parens and brackets. */

    LBP = 17;
    expression = true;

    validate(_) { return true }
}

export class Word extends Terminal {

    /* This is the abstract base class for every type of word and name. It is also imported by the
    Lexer Stage for word-tokenization. */

    static * lex(lexer, location) {

        /* Tokenize and yield a single word token. */

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

export class Constant extends Word {

    /* This is the abstract base class for constant words, (like `Infinity`, `NaN`, `true` etc).
    It is also used (internally) by the `Word` class (see the `subclass` method below). */

    expression = true;

    prefix(_) { return this }

    static subclass(location, value) {

        /* Return the appropriate subclass, based on the value. */

        switch (value) {
            case "false": return new FalseConstant(location, value);
            case "Infinity": return new InfinityConstant(location, value);
            case "NaN": return new NaNConstant(location, value);
            case "null": return new NullConstant(location, value);
            case "super": return new SuperConstant(location, value);
            case "true": return new TrueConstant(location, value);
            case "void": return new VoidConstant(location, value);
        }
    }
}

export class Keyword extends Word {

    /* This abstract base class is used by all of the keyword classes. It is also used (internally)
    by the `Word` class (see `subclass`). */

    static subclass(location, value) {

        /* Return the appropriate subclass, based on the value. */

        switch (value) {
            case "assert": return new Assert(location, value);
            case "async": return new Async(location, value);
            case "await": return new Await(location, value);
            case "break": return new Break(location, value);
            case "class": return new ClassLiteral(location, value);
            case "continue": return new Continue(location, value);
            case "debug": return new Debug(location, value);
            case "delete": return new Delete(location, value);
            case "dev": return new Dev(location, value);
            case "do": return new Do(location, value);
            case "else": return new Else(location, value);
            case "export": return new Export(location, value);
            case "for": return new For(location, value);
            case "from": return new From(location, value);
            case "function": return new FunctionLiteral(location, value);
            case "if": return new If(location, value);
            case "import": return new Import(location, value);
            case "let": return new Let(location, value);
            case "pass": return new Pass(location, value);
            case "private": return new Private(location, value);
            case "return": return new Return(location, value);
            case "static": return new Static(location, value);
            case "subclass": return new SubclassLiteral(location, value);
            case "throw": return new Throw(location, value);
            case "var": return new Var(location, value);
            case "while": return new While(location, value);
            case "yield": return new Yield(location, value);
        }
    }
}

export class BranchStatement extends Keyword {

    /* This is the abstract base class for statements that are used to branch from loops (in
    practice, just `break` and `continue`) and accept an optional (`Variable`) label. */

    prefix(parser) {

        /* If the parser is on a label, note and gather it. */

        if (parser.on(Variable)) this.note("label").push(parser.gatherVariable());

        return this;
    }

    js(writer) {

        /* Render a `break` or `continue` statement with its optional label. */

        const label = this.notes.includes("label") ? space + this.at(0).js(writer) : empty;

        return `${this.spelling}${label}`;
    }
}

export class CommandStatement extends Keyword {

    /* This is the abstract class for keywords that are followed by a required, arbitrary
    expression (`await`, `delete` and `throw`). */

    prefix(parser) {

        return this.push(parser.gatherExpression());
    }
}

export class Declaration extends Keyword {

    /* This abstract base class implements `let` and `var` declarations. */
}

export class Functional extends Keyword {

    /* Used to group `Async`, `FunctionLiteral`, `ClassLiteral` and `SubclassLiteral`. */
}

export class ClassQualifier extends Declaration {

    /* This is the abstract base class for the `static` and `private` qualifiers that prefix
    declarations inside classes. */

    prefix(parser) {

        /* When present, gather an optionally asynchronous function, and parse a regular
        declaration otherwise. */

        if (parser.on(Async, FunctionLiteral)) return this.push(parser.gather());
        else return super.prefix(parser);
    }

    validate(parser) {

        /* This method checks that the statement is directly contained by the body of a class. */

        return parser.check($ => true, $ => $ === CLASSBLOCK);
    }
}

export class Header extends Keyword {

    /* This is the abstract base class for statements that have a block. It is imported by the
    parser, which uses it to implement LIST correctly.*/
}

export class PredicatedBlock extends Header {

    /* This is the abstract base class for the predicated blocks (`if`, `else if`, and `while`,
    but not `else` on its own, as it has no predicate). */

    prefix(parser, context=undefined) {

        /* Gather the predicate, then the block. If the `context` is `do`, use a functional
        block (so `return` becomes legal). Otherwise, infer the blocktype frmo the `notes`
        array (which the subclasses set to `SIMPLEBLOCK` or `LOOPBLOCK`, as required). */

        const blocktype = context instanceof Do ? FUNCTIONBLOCK : this.notes.at(0);

        return this.push(parser.gatherExpression(), parser.gatherBlock(blocktype));
    }

    js(writer) {

        /* Render a statement with a predicate and a block. */

        return `${this.value} (${this.at(0).js(writer)}) ${writer.writeBlock(this.at(1))}`;
    }
}

export class Operator extends Token {

    /* This is the abstract base class for all of the operator subclasses. It is used by the
    lexer to gather an unbroken sequence of our symbolic operator characters, split it into
    individual operators (see the static `slice` helper below), then yield the operators
    as concrete tokens, one at a time. */

    expression = true;

    static slice(value, offset, location) {

        /* Take a `value` string containing a contiguous sequence of symbolic operator characters,
        and break it into an array of individual operators, greedily, from left to right, based on
        a given starting `offset` in to the string.

        The helper repeatedly yields the longest token it can make (starting from the beginning of
        the string), before recuring on the remaining characters. If it is unable to exhaust the
        string that way, the function complains instead (requiring the `location` argument).

        This is the lexer's algorithm for symbolic operator disambiguation. */

        if (not(value)) return [];

        if (operators.includes(value)) return [value];

        if (offset > value.length) throw new LarkError(`invalid operator (${value})`, location);

        const [start, end] = [value.slice(0, -offset), value.slice(-offset)];

        if (operators.includes(start)) return [start, ...Operator.slice(end, 1, location)];
        else return Operator.slice(value, offset + 1, location);
    }

    static subclass(location, value) {

        /* Take a value and its location, and use them to instantiate and return an appropriate
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
            case "\\": return new Floor(location, value);
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
            case "\\=": return new AssignFloor(location, value);
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
            case "on": return new On(location, value);
            case "or": return new Or(location, value);
            case "pack": return new Pack(location, value);
            case "packed": return new Packed(location, value);
            case "seal": return new Seal(location, value);
            case "sealed": return new Sealed(location, value);
            case "when": return new When(location, value);
        }
    }

    static * lex(lexer, location) {

        /* This API method tokenizes and yields as many operators as it can gather (refer to the
        `Operator.slice` method above for an explanation of how it works). */

        let values = lexer.read();

        while (lexer.at(symbolics)) values += lexer.advance();

        for (const value of Operator.slice(values, 1, location)) {

            yield Operator.subclass(location, value);

            location += value.length;
        }
    }

    validate(_) { return true }

    get named() {

        /* This computed property is `true` when the operator instance is named, and `false`
        otherwise. It's used by the parser to estbalish whether the operator is also a valid
        property (as operators like `not` and `throw` are also valid as property names). */

        return alphas.includes(this.value[0]);
    }
}

export class PrefixOperator extends Operator {

    /* This abstract class provides general functionality for operators that are only valid in the
    prefix denotation. */

    prefix(parser) {

        return this.push(parser.gatherExpression(this.RBP));
    }

    js(writer) {

        return `${this.spelling} ${this.at(0).js(writer)}`;
    }
}

export class InfixOperator extends Operator {

    /* This abstract class provides general functionality for operators that are only valid in the
    infix denotation. */

    initial = false;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP));
    }

    js(writer) {

        return `${this.at(0).js(writer)} ${this.spelling} ${this.at(1).js(writer)}`;
    }
}

export class DotOperator extends InfixOperator {

    /* This abstract class provides general functionality for dot-operators, which must have a
    property name (which can be any type of word token) for their righthand operand. */

    LBP = 17;
    initial = false;

    infix(parser, left) {

        return this.push(left, parser.gatherProperty());
    }

    js(writer) {

        return `${this.at(0).js(writer)}${this.spelling}${this.at(1).js(writer)}`;
    }
}

export class GeneralOperator extends Operator {

    /* This abstract class provides general functionality for operators that are valid both in the
    prefix and infix denotation. It combines (by repeating the code) the functionality of both the
    `PrefixOperator` and `InfixOperator` base classes (as we cannot use multiple-inheritance) */

    prefix(parser) {

        return this.push(parser.gatherExpression(this.RBP));
    }

    infix(parser, left) {

        return this.note("infix").push(left, parser.gatherExpression(this.LBP));
    }

    js(writer) {

        if (this.notes.includes("infix")) {

            return `${this.at(0).js(writer)} ${this.spelling} ${this.at(1).js(writer)}`;
        }

        let separator = lowers.includes(this.spelling[0]) ? space : empty;

        return `${this.spelling}${separator}${this.at(0).js(writer)}`;
    }
}

export class ArrowOperator extends GeneralOperator {

    /* This is the abstract base class for the arrow operators. Both operators support prefix
    and infix grammars, which are right-associative. */

    LBP = 2;

    infix(parser, left) {

        /* This method overrides the inherited version to ensure that the `left` parameter
        (when present) is wrapped in parens, and that the operator is right-associative. */

        const message = "arrow operators require parenthesized arguments";

        if (!left.is(OpenParen)) throw new LarkError(message, left.location);

        return this.note("infix").push(left, parser.gatherExpression(this.LBP - 1));
    }
}

export class AssignmentOperator extends InfixOperator {

    /* This is the abstract base class used by all assignment operators, which are right-
    associative. */

    LBP = 2;
    initial = false;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP - 1));
    }
}

export class GeneralDotOperator extends DotOperator {

    /* This base class extends the dot-operator class with functionality specific to the bang
    and ask operators, which are also bitwise prefix operators. */

    RBP = 14;

    prefix(parser) {

        return this.push(parser.gatherExpression(this.RBP));
    }
}

/* -------------------------------------------------------------------------------------------- */
/* -------------------------------{ THE CONCRETE TOKEN CLASSES }------------------------------- */
/* -------------------------------------------------------------------------------------------- */

export class NumberLiteral extends Terminal {

    /* This is the class for all number-literals (integers and floats, with or without a unit). It
    is also imported by the Lexer Stage for number tokenization.

    Units must immediately follow the last digit, and always consist of one or more alphas. Units
    can use binary or hexadecimal notation, but they are not compatible with exponentiation.

    Note: Datetime related units (`hrs`, `mins`, `s`, `ms` etc) are reserved while the `Temporal`
    proposal matures.

    Note: Literals that use units are not always *number* literals per say. They always compile
    to a JavaScript literal, but the result might be a number, a string or a bigint.

    TODO: Support underscore separators (requiring a digit on either side of each underscore). */

    static units = {

        ch: n => `"${n}ch"`,                    // css unit strings...
        cm: n => `"${n}cm"`,
        em: n => `"${n}em"`,
        ex: n => `"${n}ex"`,
        mm: n => `"${n}mm"`,
        p: n => `"${n}%"`,
        pc: n => `"${n}pc"`,
        pt: n => `"${n}pt"`,
        px: n => `"${n}px"`,
        rem: n => `"${n}rem"`,
        vh: n => `"${n}vh"`,
        vmax: n => `"${n}vmax"`,
        vmin: n => `"${n}vmin"`,
        vw: n => `"${n}vw"`,

        K: n => `${n * 1_000}`,                 // numeric units of magnitude...
        M: n => `${n * 1_000_000}`,
        B: n => `${n * 1_000_000_000}`,
        T: n => `${n * 1_000_000_000_000}`,
        Q: n => `${n * 1_000_000_000_000_000}`,

        n(n, float, location) {                 // bigint literals...

            if (float) throw new LarkError("invalid (fractional) bigint literal", location);
            else return `${n}n`;
        }
    };

    expression = true;

    static * lex(...args) { yield new NumberLiteral(...args) }

    constructor(lexer, location) {

        /* This generator tokenizes a number literal, ensuring that dots are only included when
        they are valid (permitting number literals to be followed by a dot-operator without any
        parens), and handling unit prefixes and exponentiation. */

        super(location, lexer.read());

        // establish the base, and create a reference to the appropriate digit-set...

        if (lexer.on("0") && lexer.at(bases)) {

            this.value += lexer.advance();

            var [digits, isDecimal] = [lexer.on("xX") ? hexadecimal : binary, false];

        } else var [digits, isDecimal] = [decimal, true];

        // gather as many digits as possible from the appropriate set...

        lexer.gatherWhile(digits, this);

        // validate the value so far, requiring that it does not start with a zero, unless it is
        // the start of a base-prefix, or it is just a single zero, as well as checking that it's
        // not *only* a base-prefix (without any significant digits)...

        if (isDecimal && this.value[0] === "0" && this.value !== "0") { // leading zeros...

            throw new LarkError("leading zeros are invalid", location);
        }

        if (not(isDecimal) && this.value.length === 2) { // incomplete base-prefix...

            if (lexer.at(decimal)) throw new LarkError("invalid digit for notation", location);
            else throw new LarkError("incomplete base-prefix", location);
        }

        // if a dot is present and legal, continue to lex this number literal using float-notation,
        // complaining if a dot is present but illegal, and doing nothing if there's no dot...

        let float = false;

        if (lexer.at(dot)) {

            if (isDecimal && this.value[0] !== dot && lexer.peek(2, decimal)) {

                float = true;
                this.value += lexer.advance();
                lexer.gatherWhile(digits, this);

            } else throw new LarkError("fractional numbers must use decimal notation", location);
        }

        // finally, check for a numeric unit xor an exponentiation pseudo-operator - on units, use
        // the appropriate unit-helper to expand the `value` property to a JavaScript literal, and
        // on operators, lex the characters and convert to JavaScript exponentiation notation - in
        // either case, update the `value` property with the result...

        const atDouble = character => lexer.at(character) && lexer.peek(2, character);

        if (lexer.at(alphas)) { // units...

            const unit = new Token(location);

            lexer.gatherWhile(alphas, unit);
            this.value = NumberLiteral.units[unit.value](parseFloat(this.value), float, location);

        } else if (atDouble(slash) || atDouble(backslash)) { // exponentiation...

            const operator = lexer.at(slash) ? "e-" : "e";
            const exponent = new Token(location);

            lexer.advance(2);
            lexer.gatherWhile(digits, exponent);
            this.value += operator + exponent.value;
        }
    }

    prefix(_) { return this }

    validate(_) { return true }
}

export class StringLiteral extends Terminal {

    /* This is the class for string literals, and the base class for text literals. It is also
    imported by the Lexer Stage for tokenizing string and text literals.

    Lark always compiles its literals to the equivalent JS template literals (using backticks).

    Note: Lark string and text literals can appear in an infix position (following some arbitrary
    expression). In that case, they have a binding power of `16`, and the result compiles to the
    equivalent tagged-template-literal. However, unlike JavaScript, Lark accepts spaces between
    the expression and the literal that follows it (and recommeds using one space).

    Characters that need escaping (grave accents, dollars followed by opening braces etc) are all
    escaped during the lexer stage (see `static * lex` below). */

    LBP = 16;
    expression = true;

    static * lex(lexer, location) {

        /* Lex and yield a string literal as a token stream that contains a `StringLiteral`, plus
        for every interpolation, an `OpenInterpolation` token, followed by each token within the
        interpolation, then a `CloseInterpolation` token, followed by another `StringLiteral`.

        Note: While a `StringLiteral` may or may not be followed by an interpolation-sequence, any
        such sequence will always be followed by a `StringLiteral` (even if it's empty), as every
        interpolation is expected to follow and be followed by a substring.

        Note: While interpolations can be nested (like any other compound expression), that will not
        affect the pattern outlined above: The initial substring will always be followed by zero or
        more pairs that each contain an interpolation sequence followed by a substring.

        This approach allows string-interpolations to be parsed like any other compound expression,
        which is important for consistency (with whitespace rules etc). */

        function match(character, value=empty) {

            /* Take a character, and an optional value string (defaulting to empty), and gather up
            any number of contiguous instances of the same given character, concatenating them to
            the value, then return the result. */

            while (lexer.at(character) && lexer.advance()) value += lexer.read();

            return value;
        }

        const [head, characters] = [match(quote, quote), []];
        const StringClass = head.length > 1 ? TextLiteral : StringLiteral;

        lexer.advance();

        while (lexer.read()) {

            // this loop can yield any number of tokens, as it yields every token within each
            // interpolation, as well any number of substrings...

            if (lexer.on(backslash) && lexer.at(openParen)) {

                // this block handles streams of interpolated tokens...

                yield new StringClass(location, characters.join(empty));

                lexer.advance(2);
                characters.length = 0;

                yield new OpenInterpolation(lexer.locate());
                yield * lexer.gather(true);
                yield new CloseInterpolation(lexer.locate());

            } else if (lexer.on(quote)) {

                // this block handles one or more quotes, which may close the literal, may be
                // part of the literal, or may just be too many quotes for the literal...

                const candidate = match(quote, quote);  // potential closing quotes
                const headCount = head.length;          // number of opening quotes
                const tailCount = candidate.length;     // number of potential closing quotes

                if (tailCount > headCount) {            // too many closing quotes...

                    let message = `${headCount} opening quotes with ${tailCount} closing quotes`;

                    throw new LarkError(message, location);
                }

                if (tailCount === headCount) {          // exactly the right number of quotes...

                    return yield new StringClass(location, characters.join(empty));

                } else characters.push(candidate);      // too few quotes (part of the string)

            } else if (lexer.on(newline)) {

                // this block handles newlines, followed by zero or more spaces...

                lexer.terminate();
                characters.push(lexer.read() + match(space));

            } else if (lexer.on(backtick) || (lexer.on(dollar) && lexer.at(openBrace))) {

                // this block handles characters that are meaningful in a js template-literal, but
                // have no special meaning in a lark string or text literal...

                characters.push(backslash, lexer.read());

            } else characters.push(lexer.read()); // this block handles a regular character

            lexer.advance();
        }
    }

    prefix(parser) {

        /* Gather one or more pairs of operands, each containing an interpolation array, followed
        by a (required) `StringLiteral` substring (that the lexer ensures will be there). */

        while (parser.on(OpenInterpolation)) {

            parser.advance();
            this.push(parser.gatherCompoundExpression(CloseInterpolation), parser.advance(false));
        }

        return this;
    }

    infix(parser, left) {

        /* Take the left operand, push it to the token and note that this literal is tagged, before
        deferring parsing to the `prefix(parser)` method. */

        return this.note("tagged").push(left).prefix(parser);
    }

    validate(_) { return true }

    js(writer) {

        /* Generate a template literal from the various parts of the string literal (text literals
        have their own `js(writer)` method), reproducing the interpolations, and possibly including
        a tag-expression. */

        if (this.notes.includes("tagged")) {

            var prefix = this.at(0).js(writer)
            var string = this.value;

            this.splice(1);

        } else {

            var prefix = empty;
            var string = this.value;
        }

        for (const operand of this) {

            if (operand instanceof CompoundExpression) for (const interpolation of operand) {

                string += "${" + interpolation.js(writer) + "}";

            } else string += operand.value;
        }

        return prefix + backtick + string + backtick;
    }
}

export class TextLiteral extends StringLiteral {

    /* This class implements text literals, inheriting a lot from `StringLiteral`. */

    js(writer) {

        /* Generate a template literal from the various parts of the text literal, reproducing
        the interpolations, and possibly including a tag-expression. */

        const strip = string => string.replaceAll(indentation, newline);

        const self = this, indentation = iife(function() {

            /* This IIFE computes the indentation to remove from each line (as a string containing
            a newline, followed by zero or more spaces), returning it, so it becomes `indentation`
            in the outer scope. */

            const value = (self.at(-1) ?? self).value;
            const index = value.lastIndexOf(newline);

            return value.slice(index);
        });

        if (this.notes.includes("tagged")) {

            var prefix = this.at(0).js(writer);
            var string = strip(this.value);

            this.splice(1);

        } else {

            var prefix = empty;
            var string = strip(this.value);
        }

        // remove superfluous leading newline (from replacing each indent with a newline)...

        string = string.slice(1);

        // now, iterate over the token's (remaining) operands, convert them to js, wrapping
        // any interpolations appropriately, and concatenating the results to `string`...

        for (const operand of this) {

            if (operand instanceof CompoundExpression) for (const interpolation of operand) {

                string += "${" + interpolation.js(writer) + "}";

            } else string += strip(operand.value);
        }

        // finally, remove the superfluous trailing newline, then concatenate everything
        // together, and return the result...

        string = string.slice(0, -1);

        return prefix + backtick + string + backtick;
    }
}

export class And extends InfixOperator {

    /* This class implements the logical `and` operator, which compiles to `&&`. */

    LBP = 4;
    spelling = "&&";
}

export class AND extends InfixOperator {

    /* This class implements the `&` infix operator (bitwise AND). */

    LBP = 7;
}

export class ARSHIFT extends InfixOperator {

    /* This class implements the `>>>` infix operator (bitwise arithmetic-shift-right). */

    LBP = 10;
}

export class As extends PrefixOperator {

    /* This class implements the `as` prefix-operator, used to specify prototypes in object
    literals: `let oo = {as Object, x: 1, y: 2}` */

    LBP = Infinity;

    js(writer) {

        return `__proto__: ${this.at(0).js(writer)}`;
    }
}

export class Ask extends GeneralDotOperator {

    spelling = "?.";

    /* This class implements the `?` operator, which compiles to a `Math.clz32` invocation in a
    prefix context, and `?.` in an infix context. */
}

export class Assert extends Keyword {

    /* This conrete class implements the `assert` keyword, which we use for import assertions. */
}

export class Assign extends AssignmentOperator {

    /* This concrete class implements the plain assignment operator (`=`). */
}

export class AssignAND extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the bitwise AND operator (`&=`). */
}

export class AssignARSHIFT extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the arithmetic right-shift operator
    (`>>>=`). */
}

export class AssignFloor extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the floor-division operator (`\=`).
    For example, `x \= y` compiles to `x = Math.floor(x / y)` (using registers as required). */
}

export class AssignPlus extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the addition operator (`+=`). */
}

export class AssignLSHIFT extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the left-shift operator (`<<=`). */
}

export class AssignMinus extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the subtraction operator (`-=`). */
}

export class AssignModulo extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the modulo operator (`%=`). */
}

export class AssignOR extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the bitwise OR operator (`|=`). */
}

export class AssignRaise extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the raise operator (`**=`). */
}

export class AssignRSHIFT extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the right-shift operator (`>>=`). */
}

export class AssignSlash extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the division operator (`/=`). */
}

export class AssignStar extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the multiply operator (`*=`). */
}

export class AssignXOR extends AssignmentOperator {

    /* This class implements the inplace-assignment version of the bitwise XOR operator (`||=`). */
}

export class Async extends Functional {

    /* This class implements the `async` function-qualifier. */

    expression = true;

    prefix(parser) {

        /* This method checks that the next token is a function literal, and if so, gathers it as
        an operand, complaining otherwise. */

        if (parser.on(FunctionLiteral)) return this.push(parser.gather(0, this));
        else throw new LarkError("`async` qualifier without `function` keyword", this.location);
    }

    js(writer) {

        /* Render the function operand with the `async` qualifier. */

        return `async ${this.at(0).js(writer)}`;
    }
}

export class Await extends CommandStatement {

    /* This class implements the `await` operator. */

    RBP = 14;
    expression = true;

    prefix(parser) {

        /* This method gathers `await` expressions. */

        return this.push(parser.gatherExpression(this.RBP));
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is an asynchronous function block, and `false` otherwise. If nothing functional
        is found, the validation *succeeds* (note the third argument), as top-level await
        is valid. */

        return parser.check($ => $ > SIMPLEBLOCK, $ => ASYNCFUNCTIONBLOCK, true);
    }

    js(writer) {

        /* Render an `await` expression. */

        return `await ${this.at(0).js(writer)}`;
    }
}

export class Bang extends GeneralDotOperator {

    spelling = ".#";

    /* This class implements the `!` operator, which compiles to the bitwise `~` operator in a
    prefix context, and JavaScript's `.#` pseudo-dot-operator in an infix context. */
}

export class Block extends Token {

    /* Used to group statements into a block. */
}

export class Break extends BranchStatement {

    /* This class implements the `break` statement. */

    validate(parser) {

        /* Labled break-statements are valid inside any control-flow block, so we can simply check
        the immediate context to establish that it is not the body of anything functional, while
        also ensuring the label is active.

        Unlabeled break-statements are only valid inside a loop, though they may be nested inside
        simple blocks within the loop. Therefore, we handle unlabled breaks by climbing the stack,
        ignoring any simple blocks, and checking that the first non-simple block is a loop. */

        if (this.notes.includes("label")) {

            if (not(parser.label(this.at(0)))) {

                throw new LarkError(`undefined label '${this.at(0).value}'`, this.at(0).location);
            }

            return parser.check($ => true, $ => $ < FUNCTIONBLOCK);

        } else return parser.check($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }
}

export class ClassLiteral extends Functional {

    /* This class implements `class` literals, which cannot extend anything, as we use `subclass`
    for that. */

    expression = true;

    prefix(parser) {

        /* Gather a class, with its optional name and required body. */

        if (parser.on(OpenBrace)) return this.push(parser.gatherBlock(CLASSBLOCK));

        return this.push(parser.gatherVariable(), parser.gatherBlock(CLASSBLOCK));
    }
}

export class CloseBrace extends Closer {

    /* This class implements the `}` delimiter, which is used for closing blocks, as well as object
    expressions and destructured assignees. */
}

export class CloseBracket extends Closer {

    /* This class implements the `]` delimiter, which is used for closing array expressions, as well
    as destructured assignees. */
}

export class CloseInterpolation extends Opener {
    
    /* This concrete class implements the `)` delimiter within string interpolations, and is
    used by `StringLiteral` (allowing interpolations to be parsed as compound expressions). */
}

export class CloseParen extends Closer {

    /* This concrete class implements the `)` delimiter (outside of string literals), and is used
    for closing group expressions, parameters for arrow operations and function invocations. */
}

export class CompoundExpression extends Token {

    /* Used to group the contents of various compound expressions into an AST node. */
}

export class Label extends InfixOperator {

    /* This class implements the `:` pseudo-operator, used for delimiting key-value pairs in object
    expressions, and for labelling control-flow blocks. */

    LBP = 1;

    infix(parser, left) {

        /* If this colon is part of a label, update the parser to note whether the label is bound
        to a loop statement or a simple block statement. Then, gather the statement, before
        nullifying the label. Otherwise, just treat the colon like a normal infix operator. */

        if (parser.on(If, Else, While, For)) { // a label...

            if (parser.label(left) === null) parser.label(left, parser.on(For, While));
            else throw new LarkError("cannot reassign an active label", left.location);

            this.expression = false;
            this.push(left, parser.gather());
            parser.label(left, null);

        } else super.infix(parser, left); // a key-value pair

        return this;
    }

    js(writer) {

        /* Render a labelled operand (knowing that it was validated during the parser stage). */

        return `${this.at(0).js(writer)}: ${this.at(1).js(writer)}`;
    }
}

export class Comma extends Terminator {

    /* This class implements the `,` delimiter, used for separating expressions within compound
    expressions and function parameters, as well as multiple statements on the same line. */
}

export class Continue extends BranchStatement {

    /* This class implements the `continue` statement, just like JavaScript. */

    validate(parser) {

        /* A continue-statement is only valid inside a loop (whether the statement has a label or
        not), though it may be nested inside simple blocks within the loop.

        If there is a label, ensure it is active and bound to a loop, then (in either case) climb
        the stack, ignoring simple blocks, and check that the first non-simple block is a loop. */

        if (this.notes.includes("label")) {

            let label = this.at(0);
            let state = parser.label(label);

            if (state === null) throw new LarkError("undefined label", label.location);
            else if (state === false) throw new LarkError("must continue a loop", label.location);
        }

        return parser.check($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }
}

export class Debug extends Keyword {

    /* This class implements the `debug` statement, which compiles to `debugger`. */

    spelling = "debugger";

    prefix(_) { return this }
}

export class Delete extends CommandStatement {

    /* This class implements the `delete` operator, exactly like JavaScript. */

    LBP = 14;
    expression = true;

    js(writer) {

        /* Render a `delete` expression. */

        return `delete ${this.at(0).js(writer)}`;
    }
}

export class Dev extends Keyword {

    /* This class implements the `dev` qualifier, which can prefix any statement, whether formal or
    informal, ensuring it will only be included in the compiled JavaScript when the `dev` flag
    (which is `false` by default) is truthy. */

    prefix(parser) {

        /* Gather an arbitrary statement, while noting the `dev` flag, so this statement can be
        omitted from the output correctly (without the semicolon we would get if `js(writer)`
        simply returned an empty string). */

        if (parser.dev) this.note("ignore");

        return this.push(parser.gather());
    }

    js(writer) {

        /* Render the only operand and return the result, leaving it to the writer stage to omit
        the result when `ignore` was noted by `prefix(parser)`. */

        return this.at(0).js(writer);
    }
}

export class Do extends PrefixOperator {

    /* This class implements the `do` keyword, which prefixes blocks and functions to create IIFEs
    (which Lark uses as a more flexible version of block-statements). */

    prefix(parser) {

        /* If the next token is `async` or `function`, make this instance valid as an expression,
        then gather whatever follows. Otherwise, gather a do-block. */

        if (parser.on(If, While, For, Functional)) return this.push(parser.gather(0, this));
        else return this.push(parser.gatherBlock(FUNCTIONBLOCK));
    }

    js(writer) {

        /* Render a statement with a `do` qualifier, which can be a do-block (which compiles to an
        IIFE with no name or arguments etc), a control-flow block (optionally containing `return`
        statements, which compiles to an IIFE with the statement inside), or a function (which
        just gets dangling-dogballs appended). */

        if (this.at(0) instanceof Block) return `function() ${writer.writeBlock(this.at(0))}()`;

        if (this.at(0).is(Functional)) return `${this.at(0).js(writer)}()`;

        const literal = new FunctionLiteral(this.location);
        const parameters = new Parameters(this.location);
        const block = new Block(this.location).push(this.at(0))

        literal.push(null, parameters, block);

        return `${literal.js(writer)}()`;
    }
}

export class Dot extends DotOperator {

    /* This class implements the dot operator (`.`), which is used for property access. The points
    in number literals do not use this class. */
}

export class Else extends PredicatedBlock {

    /* This concrete class implements `else` and `else if` clauses. */

    prefix(parser) {

        /* Gather an else-clause (with its block) or an else-if clause (with its predicate before
        its block). */

        if (parser.on(If)) return this.note("else-if").push(parser.gather());
        else return this.push(parser.gatherBlock(SIMPLEBLOCK));
    }

    js(writer) {

        /* Render an `else` or an `else if` statement. */

        if (this.notes.includes("else-if")) return `else ${this.at(0).js(writer)}`;
        else return `else ${writer.writeBlock(this.at(0))}`;
    }
}

export class SkipAssignee extends Token {

    /* This class is used to represent an empty slot in an unpacking assignment (just a comma). */
}

export class Equal extends InfixOperator {

    /* This concrete class implements the `==` operator, which compiles to `Object.is`. */

    LBP = 8;

    js(writer) {

        /* Render an equality operation. */

        return `Object.is(${this.at(0).js(writer)}, ${this.at(1).js(writer)})`;
    }
}

export class EOF extends Terminator {

    /* This class implements the end-of-file token that is implicitly appended to every token
    stream, so the parser can check for the end of the file. */
}

export class Export extends Keyword {

    /* This class implements the export-statement. */
}

export class FalseConstant extends Constant {

    /* This class implements the `false` constant. */
}

export class FatArrow extends ArrowOperator {

    /* This class implements the fat-arrow function-operator (`=>`). */
}

export class Floor extends InfixOperator {

    /* This class implements the floor-division infix operator (`\`). */

    LBP = 12;

    js(writer) {

        /* Render a floor-division operation. */

        return `Math.floor(${this.at(0).js(writer)} / ${this.at(1).js(writer)})`;
    }
}

export class For extends Header {

    /* This class implements for-loops (old school for-loops have not been designed yet, but will
    be added in some form). */

    prefix(parser, context=undefined) {

        /* This method parses all four for-loops. It uses `parser.gatherAssignee` to avoid parsing
        the operator (`in`, `of`, `on` or `from`) as an infix. */

        const blocktype = context instanceof Do ? FUNCTIONBLOCK : LOOPBLOCK;

        this.push(parser.gatherAssignee());

        if (parser.on(In, Of, On, From)) this.note(parser.advance(false).value);
        else throw new LarkError("incomplete for-statement", this.location);

        return this.push(parser.gatherExpression(), parser.gatherBlock(blocktype));
    }

    js(writer) {

        /* Render the appropriate for-loop, adding the extra code that implements the operator. */

        const assignees = this.at(0).js(writer);
        const keyword = this.notes.includes("from") ? "for await" : "for";
        const operator = this.notes.includes("on") ? "in" : "of";
        const block = writer.writeBlock(this.at(2));

        let expression = writer.register(this.at(1));

        if (this.notes.includes("in")) {

            expression = `${expression}?.ƥvalues?.() ?? Object.values(${expression} ?? [])`;

        } else if (this.notes.includes("of")) {

            expression = `${expression}?.ƥkeys?.() ?? Object.keys(${expression} ?? {})`;
        }

        return `${keyword} (const ${assignees} ${operator} ${expression}) ${block}`;
    }
}

export class Freeze extends PrefixOperator {

    /* This class implements the `freeze` operator, which compiles to an invocation of
    `Object.freeze`. */

    RBP = 1;

    js(writer) {

        return `Object.freeze(${this.at(0).js(writer)})`;
    }
}

export class From extends Keyword {

    /* This class implements the `from` keyword, used by import and export statements. */
}

export class Frozen extends Operator {

    /* This concrete class implements the `frozen` operator, used by `Is` to implement the
    `is frozen` suffix operation, which compiles to an `Object.isFrozen` invocation. */
}

export class FunctionLiteral extends Functional {

    /* This class implements function literals. */

    LBP = 1;
    expression = true;

    static isGenerator(block) { // TODO: implement

        return false;
    }

    prefix(parser, context) {

        /* This method parses functions, based on the given context (either `Async` or `undefined`,
        with the later implying no qualifier). */

        let blockType = context?.is(Async) ? ASYNCFUNCTIONBLOCK : FUNCTIONBLOCK;

        // having established the blocktype, parse the optional function name, which when present,
        // may use a computed (runtime) value...

        if (parser.on(Variable)) {

            this.push(parser.gatherVariable());

        } else if (parser.on(OpenParen)) {

            parser.advance();
            this.push(parser.gatherCompoundExpression(CloseParen));

        } else this.push(new SkipAssignee(this.location));

        // now, handle the optional parameters and required function body...

        if (parser.on(Of)) {

            parser.advance();

            this.push(parser.gatherParameters(), parser.gatherBlock(blockType));

        } else this.push(new Parameters(this.location), parser.gatherBlock(blockType));

        this.note(FunctionLiteral.isGenerator(this.at(2)) ? "generator" : "function");

        return this;
    }

    js(writer) {

        /* Render a function literal (arrow grammar is not handled here). */

        let name = empty;

        // having assumed that `this.at(0)` is a `SkipAssignee` (indicating an anonymous function),
        // now check if it is a `CompoundExpression` (indicating a computed name) or a `Variable`
        // (indicating a regular named-function), and update `name` accordingly...

        if (this.at(0) instanceof CompoundExpression) {

            name = space + openBracket + this.at(0).at(0).js(writer) + closeBracket;

        } else if (this.at(0) instanceof Variable) name = space + this.at(0).js(writer);

        // prerender the keyword, parameters and the function body, then interpolate them into a
        // literal, and return the result...

        const keyword = "function" + (this.notes.includes("generator") ? asterisk : empty);
        const params = this.at(1).map(param => param.js(writer)).join(comma + space);
        const block = writer.writeBlock(this.at(2));

        return `${keyword}${name}(${params}) ${block}`;
    }
}

export class Greater extends InfixOperator {

    /* This class implements the greater-than operator (`>`). */

    LBP = 9;
}

export class If extends PredicatedBlock {

    /* This class implements `if` statements. */

    notes = [SIMPLEBLOCK];
}

export class Import extends Keyword {

    /* This class implements the `import` statement, with its various grammars. */
}

export class In extends InfixOperator {

    /* This concrete class implements the `in` infix-operator, which is used for membership
    tests on collections of any type. */

    LBP = 8;

    js(writer) {

        const operand = this.at(0).js(writer);
        const name = writer.register(this.at(1));

        return `(${name}?.ƥvalues?.() ?? Object.values(${name} ?? [])).includes(${operand})`;
    }
}

export class InfinityConstant extends Constant {

    /* This class implements the `Infinity` floating-point constant. */
}

export class Is extends InfixOperator {

    /* This class implements the `is`, `is not`, `is packed`, `is sealed`, `is frozen`,
    `is not packed`, `is not sealed` and `is not frozen` operators. */

    LBP = 8;

    infix(parser, left) {

        this.push(left);

        if (parser.on(Not)) { this.push(parser.advance(false)) }

        if (parser.on(Packed, Sealed, Frozen)) return this.push(parser.advance(false));
        else return this.push(parser.gatherExpression(this.LBP));
    }

    js(writer) {

        /* Render an `is` or `is not`, `is packed`, `is not packed`, `is sealed`, `is not sealed`,
        `is frozen` or `is not frozen` operation. */

        if (this.at(1).is(Not)) {

            if (this.at(2).is(Packed)) return `Object.isExtensible(${this.at(0).js(writer)})`;
            if (this.at(2).is(Sealed)) return `(!Object.isSealed(${this.at(0).js(writer)}))`;
            if (this.at(2).is(Frozen)) return `(!Object.isFrozen(${this.at(0).js(writer)}))`;

            const [value, type] = [writer.register(this.at(0)), writer.register(this.at(2))];

            return `(!(${value}?.ƥis?.(${type}) ?? ${value} instanceof ${type}))`;
        }

        if (this.at(1).is(Packed)) return `(!Object.isExtensible(${this.at(0).js(writer)}))`;
        if (this.at(1).is(Sealed)) return `Object.isSealed(${this.at(0).js(writer)})`;
        if (this.at(1).is(Frozen)) return `Object.isFrozen(${this.at(0).js(writer)})`;

        const [value, type] = [writer.register(this.at(0)), writer.register(this.at(1))];

        return `(${value}?.ƥis?.(${type}) ?? ${value} instanceof ${type})`;
    }
}

export class Lesser extends InfixOperator {

    /* This class implements the less-than-operator (`<`). */

    LBP = 9;
}

export class Let extends Declaration {

    /* This class implements let-statements, which compile to const-statements. */
}

export class LineFeed extends Terminator {

    /* This class implements the line-feed characters, used to define newlines, which may or may
    not be significant, depending on the current LIST state, which is maintained by the parser
    implicitly (removing line-feed instances from the token stream as required). */

    prefix(parser) {

        throw new LarkError("unexpected newline", this.location);
    }

    infix(parser) {

        throw new LarkError("unexpected newline", this.location);
    }
}

export class LSHIFT extends InfixOperator {

    /* This class implements the `<<` infix operator (bitwise zero-shift-left). */

    LBP = 10;
}

export class Minus extends GeneralOperator {

    /* This class implements the unary and binary minus operators (`-`). */

    LBP = 14;
    RBP = 11;
}

export class Modulo extends InfixOperator {

    /* This class implements the modulo infix operator (`%`). */

    LBP = 12;
}

export class NaNConstant extends Constant {

    /* This class implements the `NaN` floating-point constant. */
}

export class New extends PrefixOperator {

    /* This class implements the new-operator, which copies JavaScript, including the way `new`
    applies to invocations as a special-case of expression. */

    RBP = 17;
}

export class Not extends GeneralOperator {

    /* This class implements the `not` prefix operator, and the `not in` infix operator. */

    LBP = 9;
    RBP = 14;
    spelling = "!";

    infix(parser, left) {

        /* If the parser is on the `in` operator, note the infix denotation, push the left operand,
        then gather and push the right operand. Otherwise, throw an error. */

        if (parser.on(In)) {

            parser.advance();

            return this.note("infix").push(left, parser.gatherExpression(this.LBP));

        } else throw new LarkError("unexpected not-operator", this.location);
    }

    js(writer) {

        /* Render a `not` prefix operation or a `not in` infix operation. */

        if (this.notes.includes("infix")) {

            const register = writer.register(this.at(1));
            const values = `!((${register}?.ƥvalues?.() ?? Object.values(${register} ?? []))`;

            return values + `.includes(${this.at(0).js(writer)}))`;

        } else return super.js(writer);
    }
}

export class NotEqual extends InfixOperator {

    /* This class implements the `!=` operator, which compiles to `!Object.is`. */

    LBP = 8;

    js(writer) {

        return `!Object.is(${this.at(0).js(writer)}, ${this.at(1).js(writer)})`;
    }
}

export class NotGreater extends InfixOperator {

    /* This class implements our not-greater-than operator (`<=`). */

    LBP = 9;
}

export class NotLesser extends InfixOperator {

    /* This class implements our not-less-than operator (`>=`). */

    LBP = 9;
}

export class NullConstant extends Constant {

    /* This class implements the `null` constant. */
}

export class Nullish extends GeneralOperator {

    /* This class implements the infix nullish operator (`??`). It also handles pairs of `?` (count
    leading zeros) operators in a prefix position. This is an unlikely (but possible) edgecase,
    where the operator parser requires further disambiguation. */

    LBP = 3;
    RBP = 14;
}

export class Of extends InfixOperator {

    /* This class implements the infix of-operator, which is also used by functions to prefix their
    (optional) arguments, as well as `for-of` loops. */

    LBP = 8;
}

export class On extends InfixOperator {

    /* This class implements the `on` operator, which compiles to the JS `in` operator. */

    LBP = 9;
    spelling = "in";
}

export class Or extends InfixOperator {

    /* This class implements the logical `or` operator, which compiles to `||`. */

    LBP = 3;
    spelling = "||";
}

export class OR extends InfixOperator {

    /* This class implements the bitwise-or operator, which uses `|`, like JavaScript. */

    LBP = 5;
}

export class OpenBrace extends Opener {

    /* This class implements the open-brace delimiter, which is used for blocks and object
    expressions. */

    expression = true;

    prefix(parser) {

        /* This method gathers an object expression, noting when it specifies a prototype. */

        this.push(parser.gatherCompoundExpression(CloseBrace));

        if (this.check({proto: true}).prototyped) this.note("proto");

        return this;
    }

    validate(_) { return true }

    js(writer) {

        /* Render an object literal. Infer a `null` prototype, unless it contains an `as Prototype`
        expression (indicated by having a "proto" note). */

        const head = this.notes.includes("proto") ? openBrace : "{__proto__: null, ";
        const body = this.at(0).map(operand => operand.js(writer)).join(comma + space);

        return head + body + closeBrace;
    }
}

export class OpenBracket extends Caller {

    /* This class implements the open-bracket delimiter, which is used for array expressions and
    bracket-notation. */

    prefix(parser) {

        /* This method gathers an array expression. */

        this.push(parser.gatherCompoundExpression(CloseBracket));
        this.check({plain: true});

        return this;
    }

    infix(parser, left) {

        /* This method gathers bracket-notation. */

        this.note("infix").push(left, parser.gatherCompoundExpression(CloseBracket));
        this.check({skip: 1, singular: true, plain: true});

        return this;
    }

    js(writer) {

        /* Render an array literal or bracket-notation. */

        return super.js(writer, openBracket, closeBracket);
    }
}

export class OpenInterpolation extends Opener {
    
    /* This class is used by `StringLiteral` for delimiting the tokens within string interpolations
    (allowing them to be parsed as compound expressions). */
}

export class OpenParen extends Caller {

    /* This class implements the open-paren delimiter, which is used for grouped expressions and
    invocations. */

    prefix(parser) {

        /* This method gathers and validates a grouped expression. */

        this.push(parser.gatherCompoundExpression(CloseParen));
        this.check({plain: true});

        return this;
    }

    infix(parser, left) {

        /* This method gathers and validates an invocation. */

        this.note("infix").push(left, parser.gatherCompoundExpression(CloseParen));
        this.check({skip: 1, plain: true});

        return this;
    }

    js(writer) {

        /* Render a grouped expression or an invocation. */

        return super.js(writer, openParen, closeParen);
    }
}

export class Pack extends PrefixOperator {

    /* This class implements the `pack` operator, which compiles to an invocation of
    `Object.preventExtensions`. */

    RBP = 1;

    js(writer) { return `Object.preventExtensions(${this.at(0).js(writer)})` }
}

export class Packed extends Operator {

    /* This class implements the `packed` operator, used by `Is` to implement the `is packed` and
    `is not packed` suffix operators, which compile to expressions using `Object.isExtensible`. */
}

export class Parameters extends Token {

    /* This class is used to group function parameters. */
}

export class Pass extends Keyword {

    /* This class implements the `pass` keyword, used to create an explicitly empty statement. */

    prefix(_) { return this }
}

export class Plus extends GeneralOperator {

    /* This class implements the `+` operators (unary and binary). */

    LBP = 11;
    RBP = 14;
}

export class Private extends ClassQualifier {

    /* This class implements private-statements, used inside classes to declare private properties,
    which are referenced using the `!` infix dot operator. */
}

export class Raise extends InfixOperator {

    /* This class implements the raise operator (`**`), which is right-associative. */

    LBP = 13;

    infix(parser, left) { return this.push(left, parser.gatherExpression(this.LBP - 1)) }
}

export class Reserved extends Word {

    /* This class implements reserved words, which always make it as far as the parser, as they are
    valid property names (so are only invalid in any other context). */

    prefix(_) { throw new LarkError(`reserved word (${this.value})`, this.location) }

    infix(_) { throw new LarkError(`reserved word (${this.value})`, this.location) }
}

export class Return extends Keyword {

    /* This class implements the `return` keyword, which is followed by an optional expression. */

    prefix(parser) {

        if (parser.on(Terminator, Closer)) return this;
        else return this.push(parser.gatherExpression());
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if it's
        anything other than a class block, and `false` if it is one. */

        return parser.check($ => $ > SIMPLEBLOCK, $ => $ < CLASSBLOCK);
    }

    js(writer) {

        /* Render a `return` statement with its optional expression. */

        const expression = this.at(0) ? space + this.at(0).js(writer) : empty;

        return `return${expression}`;
    }
}

export class RSHIFT extends InfixOperator {

    /* This class implements the `>>` infix operator (bitwise zero-shift-right). */

    LBP = 10;
}

export class Seal extends PrefixOperator {

    /* This class implements the `seal` operator, which uses `Object.seal`. */

    RBP = 1;

    js(writer) {

        return `Object.seal(${this.at(0).js(writer)})`;
    }
}

export class Sealed extends Operator {

    /* This class implements the `sealed` operator, used by `Is` to implement the `is sealed`
    suffix operation, which uses `Object.isSealed`. */
}

export class SkinnyArrow extends ArrowOperator {

    /* This class implements the skinny-arrow function operator (`->`). */
}

export class Slash extends InfixOperator {

    /* This class implements the slash operator (`/`), used for regular division. */

    LBP = 12;
}

export class Spread extends Operator {

    /* This class implements the spread-operator (`...`), which can be a prefix operator (used to
    slurp a sequence into a variable) or a suffix operator (used to splat an arbitrary expression
    into a sequence). */

    LBP = 2;

    prefix(parser) { return this.push(parser.gatherVariable()) }

    infix(_, left) { return this.push(left) }

    js(writer) { return `...${this.at(0).js(writer)}` }
}

export class Star extends InfixOperator {

    /* This class implements the star operator (`*`), used for multiplication. */

    LBP = 12;
}

export class Static extends ClassQualifier {

    /* This class implements static-statements, used inside classes. */
}

export class SubclassLiteral extends Functional {

    /* This class implements `subclass` literals, which are used to extend one class with another
    (`subclass of Foo {}` -> `class extends Foo {}`). */

    expression = true;

    prefix(parser) {

        if (not(parser.on(Of))) this.note("named").push(parser.gatherVariable());

        if (parser.on(Of)) {

            parser.advance();

            return this.push(parser.gatherExpression(), parser.gatherBlock(CLASSBLOCK));

        } else throw new LarkError("incomplete subclass", parser.advance(false).location);
    }
}

export class SuperConstant extends Constant {

    /* This class implements the `super` constant. */
}

export class Throw extends CommandStatement {

    /* This class implements the `throw` operative keyword, which implies `new` in Lark, so
    `throw Error(message)` compiles to `throw new Error(message)`. You cannot throw strings
    or anything like that. */

    expresssion = true;

    js(writer) {

        /* Render a `throw new` expression. */

        return `throw new ${this.at(0).js(writer)}`;
    }
}

export class TrueConstant extends Constant {

    /* This class implements the `true` constant. */
}

export class Var extends Declaration {

    /* This class implements `var` declarations, which compile to `let` declarations. */
}

export class Variable extends Word {

    /* This class implements variable names. */

    expression = true;

    prefix(_) { return this }
}

export class VoidConstant extends Constant {

    /* This class implements the `void` constant, which compiles to `undefined`. */
}

export class When extends Operator {

    /* This concrete class implements the when-else ternary operator (`x when y else z`),
    which is right-associative. */

    LBP = 2;
    initial = false;

    infix(parser, left) {

        /* Gather and validate a ternary expression. */

        this.push(left, parser.gatherExpression());

        if (parser.on(Else)) parser.advance();
        else throw new LarkError("incomplete when-operation", this.location);

        return this.push(parser.gatherExpression(this.LBP - 1));
    }

    js(writer) {

        return `${this.at(1).js(writer)} ? ${this.at(0).js(writer)} : ${this.at(2).js(writer)}`;
    }
}

export class While extends PredicatedBlock {

    /* This class implements the `while` keyword. */

    notes = [LOOPBLOCK];
}

export class XOR extends InfixOperator {

    /* This class implements the Lark `||` infix operator (bitwise XOR). */

    LBP = 6;
}

export class Yield extends Keyword {

    /* This class implements the `yield` and `yield from` prefix-operators, which both begin
    formal statements (while also being a valid expressions). */

    LBP = 2;
    expression = true;

    prefix(parser) {

        /* Gather an *optional* expression, unless the parser is on `from`. In that case, gather
        the `from` keyword, then gather a *required* expression. */

        if (parser.on(From)) {

            parser.advance();

            return this.note("yield-from").push(parser.gatherExpression());

        } else if (not(parser.on(Terminator, Closer))) return this.push(parser.gatherExpression());
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if it is
        anything other than a class block, and `false` if it is one. */

        return parser.check($ => $ > SIMPLEBLOCK, $ => $ < CLASSBLOCK);
    }

    js(writer) {

        /* Render a `yield` expression with its optional operand, or a `yield from` expression with
        its required operand. */

        if (this.notes.includes("yield-from")) return `yield * ${this.at(0).js(writer)}`;

        const expression = this.at(0) ? space + this.at(0).js(writer) : empty;

        return `yield${expression}`;
    }
}
