/* This module (`abstract.js`) defines all of the abstract base classes in the token hierarchy,
leaving `concrete.js` to define all of the concrete token types. Together, they form a single,
object-oriented, token hierarchy that implements all of the language specifics.

Originally, `abstract.js` and `concrete.js` were one file, but it got too big to navigate around
effectively, so it was split in two. It's generally obvious whether a given type is abstract or
concrete, so it should be much easier to find token-types going forward.

Dividing the original file in two created many circular dependencies. To avoid chaos, this file
imports everything it needs, and exports everything it defines, though its exports are only ever
imported by `concrete.js`, which re-exports everything. All other modules import everything they
need via `concrete.js`, so the circular dependencies never lead to conflicts. */

// this module implements all of the concrete classes in the token hierarchy...

import { LarkError } from "../core/error.js"
import { put, not, iife } from "../core/helpers.js"
import { constants, keywords, operators, reserved } from "./spellings.js"

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

import {
    LOOPBLOCK,
    SIMPLEBLOCK,
    FUNCTIONBLOCK,
    ASYNCFUNCTIONBLOCK,
    CLASSBLOCK,
} from "../core/blocktypes.js"

export class Token extends Array {

    /* This is the abstract base class for all other token classes. Internally, Lark reuses the
    same classes for the tokens (in the token stream) and the AST nodes (in the abstract syntax
    tree). This approach makes Pratt parsing a lot easier, and permits a single OOP hierarchy.

    This file (`user/abstract.js`) contains all of the abstract base classes in the hierarchy.
    The concrete subclasses are all defined inside `user/concrete.js`.

    Everything a token instance needs is defined by this class, with useful defaults, including
    various properties, five API methods (that are used by the compiler stages), and a couple
    of helper methods that are only used internally (by the API methods).

    The compiler stages are relatively simple functions that each maintain some local state, as
    well as defining a high-level, declarative API, based around a handful of functions that
    share that state. Those functions (known as *API functions*) are bound to an object to
    form an API that can be passed around freely.

    The five *API methods* (`token`, `prefix`, `infix`, `validate` and `js`) take a reference to
    the corresponding API object (`lexer`, `parser` or `writer`) as their first argument, and
    are individually documented below (within the default implementations), along with the
    helper methods.

    Every token defines or inherits `LBP` and `RBP` properties (short for *left-binding-power*
    and *right-binding-power* respectively), refered to hereafter as *LBP* and *RBP*.

    LBP is required by the Parser Stage API, and is used by Pratt's Algorithm to implement operator
    precedence. RBP is only used within this object hierarchy (it is not required by any API).

    Tokens that should not have any precedence (plain keywords, terminals, terminators etc) must
    have an LBP of zero, so they can just inherit their binding-powers from this class.

    Tokens that implement an `infix(parser)` method (infix and suffix operators) must set LBP to
    the precedence of that operator, and will reuse the same value in any recursive calls to the
    Parser Stage API (when gathering their righthand operand), unless they are right-associtive,
    where they pass `LBP - 1` instead.

    Tokens that implement a `prefix(parser)` method (prefix operators) are not *required* to set
    a binding-power property, but will need to pass the precedence of the operator in recursive
    calls to the Parser Stage API. RBP is used to store that binding-power, as there are many
    tokens that implement both prefix operators and infix/suffix operators. */

    LBP = 0;                // left-binding-power (infix-operator precedence)
    RBP = 0;                // right-binding-power (prefix-operator precedence)

    notes = [];             // notes regarding variations (some tokens implement many operators)
    compile = true;         // whether to compile the statement to JS (used by `dev` mode)
    expression = false;     // whether the token forms a node which is a valid expression

    get spelling() {

        /* This API property specifies the JavaScript spelling for the given token, defaulting
        to the Lark spelling.

        Note: While `spelling` is a computed (instance) property here, it is often overridden
        by a simple stored property. */

        return this.value;
    }

    constructor(location, value=empty) {

        /* This constructor takes and initializes the two remaining properties (`location` and
        `value`) that combine with the five properties defined already to create the complete
        set of properties that all token instances require.

        Note: While a couple of the subclasses define helper methods for their own use, token
        instances have the same shape (the same properties). This is why we push the various
        AST nodes to a generic array of operands, instead of letting each subclass define
        its own properties (`lvalue`, `predicate`, `block` etc). */

        super();
        this.location = location;  // see `locate` in `lexer.js` and `LarkError` above
        this.value = value;        // the value of the token (taken from the source)
    }

    static * token(lexer) { // api method

        /* This method instantiates tokens, based on the source. It is the only API method that
        is invoked by the Lexer Stage. It is static as it wraps the constructor method to allow
        it to yield zero or more tokens (potentially including instances of a subclass) to the
        token stream. This naturally requires that `token(lexer)` is generator too.

        This method is invariably redefined by the lower classes, so the default implementation
        does not yield any tokens or anything. It is just here for consistency. */    
    }

    prefix(parser, context=undefined) { // api method

        /* This method takes a reference to the Parser API and an optional `context`. The method
        just throws an exception. However, tokens that're valid in the prefix position can define
        their own implementations that use the Parser API to parse the token stream (pushing any
        results to the token's operands), before returning the resulting AST node (which will
        normally just be `this` (which is also returned by `note` and `push`)).

        API methods that invoke `parser.gather` or `parser.gatherExpression` (on subexpressions)
        can optionally pass a `context` to those API functions (as well as a binding-power) and
        that context will be passed to the `prefix` method of the following token. This is used
        by `Async` to let `FunctionLiteral` know the context (which it needs to validate `await`
        statements). */

        throw new LarkError("invalid token (in prefix position)", this.location);
    }

    infix(parser, prefix) { // api method

        /* This method is very similar to `prefix` above, except that it is invoked on tokens in
        the infix position (with something before them), and takes its left operand (as an AST
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

        /* This chainable helper is used by `prefix` and `infix` methods to push zero or more
        operands to the current instance. */

        args.forEach(arg => super.push(arg));

        return this;
    }

    note(...args) { // internal helper

        /* This chainable helper is used by `prefix` and `infix` methods to push zero or more notes
        to the `notes` array for the current instance. */

        args.forEach(arg => this.notes.push(arg));

        return this;
    }

    is(...Classes) { // internal helper

        /* Take any number of token classes, and return `true` if `this` is an instance of one
        of the classes (or one of their subclasses), and return `false` otherwise. */

        for (const Class of Classes) if (this instanceof Class) return true;

        return false;
    }
}

export class Terminal extends Token {

    /* This is the base class for all terminal tokens (including the various words, literals and
    terminators - anything that only uses a single token). */
}

export class Terminator extends Terminal {

    /* This is the base class for the various statement-terminators (line-feeds, commas and the
    implicit End Of File token inserted at the end of every token stream). It is also imported
    by the Lexer Stage to tokenize terminators, and by the Parser Stage to classify them. */

    static * lex(lexer, location) {

        /* Check which token the Lexer Stage is on, then instantiate and yield an instance of
        the corresponding type (one of `LineFeed`, `Comma` or `EOF`). */

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
    parens and opening brackets. */

    LBP = 17;
    expression = true;

    validate(_) { return true }
}

export class Word extends Terminal {

    /* This is the abstract base class for every type of word and name. It is also imported by the
    Lexer Stage for word-tokenization. */

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

export class Constant extends Word {

    /* This is the abstract base class for constant words, used for named numbers (like `Infinity`
    and `NaN`), as well as magic variables (`this`, `default`, `arguments`, `random`, `void`, and
    `true` etc). It is also used (internally) by the `Word` class (see `subclass`). */

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
    practice, `break` and `continue`), which accept an optional (`Variable`) label. */

    prefix(parser) {

        /* If the parser is on a valid label, this method gathers it, continuing without
        operands otherwise. */

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

    /* This is the abstract base class for the `static` and `private` qualifiers that
    prefix declarations inside classes. */

    prefix(parser) {

        /* When present, gather an optionally asynchronous function, and parse a regular
        declaration otherwise. */

        if (parser.on(Async, FunctionLiteral)) return this.push(parser.gather());
        else return super.prefix(parser);
    }

    validate(parser) {

        /* This method checks that the statement is directly contained by the body of a
        class. */

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

        const blocktype = context instanceof Do ? FUNCTIONBLOCK : this.notes.at(0);

        return this.push(parser.gatherExpression(), parser.gatherBlock(blocktype));
    }

    js(writer) {

        return `${this.value} (${this.at(0).js(writer)}) ${writer.writeBlock(this.at(1))}`;
    }
}

export class Operator extends Token {

    /* This is the abstract base class for all of the operator subclasses. It is used by the
    lexer to gather an unbroken sequence of our symbolic operator characters, split it into
    individual operators (see `slice`), then yield the operators as concrete tokens, one
    at a time. */

    expression = true;

    static slice(value, offset, location) {

        /* Take a string (`value`) containing a contiguous sequence of symbolic operator
        characters, and break it into an array of individual operators, greedily, from
        left to right, based on a given `offset` (into the string) and `location`.
        
        Note: The location is only needed to generate synax errors. */

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

        /* This API method yields as many operators as it can gather, see `Operator.slice`. */

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

export class PrefixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as prefix
    operators. */

    prefix(parser) {

        return this.push(parser.gatherExpression(this.RBP));
    }

    js(writer) {

        return `${this.spelling} ${this.at(0).js(writer)}`;
    }
}

export class InfixOperator extends Operator {

    /* This base class provids functionality for operators that are only valid as infix
    operators. */

    initial = false;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP));
    }

    js(writer) {

        return `${this.at(0).js(writer)} ${this.spelling} ${this.at(1).js(writer)}`;
    }
}

export class DotOperator extends InfixOperator {

    /* This base class provides functionality for dot-operators, which must have a property
    name (which can be any type of word token) on the rightside. */

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

    /* This is the base class for operators that have prefix and infix forms. */

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

export class NumberLiteral extends Terminal {

    /* This is the concrete class for all number-literals (integers and floats, with or without a
    unit-suffix). It is also imported by the Lexer Stage for number tokenization.

    Units must immediately follow the last digit, and always consist of one or more alphas. Units
    can use binary or hexadecimal notation, but they are not compatible with exponentiation.

    Note: Datetime related units (`hrs`, `mins`, `s`, `ms` etc) are reserved while the `Temporal`
    proposal matures.

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
        parens). */

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

        if (lexer.at(alphas)) { // numeric units...

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

    /* This is the concrete class for string-literals and ther base class for text-literals. It is
    also imported by the Lexer Stage for tokenizing string and text literals.

    Lark always compiles its literals to the equivalent JS template-literals (using backticks).

    Note: Lark string and text literals can appear in an infix position (following some arbitrary
    expression). In that case, they have a binding power of `16`, and the result compiles to the
    equivalent tagged-template-literal. However, unlike JavaScript, Lark accepts spaces between
    the expression and the literal that follows it (and recommeds using one space).

    Characters that need escaping (grave accents, dollars followed by opening braces etc) are all
    escaped by the lexer stage (see `static * lex` below). */

    LBP = 16;
    expression = true;

    static * lex(lexer, location) {

        /* Lex and yield a string literal as a token stream that contains a `StringLiteral`, plus
        for every interpolation, an `OpenInterpolation` token, followed by each token with the
        interpolation, then a `CloseInterpolation` token, followed by another `StringLiteral`.

        Note: While a `StringLiteral` may or may not be followed by an interpolation-sequence, any
        such sequence will always be followed by a `StringLiteral` (even if it's empty), as every
        interpolation is considered to be preceded and followed by a substring.

        Note: While interpolations can be nested (like any other compound expression), that will not
        affect the pattern outlined above: The initial substring will always be followed by zero or
        more pairs that each contain an interpolation sequence followed by a substring.

        This approach allows string-interpolations to be parsed like any other compound expression,
        which is important for consistency (with whitespace rules etc). */

        function match(character, value=empty) {

            /* Take a character, and an optional value string (defaulting to empty), and gather up
            any number of contiguous instances of the given character, concatenating them to the
            value, then return the result. */

            while (lexer.at(character) && lexer.advance()) value += lexer.read();

            return value;
        }

        const [head, characters] = [match(quote, quote), []];
        const StringClass = head.length > 1 ? TextLiteral : StringLiteral;

        lexer.advance();

        while (lexer.read()) {

            // this loop can yield any number of tokens, as it yields every token within each
            // interpolation, plus one or more substrings...

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

                } else characters.push(candidate);      // not enough quotes to close the string

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
            this.push(parser.gatherCompoundExpression(CloseInterpolation), parser.advance(true));
        }

        return this;
    }

    infix(parser, left) {

        /* Take a prefix expression, push it to the token's operands, then defer parsing to the
        `prefix(parser)` method to parse the actual string literal. */

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

    /* This concrete class implements text literals, inheriting a lot from `StringLiteral`. */

    js(writer) {

        /* Generate a template literal from the various parts of the text literal, reproducing
        the interpolations, and possibly including a tag-expression. */

        const strip = string => string.replaceAll(indentation, newline);

        const self = this, indentation = iife(function() {

            /* This IIFE computes the indentation to remove from each line (as a string containing
            a newline, followed by zero or more spaces), returning it, so it becomes `indentation`
            in the out scope. */

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

        // remove superfluous leading newline (from replacing indentation with a newline)...

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

    /* This concrete class implements the logical `and` operator, which compiles to `&&`. */

    LBP = 4;
    spelling = "&&";
}

export class AND extends InfixOperator {

    /* This concrete class implements the `&` infix operator (bitwise AND). */

    LBP = 7;
}

export class ARSHIFT extends InfixOperator {

    /* This concrete class implements the `>>>` infix operator (bitwise arithmetic-shift-right). */

    LBP = 10;
}

export class As extends PrefixOperator {

    /* This concrete class implements the `as` prefix-operator, used to specify prototypes in object
    literals: `let oo = {as Object, x: 1, y: 2}` */

    LBP = Infinity;

    js(writer) {

        return `__proto__: ${this.at(0).js(writer)}`;
    }
}

export class Ask extends GeneralDotOperator {

    spelling = "?.";

    /* This concrete class implements the `?` operator, which compiles to a `Math.clz32`
    invocation in a prefix context, and `?.` in an infix context. */
}

export class Assert extends Keyword {

    /* This conrete class provides the assert-keyword, currently only used within import
    assertions. */
}

export class Assign extends AssignmentOperator {

    /* This concrete class implements the assignment infix operator (`=`). */
}

export class AssignAND extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the bitwise AND
    operator (`&=`). */
}

export class AssignARSHIFT extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the arithmetic
    right-shift operator (`>>>=`). */
}

export class AssignFloor extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the floor-division
    operator (`\=`). */
}

export class AssignPlus extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the addition
    operator (`+=`). */
}

export class AssignLSHIFT extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the left-shift
    operator (`<<=`). */
}

export class AssignMinus extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the subtraction
    operator (`-=`). */
}

export class AssignModulo extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the modulo
    operator (`%=`). */
}

export class AssignOR extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the bitwise OR
    operator (`|=`). */
}

export class AssignRaise extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the exponentiation
    operator (`**=`). */
}

export class AssignRSHIFT extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the right-shift
    operator (`>>=`). */
}

export class AssignSlash extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the sane-division
    operator (`/=`). */
}

export class AssignStar extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the multiplication
    operator (`-=`). */
}

export class AssignXOR extends AssignmentOperator {

    /* This concrete class implements the inplace-assignment version of the bitwise XOR
    operator (`||=`). */
}

export class Async extends Functional {

    /* This concrete class implements the `async` function-qualifier. */

    expression = true;

    prefix(parser) {

        /* This method checks that the next token is a function literal, and if so, gathers
        it as an operand, complaining otherwise. */

        if (parser.on(FunctionLiteral)) return this.push(parser.gather(0, this));
        else throw new LarkError("`async` qualifier without `function` keyword", this.location);
    }

    js(writer) {

        /* Render the function operand with the `async` qualifier. */

        return `async ${this.at(0).js(writer)}`;
    }
}

export class Await extends CommandStatement {

    /* This concrete class implements the `await` operator, used to await promises. */

    RBP = 14;
    expression = true;

    prefix(parser) {

        /* This method gathers await-expressions. */

        return this.push(parser.gatherExpression(this.RBP));
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true` if
        it is an asynchronous function block, and `false` otherwise. If nothing functional
        is found, the validation *succeeds* (note the third argument), as top-level await
        is valid (unlike all other other such cases). */

        return parser.check($ => $ > SIMPLEBLOCK, $ => ASYNCFUNCTIONBLOCK, true);
    }

    js(writer) {

        /* Render an `await` expression. */

        return `await ${this.at(0).js(writer)}`;
    }
}

export class Bang extends GeneralDotOperator {

    spelling = ".#";

    /* This concrete class implements the `!` operator, which compiles to the bitwise `~`
    operator in a prefix context, and `.#` in an infix context. */
}

export class Block extends Token {

    /* Used to group statements into a block. */
}

export class Break extends BranchStatement {

    /* This concrete class implements the `break` statement, which is just like JS. */

    validate(parser) {

        /* Labled break-statements are valid inside any control-flow block, so we can
        simply check the immediate context to establish that it is not the body of
        anything functional, while also ensuring the label is active.

        Unlabeled break-statements are only valid inside a loop, though they may be
        nested inside simple blocks within the loop. Therefore, we handle unlabled
        breaks by climbing the stack, ignoring any simple blocks, and checking
        that the first non-simple block is a loop. */

        if (this.notes.includes("label")) {

            if (not(parser.label(this.at(0)))) {

                throw new LarkError(`undefined label '${this.at(0).value}'`, this.at(0).location);
            }

            return parser.check($ => true, $ => $ < FUNCTIONBLOCK);

        } else return parser.check($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }
}

export class ClassLiteral extends Functional {

    /* This concrete class implements `class` literals, which cannot extend anything,
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

export class CloseInterpolation extends Opener {
    
    /* This concrete class implements the `)` delimiter within string interpolations, and is
    used by `StringLiteral` (allowing interpolations to be parsed as compound expressions). */
}

export class CloseParen extends Closer {

    /* This concrete class implements the `)` delimiter, used for closing group expressions,
    arrow params and function invocations. */
}

export class CompoundExpression extends Token {

    /* Used to group the contents of various compound expressions into an AST node. */
}

export class Label extends InfixOperator {

    /* This concrete class implements the `:` pseudo-operator, used for delimiting key-value
    pairs in object expressions, and for labels on blocks. */

    LBP = 1;

    infix(parser, left) {

        /* If this colon is part of a label, update the parser to note whether the label is
        bound to a loop statement or a simple block statement. Then, gather the statement,
        before nullifying the label. Otherwise, just treat the colon like a normal infix
        operator. */

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

        return `${this.at(0).js(writer)}: ${this.at(1).js(writer)}`;
    }
}

export class Comma extends Terminator {

    /* This concrete class implements the `,` delimiter, used for delimiting expressions in
    groups, invocations and compound literals, as well as params, declarations etc. */
}

export class Continue extends BranchStatement {

    /* This concrete class implements the `continue` statement, just like JavaScript. */

    validate(parser) {

        /* A continue-statement is only valid inside a loop (whether the statement has a
        label or not), though it may be nested inside simple blocks within the loop.
        
        If there is a label, ensure it is active and bound to a loop, then in either case,
        climb the stack, ignoring simple blocks, and check that the first non-simple block
        is a loop. */

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

    /* This concrete class implements the `debug` statement, which compiles to `debugger`. */

    spelling = "debugger";

    prefix(_) { return this }
}

export class Delete extends CommandStatement {

    /* This concrete class implements the `delete` operator, exactly like JavaScript. */

    LBP = 14;
    expression = true;

    expresssion = true;

    js(writer) {

        /* Render a `delete` expression. */

        return `delete ${this.at(0).js(writer)}`;
    }
}

export class Dev extends Keyword {

    /* This concrete class implements the `dev` qualifier, which can prefix any statement,
    formal or informal, ensuring it will only be included in the compiled JavaScript when
    the `dev` flag (which is `false` by default) is truthy. */

    prefix(parser) {

        /* Gather an arbitrary statement, while copying the `dev` flag to the `compile`
        property, so this statement can be omitted from the output correctly (without
        the semicolon we would get if `js(writer)` simply returned an empty string). */

        this.compile = parser.dev;

        return this.push(parser.gather());
    }

    js(writer) {

        /* Render the only operand and return the result, leaving it to the writer stage to
        omit the result when the `compile` property (of `this` instance) is falsey. */

        return this.at(0).js(writer);
    }
}

export class Do extends PrefixOperator {

    /* This concrete class implements the `do` keyword, which prefixes blocks and functions to
    create IIFEs (which Lark uses as a more flexible version of block-statements). */

    prefix(parser) {

        /* If the next token is `async` or `function`, make this instance valid as an expression,
        then gather whatever follows. Otherwise, gather a control-flow block. */

        if (parser.on(If, While, For, Functional)) return this.push(parser.gather(0, this));
        else return this.push(parser.gatherBlock(FUNCTIONBLOCK));
    }

    js(writer) {

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

    /* This concrete class implements the dot operator (`.`), which is used for property access.
    The points in number literals do not use this class. */
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

    /* Used to represent an empty slot in an unpacking assignment (just a comma). */
}

export class Equal extends InfixOperator {

    /* This concrete class implements the `==` operator, which compiles to `Object.is`. */

    LBP = 8;

    js(writer) {

        return `Object.is(${this.at(0).js(writer)}, ${this.at(1).js(writer)})`;
    }
}

export class EOF extends Terminator {

    /* This concrete class implements the implicit End Of File token that is appended
    to every token stream, and used by the parser to check for the end of the file. */
}

export class Export extends Keyword {

    /* This conrete class implements the export-statement. */
}

export class FalseConstant extends Constant {

    /* This concrete class implements the `false` constant. */
}

export class FatArrow extends ArrowOperator {

    /* This concrete class implements the fat-arrow function-operator (`=>`). */
}

export class Floor extends InfixOperator {

    /* This concrete class implements the floor-division infix operator (`\`). */

    LBP = 12;

    js(writer) {

        return `Math.floor(${this.at(0).js(writer)} / ${this.at(1).js(writer)})`;
    }
}

export class For extends Header {

    /* This concrete class implements for-loops (old school for-loops have not been
    designed yet, but will be added in some form). */

    prefix(parser, context=undefined) {

        /* This method parses all four for-loops. It uses `parser.gatherAssignee` to
        avoid parsing the operator (`in`, `of`, `on` or `from`) as an infix. */

        const blocktype = context instanceof Do ? FUNCTIONBLOCK : LOOPBLOCK;

        this.push(parser.gatherAssignee());

        if (parser.on(In, Of, On, From)) this.note(parser.advance(true).value);
        else throw new LarkError("incomplete for-statement", this.location);

        return this.push(parser.gatherExpression(), parser.gatherBlock(blocktype));
    }

    js(writer) {

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

    /* This concrete class implements the `freeze` operator, which compiles to an invocation
    of `Object.freeze`. */

    RBP = 1;

    js(writer) {

        return `Object.freeze(${this.at(0).js(writer)})`;
    }
}

export class From extends Keyword {

    /* This concrete class implements the `from` keyword, used by import and export
    statements. */
}

export class Frozen extends Operator {

    /* This concrete class implements the `frozen` operator, used by `Is` to implement the
    `is frozen` suffix operation, which compiles to an `Object.isFrozen` invocation. */
}

export class FunctionLiteral extends Functional {

    /* This is the concrete class for function literals. */

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

        /* Render a function literal (not handling arrow grammar). */

        let name = empty;

        // having assumed that `this.at(0)` is a `SkipAssignee` (indicating an anonymous function),
        // now check if it is a `CompoundExpression` (indicating a computed name) or a `Variable`
        // (indicating a regular named-function), and update `name` accordingly...

        if (this.at(0) instanceof CompoundExpression) {

            name = space + openBracket + this.at(0).at(0).js(writer) + closeBracket;

        } else if (this.at(0) instanceof Variable) name = space + this.at(0).js(writer);

        // prerender the keyword, parameters and the function body, then interpolate them into
        // a literal, and return the result...

        const keyword = "function" + (this.notes.includes("generator") ? asterisk : empty);
        const params = this.at(1).map(param => param.js(writer)).join(comma + space);
        const block = writer.writeBlock(this.at(2));

        return `${keyword}${name}(${params}) ${block}`;
    }
}

export class Greater extends InfixOperator {

    /* This concrete class implements the greater-than operator (`>`). */

    LBP = 9;
}

export class If extends PredicatedBlock {

    /* This concrete class implements if-statements. */

    notes = [SIMPLEBLOCK];
}

export class Import extends Keyword {

    /* This conrete class implements the import-statement, with its various grammars. */
}

export class In extends InfixOperator {

    /* This concrete class implements the `in` infix-operator, which is used for membership
    tests on collections of any type. */

    LBP = 8;

    js(writer) {

        const operand = this.at(0).js(writer);
        const register = writer.register(this.at(1));

        return `(${register}?.ƥvalues?.() ?? Object.values(${register} ?? [])).includes(${operand})`;
    }
}

export class InfinityConstant extends Constant {

    /* This concrete class implements the `Infinity` floating-point constant. */
}

export class Is extends InfixOperator {

    /* This concrete class implements the `is`, `is not`, `is packed`, `is sealed`,
    `is frozen`, `is not packed`, `is not sealed` and `is not frozen` operators. */

    LBP = 8;

    infix(parser, left) {

        this.push(left);

        if (parser.on(Not)) { this.push(parser.advance(true)) }

        if (parser.on(Packed, Sealed, Frozen)) return this.push(parser.advance(true));
        else return this.push(parser.gatherExpression(this.LBP));
    }

    js(writer) {

        /* Render an `is` or `is not` infix-operation, or an `is packed`, `is not packed`,
        `is sealed`, `is not sealed`, `is frozen` or `is not frozen` suffix-operation.
        
        Note: All of the operators that begin with `is` return a bool, so it's unlikely that
        they would ever form a standalone expression-statement (which is why we ignore the
        context of the `writer`).

        TODO: Implement the `is of` and `is not of` operators (which also return bools). */

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

    /* This is the concrete class for the less-than-operator (`<`). */

    LBP = 9;
}

export class Let extends Declaration {

    /* This concrete class implements let-statements, which compile to const-statements. */
}

export class LineFeed extends Terminator {

    /* This concrete class implements the line-feed characters, used to define newlines,
    which may or may not be significant, depending on the current LIST state, which is
    maintained by the parser implicitly (removing line-feed instances from the token
    stream as required). */

    prefix(parser) {

        throw new LarkError("unexpected newline", this.location);
    }

    infix(parser) {

        throw new LarkError("unexpected newline", this.location);
    }
}

export class LSHIFT extends InfixOperator {

    /* This concrete class implements the `<<` infix operator (bitwise zero-shift-left). */

    LBP = 10;
}

export class Minus extends GeneralOperator {

    /* This concrete class implements the unary and binary minus operators (`-`). */

    LBP = 14;
    RBP = 11;
}

export class Modulo extends InfixOperator {

    /* This concrete class implements the modulo infix operator (`%`). */

    LBP = 12;
}

export class NaNConstant extends Constant {

    /* This concrete class implements the `NaN` floating-point constant. */
}

export class New extends PrefixOperator {

    /* This concrete class implements the new-operator, which copies JavaScript, including
    the way `new` applies to invocations as a special-case of expression. */

    RBP = 17;
}

export class Not extends GeneralOperator {

    /* This concrete class implements the `not` prefix operator, and the `not in` infix
    operator. */

    LBP = 9;
    RBP = 14;
    spelling = "!";

    infix(parser, left) {

        if (parser.on(In)) {

            parser.advance(true);

            return this.note("infix").push(left, parser.gatherExpression(this.LBP));

        } else throw new LarkError("unexpected not-operator", this.location);
    }

    js(writer) {

        if (this.notes.includes("infix")) {

            const register = writer.register(this.at(1));

            return `!((${register}?.ƥvalues?.() ?? Object.values(${register} ?? [])).includes(${this.at(0).js(writer)}))`;

        } else return super.js(writer);
    }
}

export class NotEqual extends InfixOperator {

    /* This concrete class implements the `!=` operator, which compiles to `!Object.is`. */

    LBP = 8;

    js(writer) {

        return `!Object.is(${this.at(0).js(writer)}, ${this.at(1).js(writer)})`;
    }
}

export class NotGreater extends InfixOperator {

    /* This concrete class implements our not-greater-than (less-than-or-equal-to) infix
    operator (`<=`). */

    LBP = 9;
}

export class NotLesser extends InfixOperator {

    /* This concrete class implements our not-less-than (greater-than-or-equal-to) infix
    operator (`>=`). */

    LBP = 9;
}

export class NullConstant extends Constant {

    /* This concrete class implements the `null` constant. */
}

export class Nullish extends GeneralOperator {

    /* This concrete class implements the infix nullish operator (`??`). It also handles
    pairs of clz32-operators (`?`) in a prefix position (this is an edgecase, where the
    operator parser requires further disambiguation). */

    LBP = 3;
    RBP = 14;
}

export class Of extends InfixOperator {

    /* This concrete class implements the infix of-operator, which is also used by functions
    to prefix their (optional) arguments, as well as `for-of` loops. */

    LBP = 8;
}

export class On extends InfixOperator {

    /* This concrete class implements the `on` operator, which compiles to the JS `in` operator. */

    LBP = 9;
    spelling = "in";
}

export class Or extends InfixOperator {

    /* This concrete class implements the logical-or operator, which compiles from `or` to `||`. */

    LBP = 3;
    spelling = "||";
}

export class OR extends InfixOperator {

    /* This concrete class implements the bitwise-or operator, which uses `|`, like JavaScript. */

    LBP = 5;
}

export class OpenBrace extends Opener {

    /* This concrete class implements the open-brace delimiter, which is used for blocks and object
    expressions. */

    expression = true;

    prefix(parser) {

        /* This method gathers an object expression, noting if it specifies a prototype by setting
        the `value` string to "__proto__". */

        this.push(parser.gatherCompoundExpression(CloseBrace));

        if (this.check({proto: true}).prototyped) this.value = "__proto__";

        return this;
    }

    validate(_) { return true }

    js(writer) {

        /* Output a JS object literal. Infer a `null` prototype, unless it contains a
        `(Prototype)` expression (indicated by having a `value` of "__proto__"). */

        const head = this.value === "__proto__" ? openBrace : "{__proto__: null, ";
        const body = this.at(0).map(operand => operand.js(writer)).join(comma + space);

        return head + body + closeBrace;
    }
}

export class OpenBracket extends Caller {

    /* This concrete class implements the open-bracket delimiter, which is used for array
    expressions and bracket-notation. */

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

        /* Output a JS array literal or bracket notation. */

        return super.js(writer, openBracket, closeBracket);
    }
}

export class OpenInterpolation extends Opener {
    
    /* This token type is used by `StringLiteral` for delimiting the tokens within string
    interpolations (allowing them to be parsed as compound expressions). */
}

export class OpenParen extends Caller {

    /* This concrete class implements the open-paren delimiter, which is used for grouped
    expressions and invocations. */

    prefix(parser) {

        /* This method gathers a grouped expression. */

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

        /* Output a grouped expression or an invocation. */

        return super.js(writer, openParen, closeParen);
    }
}

export class Pack extends PrefixOperator {

    /* This concrete class implements the `pack` operator, which compiles to an invocation
    of `Object.preventExtensions`. */

    RBP = 1;

    js(writer) {

        return `Object.preventExtensions(${this.at(0).js(writer)})`;
    }
}

export class Packed extends Operator {

    /* This concrete class implements the `packed` operator, used by `Is` to implement the
    `is packed` and `id not packed` suffix operators, which compile to expressions using
    `Object.isExtensible` (which equates to is-not-packed). */
}

export class Parameters extends Token {

    /* Used to group function parameters. */
}

export class Pass extends Keyword {

    /* This concrete class implements the `pass` keyword, used to create an explicitly
    empty statement. */

    prefix(_) { return this }
}

export class Plus extends GeneralOperator {

    /* This concrete class implements the `+` operator (prefix and infix). */

    LBP = 11;
    RBP = 14;
}

export class Private extends ClassQualifier {

    /* This contrete class implements private-statements, used inside classes. */
}

export class Raise extends InfixOperator {

    /* This concrete class implements the exponentiation infix operator, which is right-
    associative. */

    LBP = 13;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP - 1));
    }
}

export class Reserved extends Word {

    /* This class implements reserved words, which always make it as far as the parser,
    as they are valid property names (so only invalid in any other context). */

    prefix(_) {

        throw new LarkError(`reserved word (${this.value})`, this.location);
    }

    infix(_) {

        throw new LarkError(`reserved word (${this.value})`, this.location);
    }
}

export class Return extends Keyword {

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

    js(writer) {

        /* Render a `return` statement with its optional expression. */

        const expression = this.at(0) ? space + this.at(0).js(writer) : empty;

        return `return${expression}`;
    }
}

export class RSHIFT extends InfixOperator {

    /* This concrete class implements the `>>` infix operator (bitwise zero-shift-right). */

    LBP = 10;
}

export class Seal extends PrefixOperator {

    /* This concrete class implements the `seal` operator, which compiles to an invocation
    of `Object.seal`. */

    RBP = 1;

    js(writer) {

        return `Object.seal(${this.at(0).js(writer)})`;
    }
}

export class Sealed extends Operator {

    /* This concrete class implements the `sealed` operator, used by `Is` to implement the
    `is sealed` suffix operation, which compiles to an `Object.isSealed` invocation. */
}

export class SkinnyArrow extends ArrowOperator {

    /* This concrete class implements the skinny-arrow function operator (`->`). */
}

export class Slash extends InfixOperator {

    /* This concrete class implements the slash operator (`/`), used for (sane) division. */

    LBP = 12;
}

export class Spread extends Operator {

    /* This concrete class implements the spread suffix-operator (`...`). */

    LBP = 2;

    infix(_, left) {

        return this.push(left);
    }
}

export class Star extends InfixOperator {

    /* This concrete class implements the star operator (`*`), used for multiplication. */

    LBP = 12;
}

export class Static extends ClassQualifier {

    /* This contrete class implements static-statements, used inside classes. */
}

export class SubclassLiteral extends Functional {

    /* This concrete class implements `subclass` literals, which are used to extend one
    class with another (`subclass of Foo` -> `class extends Foo`). */

    expression = true;

    prefix(parser) {

        if (not(parser.on(Of))) this.push(parser.gatherVariable());

        if (parser.on(Of)) {

            parser.advance();

            return this.push(parser.gatherExpression(), parser.gatherBlock(CLASSBLOCK));

        } else throw new LarkError("incomplete subclass", parser.advance(true).location);
    }
}

export class SuperConstant extends Constant {

    /* This concrete class implements the `super` constant. */
}

export class Throw extends CommandStatement {

    /* This concrete class implements the `throw` statement, which implies `new` in Lark,
    so `throw t` compiles to `throw new t` (you cannot throw strings etc). */

    expresssion = true;

    js(writer) {

        /* Render a `throw new` expression. */

        return `throw new ${this.at(0).js(writer)}`;
    }
}

export class TrueConstant extends Constant {

    /* This concrete class implements the `true` constant. */
}

export class Var extends Declaration {

    /* This concrete class implements var-statements, which compile to let-statements. */
}

export class Variable extends Word {

    /* This concrete class implements variable names. */

    expression = true;

    prefix(_) { return this }
}

export class VoidConstant extends Constant {

    /* This concrete class implements the `void` constant, which compiles to `undefined`. */
}

export class When extends Operator {

    /* This concrete class implements the when-else ternary operator (`x when y else z`),
    which is right-associative. */

    LBP = 2;
    initial = false;

    infix(parser, left) {

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

    /* This concrete class implements the `while` keyword. */

    notes = [LOOPBLOCK];
}

export class XOR extends InfixOperator {

    /* This concrete class implements the `||` infix operator (XOR). */

    LBP = 6;
}

export class Yield extends Keyword {

    /* This concrete class implements the `yield` and `yield from` prefix-operators, which both
    begin formal statements (while also being a valid expressions). */

    LBP = 2;
    expression = true;

    prefix(parser) {

        /* Gather an *optional* expression, unless the parser is on `from`. In that case,
        gather the `from` keyword, and then gather the *required* expression. */

        if (parser.on(From)) {

            parser.advance(true);

            return this.note("yield-from").push(parser.gatherExpression());

        } else if (not(parser.on(Terminator, Closer))) return this.push(parser.gatherExpression());
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true`
        if it is anything other than a class block, and `false` if it is one. */

        return parser.check($ => $ > SIMPLEBLOCK, $ => $ < CLASSBLOCK);
    }

    js(writer) {

        /* Render a `yield` expression with its optional operand, or a `yield from` expression
        with its required operand. */

        if (this.notes.includes("yield-from")) return `yield * ${this.at(0).js(writer)}`;

        const expression = this.at(0) ? space + this.at(0).js(writer) : empty;

        return `yield${expression}`;
    }
}
