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

import { put, not } from "../core/helpers.js"

import { LarkError } from "../core/error.js"

import {
    alphas,
    closeBrace,
    closeBracket,
    closeParen,
    comma,
    empty,
    lowers,
    newline,
    openBrace,
    openBracket,
    openParen,
    quote,
    space,
    symbolics,
    wordCharacters,
} from "../core/ascii.js"

import { CLASSBLOCK } from "../core/blocktypes.js"

import { constants, keywords, operators, reserved } from "../user/spellings.js"

import {
    And,
    AND,
    ARSHIFT,
    As,
    Ask,
    Assert,
    Assign,
    AssignAND,
    AssignARSHIFT,
    AssignFloor,
    AssignPlus,
    AssignLSHIFT,
    AssignMinus,
    AssignModulo,
    AssignOR,
    AssignRaise,
    AssignRSHIFT,
    AssignStar,
    AssignXOR,
    Async,
    Await,
    Bang,
    Break,
    ClassLiteral,
    CloseBrace,
    CloseBracket,
    CloseParen,
    Label,
    Comma,
    Continue,
    Debug,
    Delete,
    Dev,
    Do,
    Dot,
    Else,
    Equal,
    EOF,
    Export,
    FalseConstant,
    FatArrow,
    Floor,
    For,
    Freeze,
    From,
    Frozen,
    FunctionLiteral,
    Greater,
    If,
    Import,
    In,
    InfinityConstant,
    Is,
    Lesser,
    Let,
    LineFeed,
    LSHIFT,
    Minus,
    Modulo,
    NaNConstant,
    New,
    Not,
    NotEqual,
    NotGreater,
    NotLesser,
    NullConstant,
    Nullish,
    Of,
    On,
    Or,
    OR,
    OpenBrace,
    OpenBracket,
    OpenParen,
    Pack,
    Packed,
    Pass,
    Plus,
    Private,
    Raise,
    Reserved,
    Return,
    RSHIFT,
    Seal,
    Sealed,
    SkinnyArrow,
    Slash,
    Spread,
    Star,
    Static,
    Subclass,
    SuperConstant,
    Throw,
    TrueConstant,
    Var,
    Variable,
    VoidConstant,
    When,
    While,
    XOR,
    Yield
} from "../user/concrete.js"

export class Token {

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

    The five *API methods* (`token`, `prefix`, `infix`, `validate` and `js`) take a reference
    to the corresponding API object (`lexer`, `parser` or `writer`) as their first argument,
    and are individually documented below (within the default implementations), along with
    the helper methods. */

    LBP = 0;                // left-binding-power (described by the `get RBP` method below)
    operands = [];          // the token's operands (including params, blocks, tokens etc)
    compile = true;         // whether to compile the statement to JS (used by `dev` mode)
    initial = true;         // whether the token was found in the prefix position or not
    expression = false;     // whether the token forms a node which is a valid expression

    get spelling() {

        /* This API property specifies the JavaScript spelling for the given token, defaulting
        to the Lark spelling.

        Note: While `spelling` is a computed (instance) property here, it is often overridden
        by a simple stored property. */

        return this.value;
    }

    get RBP() {

        /* Every token defines or inherits `LBP` and `RBP` properties (short for *left-binding-
        power* and *right-binding-power* respectively), refered to hereafter as *LBP* and *RBP*.

        LBP is required by the Parser Stage API, and is used by Pratt's Algorithm to implement
        operator precedence. RBP is only used within this object hierarchy (it is not required
        by any API), and only exists because many token types need two binding powers, and it
        can be useful to name them.

        Tokens that should not have any precedence (keywords, terminals, terminators etc) must
        have an LBP of zero, so they can just inherit their binding powers from this class.

        Tokens that implement an `infix(parser)` method (infix and suffix operators) must set
        LBP to the precedence of that operator, and will *almost* always pass the same value
        in recursive calls to the Parser Stage API (when gathering their righthand operand).
        The note below the following paragraph describes the exception.

        Tokens that implement a `prefix(parser)` method (prefix operators) are not required to
        set a binding power property, but will need to pass the precedence of the operator in
        recursive calls to the Parser Stage API. RBP is often used to store that value, as
        tokens can implement both prefix operators and infix/suffix operators.

        Note: RBP is also used by right-associative operators (that do not have a prefix form),
        just as a convenient place to store `LBP - 1` (as per Pratt's Algorithm).

        This computed property returns LBP as the value for RBP, as they're often the same value.
        Subclasses can simply override this property (usually just using a stored property). */

        return this.LBP;
    };

    constructor(location, value=empty) {

        /* This constructor takes and initializes the two remaining propertoes (`location` and
        `value`) that combine with the five properties defined above (`LBP`, `RBP`, `operands`,
        `expression` and `spelling`) to create the complete set of seven properties that all
        token instances require.

        Note: While a couple of the subclasses define helper methods for their own use, token
        instances generally have the same shape (the same properties). This is why we push
        the various AST nodes to a generic array of operands, instead of letting each
        subclass define its own properties (`lvalue`, `predicate`, `block` etc). */

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

        /* This method takes a reference to the Parser API and an optional `context`. By defaut,
        the method just throws an exception. However, tokens that are valid in the prefix position
        can define their own implementations that use the Parser API to parse the token stream
        (pushing results to the `operands` array), before returning the resulting AST node
        (usually `this`).
        
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

        /* This helper is used by `prefix` and `infix` methods to push zero or more operands to
        the operands array for the current instance. The method returns a reference to `this`,
        as its caller will invariably need to do so too. */

        args.forEach(arg => this.operands.push(arg));

        return this;
    }

    at(index) { // internal helper

        /* Expose the `at` method of the `operands` array directly. */

        return this.operands.at(index);
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

        const operands = this.operands.slice(skip);

        // next, if the `singular` rule applies, check that there is only one relevant operand...

        if (singular) switch (operands.length) {
            case 1: break;
            case 0: throw new LarkError("expected an expression", this.location);
            default: throw new LarkError("unexpected expression", operands[1].location);
        }

        // next, if either of the `plain` or `proto` rules apply, traverse the `operands` array,
        // and enforce whichever rules apply to each operand...

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

        if (this.initial) return opener + join(this.operands) + closer;
        else return this.at(0).js(writer) + opener + join(this.operands.slice(1)) + closer;
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
            case "subclass": return new Subclass(location, value);
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

        if (parser.on(Variable)) this.push(parser.gatherVariable());

        return this;
    }

    js(writer) {

        /* Render a `break` or `continue` statement with its optional label. */

        const expression = this.at(0) ? space + this.at(0).js(writer) : empty;

        return `${this.spelling}${expression}`;
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

    prefix(parser) {

        return this.push(parser.gatherExpression(), parser.gatherBlock(this.constructor.block));
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

        this.initial = false;

        return this.push(left, parser.gatherExpression(this.LBP));
    }

    js(writer) {

        if (this.initial) {

            let separator = lowers.includes(this.spelling[0]) ? space : empty;

            return `${this.spelling}${separator}${this.at(0).js(writer)}`;

        } else return `${this.at(0).js(writer)} ${this.spelling} ${this.at(1).js(writer)}`;
    }
}

export class ArrowOperator extends GeneralOperator {

    /* This is the abstract base class for the arrow operators. Both operators support prefix
    and infix grammars, which are right-associative. */

    LBP = 2;
    RBP = 1;

    infix(parser, left) {

        /* This method overrides the inherited version to ensure that the `left` parameter
        (when present) is wrapped in parens, and that the operator is right-associative. */

        this.initial = false;

        if (left.is(OpenParen)) return this.push(left, parser.gatherExpression(this.RBP));
        else throw new LarkError("arrow-operator parameters must be parenthesized", left.location);
    }
}

export class AssignmentOperator extends InfixOperator {

    /* This is the abstract base class used by all assignment operators, which are right-
    associative. */

    LBP = 2;
    RBP = 1;
    initial = false;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.RBP));
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