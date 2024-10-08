import {
    alphas,
    backslash,
    backtick,
    bases,
    binary,
    closeBrace,
    closeBracket,
    closeParen,
    colon,
    comma,
    constants,
    decimal,
    dollar,
    dot,
    empty,
    hexadecimal,
    keywords,
    lowers,
    newline,
    openBrace,
    openBracket,
    openParen,
    operators,
    quote,
    reserved,
    space,
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

export class LarkError extends SyntaxError {

    /* This concrete class is used for all Lark syntax errors (across all of the stages). */

    constructor(message, location) {

        /* Take a message string and a location `Number`, unpack the line and column numbers,
        and use them to create a complete error message, deleting the contents of the `stack`
        and customizing the error name.

        Note: Line and column numbers are stored as if the `Number` is an unsigned 32-bit
        integer, with the most significant three bytes storing the line number, and the
        least significant byte storing the column number (both internally zero-indexed).
        This just makes it easier to ignore the location data when debugging. */

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

    /* This is the abstract base class for all other token classes. Internally, Lark reuses the
    same classes for the tokens (in the token stream) and the AST nodes (in the abstract syntax
    tree). This approach makes Pratt parsing a lot easier, and permits a single OOP hierarchy
    (which makes up the rest of this file).

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

    LBP = 0;               // left-binding power (a generalization of precedence, per Pratt)
    RBP = 0;               // right-binding-power (prefix and infix precedence often differs)
    operands = [];         // the token's operands (including params, blocks, tokens etc)
    expression = false;    // specifies whether the token forms a valid expression

    get spelling() {

        /* This API property specifies the JavaScript spelling for the given token, defaulting
        to the Lark spelling.

        Note: While `spelling` is a computed (instance) property here, it is often overridden
        by a simple stored property. */

        return this.value;
    }

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
        by `Async` to let `Lambda`, `Function` and `Generator` know the context (without which,
        they cannot correctly validate `await` statements). */

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

        /* This helper just makes indexing operands a little bit easier. */

        return this.operands.at(index);
    }

    is(Type) { // internal helper

        /* This helper just makes checking instance types a little bit easier. */

        return this instanceof Type;
    }
}

class Terminal extends Token {

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

export class NumberLiteral extends Terminal {

    /* This is the concrete class for all number-literals. It is also imported by the Lexer Stage
    for number tokenization. */

    expression = true;

    static * lex(...args) { yield new NumberLiteral(...args) }

    constructor(lexer, location) {

        /* This constructor tokenizes a number literal, ensuring that dots are only included when
        they are valid (permitting number literals to be followed by a dot operator). */

        super(location, lexer.read());

        // establish the base, and create a reference to the appropriate digit-set...

        if (lexer.on("0") && lexer.at(bases)) {

            this.value += lexer.advance();

            var [digits, isDecimal] = [lexer.on("xX") ? hexadecimal : binary, false];
        
        } else var digits = decimal, isDecimal = true;

        // gather as many digits as possible from the appropriate set...

        lexer.gatherWhile(digits, this);

        // validate the value so far, requiring that it does not start with a zero, unless it is
        // the start of a base-prefix, or it is just a single zero, as well as checking that it
        // is not just a a base-prefix without any significant digits...

        if (isDecimal && this.value[0] === "0" && this.value !== "0") { // leading zeros...

            throw new LarkError("leading zeros are invalid", location);
        }

        if (!isDecimal && this.value.length === 2) { // base-prefix without (valid) digits...

            if (lexer.at(decimal)) throw new LarkError("invalid digit for notation", location);
            else throw new LarkError("incomplete base-prefix", location);
        }

        // finally, check for a dot, and handle it if present...

        if (lexer.at(dot)) {

            // if a decimal point is currently legal (the literal uses decimal notation and the
            // literal did not start with a dot), and should be considered part of the literal
            // (at least one decimal digit immediately follows the dot), then gather the dot
            // and the digits that follow it, otherwise complain (as only decimals use dots
            // in javascript)...

            if (isDecimal && this.value[0] !== dot && lexer.peek(+2, decimal)) {

                this.value += lexer.advance();
                lexer.gatherWhile(digits, this);
            
            } else throw new LarkError("fractional numbers must use decimal notation", location);
        }
    }

    prefix(_) { return this }

    validate(_) { return true }
}

export class StringLiteral extends Terminal {

    /* This is the concrete class for string-literals and text-literals. It is also imported by the
    Lexer Stage for tokenizing strings and their interpolations.

    Lark always compiles its literals to the equivalent JS template-literals (using backticks).

    Note: Lark string and text literals can appear in an infix position (following some arbitrary
    expression). In that case, they have a binding power of `16`, and the result compiles to the
    equivalent tagged-template-literal. However, unlike JavaScript, Lark accepts spaces between
    the expression and the literal that follows it (and recommeds using one space).

    The `value` string is used to store the opening quotes, which will always contain one quote or
    at least three (distinguishing string-literals from text-literals). The `operands` array stores
    the string contents, which are broken into strings and arrays of tokens. The arrays contain the
    interpolations, which are lexed into token arrays, and later replaced by their corresponding
    AST nodes by the parser stage.

    Characters that need escaping (grave accents, dollars followed by opening braces etc) are all
    escaped by the lexer stage (prepending backslashes to the strings in the `operands` array).

    The ignored parts of text-literals (the leading line, the trailing line and any indentation)
    are all removed from the `operands` array during the parser stage.

    The writer stage only needs to concatenate the strings in the `operands` array together, with
    recursive calls to `write` on interpolations to interpolate their JS strings (using a dollar
    and a pair of curly braces). */

    LBP = 16;
    expression = true;

    static * lex(...args) { yield new StringLiteral(...args) }

    constructor(lexer, location) {

        /* This constructor gathers the value of a string literal, writing the characters and any
        interpolations (as arrays of tokens) to the `operands` array. */

        function gather(character, value=empty) {

            /* Take a character, and an optional value string (defaulting to empty), and gather up any number of contiguous instances of the given character, concatenating them to the
            value, then return the result. */

            while (lexer.at(character) && lexer.advance()) value += lexer.read();

            return value;
        }

        super(location, gather(quote, quote)); // store the opening quote[s] in `value`

        while (lexer.advance()) {

            if (lexer.on(quote)) {

                // this block handles one or more quotes, which may close the literal, may be
                // part of the literal, or may just be too many quotes for the literal...

                const candidate = gather(quote, quote); // potential closing quotes
                const headCount = this.value.length;    // number of opening quotes
                const tailCount = candidate.length;     // number of potential closing quotes

                if (tailCount > headCount) {            // too many closing quotes...

                    let message = `${headCount} opening quotes with ${tailCount} closing quotes`;

                    throw new LarkError(message, this.location);
                }

                if (candidate === this.value) break;    // the required amount of closing quotes
                else this.push(candidate);              // too few closing quotes to close on

            } else if (lexer.on(newline)) {

                // this block handles newlines, followed by zero or more spaces...

                this.push(lexer.read() + gather(space));

            } else if (lexer.on(backslash) && lexer.at(openParen)) {

                // this block notes the previous state of the lexer (normal or interpolating),
                // then sets it to interpolating, gathers a stream of tokens into the `operands`
                // array, before restoring the previous state...

                let previousState = lexer.interpolate();

                lexer.advance();
                lexer.interpolate(true);
                this.push([...lexer.gatherStream()]);
                lexer.interpolate(previousState);    

            } else if (lexer.on(backtick) || (lexer.on(dollar) && lexer.at(openBrace))) {

                // this block handles characters that are meaningful in a js template-literal, but
                // have no special meaning in a lark string or text literal...

                this.push(backslash, lexer.read());

            } else this.push(lexer.read()); // a regular character
        }
    }

    prefix(parser) {

        /* Due to the way string literals are tokenized, the `operands` array already contains the
        strings and interpolations that comprise the literal. Each interpolation is an array of
        tokens (which may recursively contain strings with their own interpolations). This
        method parses the interpolations, replacing them with corresponding AST nodes.

        The method also checks that interpolations are valid expressions, complaining if there are
        any interpolated statements.
        
        When the instance is a text literal, this method will also validate the special rules for
        text literals, before removing the first and last lines, as well as any indentation. */

        for (const [index, operand] of Object.entries(this.operands)) {

            if (typeof operand === "string") continue;

            // parse and expand the tuple (assigning it back to the `operands` array)...

            const tuple = this.operands[index] = [...parser.parse(operand)];

            // check that `tuple` only contains expressions (not statements)...

            for (const interpolation of tuple) if (!interpolation.expression) {

                const message = "string interpolations can only contain expressions";

                throw new LarkError(message, interpolation.location);
            }
        }

        if (this.value === quote) return this;

        // the rest of this method only applies to text literals...

        const messages = {
            closer: "text literal is improperly closed",
            indent: "text literal cannot be less indented than its closing quotes"
        };

        const lastOperand = this.operands.pop();  // the closing indentation of the text literal
        const indentation = /^\n\x20*$/;          // matches a newline, then zero or more spaces

        if (!indentation.test(lastOperand)) throw new LarkError(messages.closer, this.location);

        this.operands = this.operands.slice(1);   // remove the opening line from the literal

        for (const [index, operand] of Object.entries(this.operands)) {

            // skip this operand, unless it's an indentation string...

            if (!indentation.test(operand)) continue;

            // if the current operand is at least as indented as the last operand, strip out the
            // appropriate number of space characters, and update the `operand` array, otherwise
            // complain there is not enough indentation...

            if (operand.startsWith(lastOperand)) {

                this.operands[index] = newline + operand.slice(lastOperand.length);

            } else throw new LarkError(messages.indent, this.location);
        }

        return this;
    }

    infix(parser, prefix) {

        /* String literals can follow an expression (like a suffix operator) to create a tagged
        template literal. The literal still gets parsed as normal. The only difference is that
        the prefix expression is appended to the operands array.
        
        Note: The `operands` array always ends with a `String` or `Array`, unless it is tagged.
        Tagged literals end with `Token`, so `js(writer)` uses the type of the last operand to
        know whether a literal is tagged or not. */

        this.prefix(parser);

        return this.push(prefix);
    }

    validate(_) { return true }

    js(writer) {

        /* Generate a template literal from the various parts of the string literal, reproducing
        the interpolations, and possibly including a tag expression. */

        if (this.operands.at(-1) instanceof Token) { // tagged literal...

            var output = this.operands.pop().js(writer) + backtick;

        } else var output = backtick; // plain string literal

        for (const operand of this.operands) {

            if (operand instanceof Array) for (const interpolation of operand) {

                output += "${" + interpolation.js(writer) + "}";

            } else output += operand;
        }

        return output + backtick;
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

class Opener extends Delimiter {

    /* This is an abstract base class for opening parens, brackets and braces. Subclasses implement
    grouped expressions, invocations, array literals, object literals and bracket notation. */

    check(rules) {

        /* This helper is used to traverse, validate and modify the operands that represent the
        items inside the compound-expressions that are implemented by its subclasses. It takes a
        hash of arguments that specify which rules to check, and which parameters to use, before
        returning a hash of results, currently only containing `{prototyped: Bool}`:

            + `proto bool`: When enabled, `__proto__` keys get converted to regular strings, so they
               have no special meaning, and `(Prototype)` expressions are noted (and later expanded
               to JavaScript `__proto__: Prototype` pairs in their `js(writer)` methods). If exactly
               one such expression is noted, the `prototyped` boolean result is set to `true`. If
               more than one expression gets expanded, the helper complains instead. The value of
               `proto` defaults to `false` (so none of the above applies by default).
            + `prefixed Token`: When present, the check starts from the operand following the first
               operand of the given token-type, and starts from the first token otherwise.
            + `plain bool`: When truthy, the helper checks that all of the relevant operands are
               plain expressions (without labels), complaining otherwise. Defaults to `false`.
            + `singular bool`: When truthy, the helper ensures that the `checklist` containing all
               of the relevant operands (having accounted for `prefixed`, when present) contains
               exactly one operand. Defaults to `false`.

        Note: Only top-level operands are checked (as children handle their own operands). */

        const {proto, prefixed, singular, plain} = rules;

        // start by slicing the `operands` array, keeping everything after the starting token, or
        // the entire array, if `prefixed` was not defined...

        let startingIndex = 0;

        if (prefixed) for ( ; this.operands[startingIndex - 1] !== prefixed; startingIndex++);

        const checklist = this.operands.slice(startingIndex);

        // next, if the `singular` rule is truthy, check that there is only one relevant operand...

        if (singular) switch (checklist.length) {
            case 1: break;
            case 0: throw new LarkError("expected an expression", this.location);
            default: throw new LarkError("unexpected expression", checklist[1].location);
        }

        // next, if either of the `plain` or `proto` rules needs checking, traverse the `checklist`
        // array; if `plain` is truthy, simply ensure that there are no labels; while if `proto` is
        // truthy, ensure that `__proto__` keys are wrapped in quotes and that any `(Prototype)`
        // expressions are expanded into `__proto__` keys, while also ensuring that there is
        // at most one `(Prototype)` expression in the `checklist` array, else complain...

        let prototyped = false;

        if (plain || proto) for (const operand of checklist) {

            if (operand instanceof Label) {

                if (plain) throw new LarkError("unexpected label", operand.location);

                if (proto && operand.at(0).is(Variable) && operand.at(0).value === "__proto__") {

                    operand.at(0).value = quote + operand.at(0).value + quote;
                }

            } else if (proto && operand.is(OpenParen)) {

                if (prototyped) throw new LarkError("superfluous prototype", operand.location);
                else { prototyped = true; operand.value = "__proto__" }
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

        if (this.at(1)?.prototype instanceof Opener) { // infix...

            const prefix = this.at(0).js(writer);

            return prefix + opener + join(this.operands.slice(2)) + closer;

        } else return opener + join(this.operands) + closer; // prefix
    }
}

export class Closer extends Delimiter {

    /* This is an abstract base class for closing parens, brackets and braces. It is also imported
    by the Parser Stage to check for closing tokens. */
}

class Caller extends Opener {

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

class Constant extends Word {

    /* This is the abstract base class for constant words, used for named numbers (like `Infinity`
    and `NaN`), as well as magic variables (`this`, `default`, `arguments`, `random`, `void`, and
    `true` etc). It is also used (internally) by the `Word` class (see `subclass`). */

    expression = true;

    prefix(_) { return this }

    static subclass(location, value) {

        /* Return the appropriate subclass, based on the value. */

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

    /* This abstract base class is used by all of the keyword classes. It is also used (internally)
    by the `Word` class (see `subclass`). */

    static subclass(location, value) {

        /* Return the appropriate subclass, based on the value. */

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

    /* This is the abstract base class for statements that are used to branch from loops (in
    practice, `break` and `continue`), which accept an optional (`Variable`) label. */

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

class ClassQualifier extends Declaration {

    /* This is the abstract base class for the `static`, `local` and `private` qualifiers
    that prefix declarations inside classes. */

    prefix(parser) {

        /* When present, gather an optionally asynchronous function, and parse a regular
        declaration otherwise. */

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

    /* This is the abstract base class for statements that have a block. It is imported by the
    parser, which uses it to implement LIST correctly.*/
}

class PredicatedBlock extends Header {

    /* This is the abstract base class for the predicated blocks (`if`, `else if`, `while`,                 // TODO: remember `else if`
    `unless` and `until`). */

    prefix(parser) {

        return this.push(parser.gatherExpression(), parser.gatherBlock(this.constructor.block));
    }

    js(writer) {

        return `${this.value} (${this.at(0).js(writer)}) {${writer.writeBlock(this.at(1))}}`;
    }
}

class Functional extends Header {

    /* This is the abstract base class for all functional blocks, including the `lambda`,
    `function` and `generator` blocks, but not including the arrow grammars. This applies
    to function statements/expressions, whether they are asynchronous, IIFEs, neither or
    both. */

    LBP = 1;
    expression = true;

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

    expression = true;

    static slice(value, offset, location) {

        /* Take a string (`value`) containing a contiguous sequence of symbolic operator
        characters, and break it into an array of individual operators, greedily, from
        left to right, based on a given `offset` (into the string) and `location`.
        
        Note: The location is only needed to generate synax errors. */

        if (!value) return [];

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

    js(writer) {

        return `${this.at(0).js(writer)} ${this.spelling} ${this.at(1).js(writer)}`;
    }
}

class DotOperator extends InfixOperator {

    /* This base class provides functionality for dot-operators, which must have a property
    name (which can be any type of word token) on the rightside. */

    LBP = 17;

    infix(parser, left) {

        return this.push(left, parser.gatherProperty());
    }

    js(writer) {

        return `${this.at(0).js(writer)}${this.spelling}${this.at(1).js(writer)}`;
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

    js(writer) {

        if (this.operands.length === 1) { // prefix operator...

            let separator = lowers.includes(this.spelling[0]) ? space : empty;

            return `${this.spelling}${separator}${this.at(0).js(writer)}`;

        } else return `${this.at(0).js(writer)} ${this.spelling} ${this.at(1).js(writer)}`;
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

        if (left instanceof OpenParen) return this.push(left, parser.gatherExpression(this.RBP));
        else throw new LarkError("arrow-operator parameters must be parenthesized", left.location);
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

class GeneralDotOperator extends DotOperator {

    /* This base class extends the dot-operator class with functionality specific to the bang
    and ask operators, which are also bitwise prefix operators. */

    RBP = 14;

    prefix(parser) {

        return this.push(parser.gatherExpression(this.RBP));
    }
}

/// THE CONCRETE TOKEN CLASSES...

class AllConstant extends Constant {

    spelling = "*";

    /* This concrete class implements the `all` constant, used instead of an asterisk
    in import and export statements. */
}

class ArgumentsConstant extends Constant {

    /* This concrete class implements the `arguments` constant. */
}

class And extends InfixOperator {

    /* This concrete class implements the logical `and` operator, which compiles to `&&`. */

    LBP = 4;
    spelling = "&&";
}

class AND extends InfixOperator {

    /* This concrete class implements the `&` infix operator (bitwise AND). */

    LBP = 7;
}

class ARSHIFT extends InfixOperator {

    /* This concrete class implements the `>>>` infix operator (bitwise arithmetic-shift-right). */

    LBP = 10;
}

class As extends InfixOperator {

    /* This concrete class implements the `as` operator, used in import and export statements for
    assignments and reassignments (like in JavaScript). */

    LBP = 1;
}

class Ask extends GeneralDotOperator {

    spelling = "?.";

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
    `function` and `generator` keywords to define asynchronous versions. */

    expression = true;

    prefix(parser) {

        /* This method checks that the next token is valid, and if so, gathers it as an
        operand, complaining otherwise. */

        if (parser.on(Functional)) return this.push(parser.gather(0, this));
        else throw new LarkError("unexpected async qualifier", this.location);
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

class Bang extends GeneralDotOperator {

    spelling = ".#";

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

                throw new LarkError(`undefined label '${label.value}'`, label.location);
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

export class Label extends InfixOperator {

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
            else throw new LarkError("cannot reassign an active label", prefix.location);

            this.expression = false;
            this.push(prefix, parser.gather());

            parser.label(prefix, null);

        } else super.infix(parser, prefix); // a key-value pair

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

            if (state === null) throw new LarkError("undefined label", label.location);
            else if (state === false) throw new LarkError("must continue a loop", label.location);
        }

        return parser.check($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }
}

class Debug extends Keyword {

    /* This concrete class implements the `debug` statement, which compiles to `debugger`. */

    spelling = "debugger";

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
    parser is in `devmode`. */// TODO: add a `devmode` parameter to each stage

    prefix(parser) {

        return this.push(parser.gather());
    }
}

class Do extends Header {

    /* This concrete class implements the `do` keyword, which prefixes blocks to create
    block statements, or prefixes `async`, `lambda`, `function` or `generator` to create
    an IIFE. */

    prefix(parser) {

        /* If the next token is `async` or something functional, make this instance valid
        as an expression, then gather whatever follows. Otherwise, gather a control-flow
        block. */

        if (parser.on(Async, Functional)) {
            
            this.expression = true;
            this.push(parser.gather());

        } else this.push(parser.gatherBlock(SIMPLEBLOCK));

        return this;
    }
}

class Dot extends DotOperator {

    /* This concrete class implements the dot operator (`.`), which is used for property access.
    The points in number literals do not use this class. */
}

class Else extends PredicatedBlock {

    /* This concrete class implements `else` and `else if` clauses. */

    prefix(parser) {

        /* Gather an else-clause (with its block) or an else-if clause (with its predicate before
        its block). */

        if (parser.on(If)) return this.push(If, parser.gather());
        else return this.push(parser.gatherBlock(SIMPLEBLOCK));
    }

    js(writer) {

        if (this.at(0) === If) return `else ${this.at(1).js(writer)}`;
        else return `else {${writer.writeBlock(this.at(0))}}`;
    }
}

class Equal extends InfixOperator {

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

    js(writer) {

        return `Math.floor(${this.at(0).js(writer)} / ${this.at(1).js(writer)})`;
    }
}

class For extends Header {

    /* This concrete class implements for-in loops (old school for-loops have not been
    designed yet, but will be added in some form. */

    prefix(parser) {

        /* This method parses a for-in-loop. It must be careful when gathering the param,
        as the `in` keyword is also an infix operator. */

        this.push(parser.gatherAssignee());

        if (parser.on(In, From)) this.push(parser.advance(parser.on(In) ? In : From));
        else throw new LarkError("incomplete for-loop", this.location);

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

    prefix(parser, context) {

        /* This method parses full-fat functions, based on the given context (which may
        be an instance of `Async` or `undefined`). */

        let blockType = context instanceof Async ? ASYNCFUNCTIONBLOCK : FUNCTIONBLOCK;

        return this.gatherFullHeader(parser, blockType);
    }
}

class Generator extends Functional {

    /* This is the concrete class for generator statements, which are also expressions. */

    prefix(parser, context) {

        /* This method parses generators, based on the given context (which may be an
        instance of `Async` or `undefined`). */

        let blockType = context instanceof Async ? ASYNCGENERATORBLOCK : GENERATORBLOCK;

        return this.gatherFullHeader(parser, blockType);
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

    /* This concrete class implements the infix in-operator. */

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

    js(writer) {

        if (this.at(1) instanceof Not) {

            if (this.at(2) instanceof Packed) return `Object.isExtensible(${this.at(0).js(writer)})`;
            if (this.at(2) instanceof Sealed) return `!(Object.isSealed(${this.at(0).js(writer)}))`;
            if (this.at(2) instanceof Frozen) return `!(Object.isFrozen(${this.at(0).js(writer)}))`;

            return `!(${this.at(0).js(writer)} instanceof ${this.at(2).js(writer)})`;
        }

        if (this.at(1) instanceof Packed) return `!(Object.isExtensible(${this.at(0).js(writer)}))`;
        if (this.at(1) instanceof Sealed) return `Object.isSealed(${this.at(0).js(writer)})`;
        if (this.at(1) instanceof Frozen) return `Object.isFrozen(${this.at(0).js(writer)})`;

        return `${this.at(0).js(writer)} instanceof ${this.at(1).js(writer)}`;
    }
}

class Lambda extends Functional {

    /* This is the concrete class for lambda statements, which are also expressions. */

    prefix(parser, context) {

        /* This method parses lambdas, based on the given context (which may be an instance
        of `Async` or `undefined`). */

        let blockType = context instanceof Async ? ASYNCFUNCTIONBLOCK : FUNCTIONBLOCK;

        return this.gatherLambdaHeader(parser, blockType);
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

class Local extends ClassQualifier {

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
    spelling = "!";

    infix(parser, left) {

        if (parser.on(In)) {

            return this.push(left, parser.advance(true), parser.gatherExpression(this.LBP));

        } else throw new LarkError("unexpected not-operator", this.location);
    }
}

class NotEqual extends InfixOperator {

    /* This concrete class implements the `!=` operator, which compiles to `!Object.is`. */

    LBP = 8;

    js(writer) {

        return `!Object.is(${this.at(0).js(writer)}, ${this.at(1).js(writer)})`;
    }
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
    pairs of clz32-operators (`?`) in a prefix position (this is an edgecase, where the
    Operator. disambiguation). */

    LBP = 3;    // the infix precedence applies to the nullish operator
    RBP = 14;   // the prefix precedence applies to clz32 (and equals bitwise-not)
}

class Of extends InfixOperator {

    /* This concrete class implements the infix of-operator, which is also used by functions
    and generators to prefix their (optional) arguments, as well as `for-of` loops. */

    LBP = 8;
}

class Or extends InfixOperator {

    /* This concrete class implements the logical-or operator, which compiles from `or` to `||`. */

    LBP = 3;
    spelling = "||";
}

class OR extends InfixOperator {

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

        this.operands = parser.gatherCompoundExpression(CloseBrace);

        if (this.check({proto: true}).prototyped) this.value = "__proto__";

        return this;
    }

    validate(_) { return true }

    js(writer) {

        /* Output a JS object literal. Infer a `null` prototype, unless it contains a
        `(Prototype)` expression (indicated by having a `value` of "__proto__"). */

        const head = this.value === "__proto__" ? openBrace : "{__proto__: null, ";
        const body = this.operands.map(operand => operand.js(writer)).join(comma + space);

        return head + body + closeBrace;
    }
}

export class OpenBracket extends Caller {

    /* This concrete class implements the open-bracket delimiter, which is used for array
    expressions and bracket-notation. */

    prefix(parser) {

        /* This method gathers an array expression. */

        this.operands = parser.gatherCompoundExpression(CloseBracket);
        this.check({plain: true});

        return this;
    }

    infix(parser, left) {

        /* This method gathers bracket-notation. */

        this.push(left, OpenBracket, ...parser.gatherCompoundExpression(CloseBracket));
        this.check({prefixed: OpenBracket, singular: true, plain: true});

        return this;
    }

    js(writer) {

        /* Output a JS array literal or bracket notation. */

        return super.js(writer, openBracket, closeBracket);
    }
}

class OpenParen extends Caller {

    /* This concrete class implements the open-paren delimiter, which is used for grouped
    expressions and invocations. */

    prefix(parser) {

        /* This method gathers a grouped expression. */

        this.operands = parser.gatherCompoundExpression(CloseParen);
        this.check({plain: true});

        return this;
    }

    infix(parser, left) {

        /* This method gathers an invocation, which will be validated later. */

        this.push(left, OpenParen, ...parser.gatherCompoundExpression(CloseParen));
        this.check({prefixed: OpenParen, plain: true});

        return this;
    }

    js(writer) {

        /* Output a grouped expression or an invocation (using the `js` method inherited from
        the `Opener` base class, unless the `value` string is "__proto__". In which case, the
        first operand gets expanded to a prototype declaration.
        
        Note: It is a syntax error to have more than one expression in a prototype expression. */

        if (this.value === "__proto__") return this.value + colon + space + this.at(0).js(writer);
        else return super.js(writer, openParen, closeParen);
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

class Private extends ClassQualifier {

    /* This contrete class implements private-statements, used inside classes. */

    prefix(parser) {

        if (parser.on(Static, Local)) return this.push(parser.gather());
        else throw new LarkError("incomplete private-declaration", this.location);
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

        throw new LarkError(`reserved word (${this.value})`, this.location);
    }

    infix(_) {

        throw new LarkError(`reserved word (${this.value})`, this.location);
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

class Static extends ClassQualifier {

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

        } else throw new LarkError("incomplete subclass", parser.advance(true).location);
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
        else throw new LarkError("incomplete when-operation", this.location);

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
