/* This module defines `Token` and all of its subclasses, forming a single token hierarchy that
implements all of the language specifics. */

import { constants, keywords, operators, reserved } from "../language/spellings.js"

import { put, not, iife, lark } from "../compiler/helpers.js"
import { LarkError } from "../compiler/error.js"

import {
    alphas,
    asterisk,
    atSign,
    backslash,
    backtick,
    bases,
    binary,
    closeBrace,
    closeBracket,
    closeParen,
    colon,
    comma,
    decimal,
    dollar,
    dot,
    equals,
    empty,
    hexadecimal,
    lowers,
    newline,
    openBrace,
    openBracket,
    openParen,
    pound,
    quote,
    slash,
    space,
    operationals,
    wordCharacters,
    wordInitials,
    underscore,
} from "../compiler/ascii.js"

// MARK: THE TOKEN SUPERCLASS

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

    The API methods (`token`, `prefix`, `infix`, `validate` and `js`) each require a reference to
    an API object (`lexer`, `parser`, `validator` or `writer`) as their first argument (which is
    often destructured). Each method is individually documented below (within their respective
    default implementations).

    Every token defines or inherits `LBP` and `RBP` properties (short for *left-binding-power*
    and *right-binding-power* respectively), refered to hereafter as *LBP* and *RBP*.

    LBP is required by the Parser Stage API, and is used by Pratt's Algorithm to implement operator
    precedence. RBP is only used within this object hierarchy (it is not required by any API).

    Tokens that should not have any precedence (plain keywords, terminals, terminators etc) must
    have an LBP of zero, so they can just inherit their binding-powers from this class.

    Tokens that implement an `infix` method (infix, suffix and mixfix operators) must set LBP to
    the precedence of that operator, and will reuse the same value in any recursive calls to the
    Parser Stage API (when gathering any righthand operands), except for right-associtive
    operators, which pass `LBP - 1` instead.

    Tokens that implement a `prefix` method (prefix operators) are not *required* to set a binding-
    power property, but will need to pass the precedence of the operator in recursive calls to the
    Parser Stage API. RBP is used to store that binding-power, as there are many tokens that
    implement both prefix operators and infix/suffix operators.

    The `notes` property is an (initially empty) set of strings that subclasses use to note various
    details regarding the specifics of that instance. Each note must belong to a static, frozen set
    named `notables`, as after being noted (by calling the `note` helper method on one or more note
    strings), the note effectively becomes a property of that instance (as instances get wrapped by
    a proxy in `Token.constructor`, then explicitly returned, replacing `this`), so would clobber
    properties and methods otherwise.

    The `expression` property is set to `true` by anything that is a valid expression, and left
    `false` otherwise. The property is required by the Parser API.

    The `safe` property is used during the Writer Stage (by `/language/javascript.js` and this
    file) to know if an expression can be safely reused in the compiled code (`true`), or needs
    to be assigned to a Lark register during first evaluation, then referenced (`false`). */

    location; value; // initialized by the constructor

    LBP = 0;
    RBP = 0;
    safe = false;
    expression = false;
    notes = new Set();

    static notables = Object.freeze(new Set([
        "tagged_template", "proto_label", "yield_from", "at_name", "at_parameter", "if_else",
        "for_loop", "for_in", "for_of", "for_on", "for_from", "else_if", "not_in", "not_of",
        "float_notation", "integer_notation", "unit_notation", "exponentiation_notation",
        "nud", "led", "lvalue", "oo_literal", "set_literal", "map_literal", "parameter",
        "ignored", "labelled", "validated", "terminated", "then_statement",
        "packed_qualifier", "sealed_qualifier", "frozen_qualifier",
        "async_qualifier", "yield_qualifier", "not_qualifier",
        "property_descriptor", "property_description",
    ]));

    constructor(location, value=empty) {

        /* This constructor takes and initializes the `location` property (as a `BigInt`), and the
        `value` property (as a `String`, copied from the source), both provided during the Lexer
        Stage, when all tokens are initialzed (the parser reuses tokens as AST nodes, per Pratt).

        The `location` is represented by an unsigned (and unbounded) `BigInt`, which stores the
        column number (internally zero-indexed) in the lowest eight bits (limiting files to 256
        columns), and the line number in the higher bits (with effectively no upperbound).

        Lark recommends using 128-column source files, but supports upto 256 columns for edgecases
        that require super-long lines. Users cannot go past 256 columns (it's a syntax error). */

        super();
        this.location = location;
        this.value = value;

        const get = function(target, name) {

            /* This proxy handler exposes the strings in the `notes` set as instance properties. */

            return Token.notables.has(name) ? target.notes.has(name) : target[name];
        };

        return new Proxy(this, {get});
    }

    get spelling() { // api property

        /* This API property specifies the JavaScript spelling for the given token, defaulting
        to the Lark spelling of the same token. */

        return this.value;
    }

    static * token(lexer) { // api method

        /* This method instantiates tokens, based on the source. It is the only API method that
        is invoked by the Lexer Stage. It is static as it wraps the constructor method to allow
        it to yield zero or more tokens (potentially including instances of a subclass) to the
        token stream. This naturally requires that `token` is a generator too.

        This method is invariably redefined by the lower classes, so the default implementation
        only contains this docstring. */
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

        if (parser.on(Terminator)) throw new LarkError("unexpected Terminator", this.location);
        else throw new LarkError("invalid Token (in prefix denotation)", this.location);
    }

    infix(parser, left) { // api method

        /* This method is very similar to `prefix` above, except that it is invoked on tokens in
        the infix position (with something before them), and takes its `left` operand (as an AST
        node) instead of a context. The default implementation just throws an exception. */

        if (parser.on(Terminator)) throw new LarkError("unexpected Terminator", this.location);
        else throw new LarkError("invalid Token (in infix denotation)", this.location);
    }

    validate(validator) {

        /* This method takes a reference to the Validator Stage API, and calls this method on each
        operand, passing the API along. By itself, this just walks the AST without doing anything,
        but derived classes are able to override this method with their own validation logic, and
        then validate their operands the same way as this method to keep the recursion going. */

        for (const operand of this) operand.validate(validator);
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

        for (const arg of args) super.push(arg);

        return this;
    }

    note(...args) { // internal helper

        /* This chainable helper complements `push`, and is used to push zero or more notes to the
        `notes` set, which then act as properties of the instance.

        As a sanity check, this method also throws if the given note is not in the authorized set
        (`Token.notables`). This prevents me from creating a note named something like "map" that
        clobbers the corresponding `Array` method (again). */

        for (const arg of args) if (Token.notables.has(arg)) this.notes.add(arg);
        else throw new ReferenceError("unregistered note");

        return this;
    }

    is(...Classes) { // internal helper

        /* This helper takes any number of token subclasses, and returns `true` if `this` is an
        instance of one of the classes (or one of their subclasses), and `false` otherwise. */

        for (const Class of Classes) if (this instanceof Class) return true;

        return false;
    }

    render(upperElement=undefined) {

        /* Recursively render the token to the given HTML element (`upperElement`). Each token gets
        rendered as a `token` element that contains a `title` element with all the token's details,
        and an optional `child` element that contains the token's children when it has any (which
        is never the case when rendering the token stream from the lexer). When the token has a
        `child` element, the element is passed to each child as its `upperElement` argument.

        If `upperElement` is `undefined`, this function creates a new `child` element, and appends
        it to `document.body`. A `child` root element is required for the CSS to work correctly.

        This makes it much easier to read through the token stream or parse tree (especially as the
        `Proxy` wrapper nests everything horribly in DevTools).

        Note: The small amount of CSS required is in `index.html` (inline). As the console evolves,
        it will get broken out into separate files. */

        if (upperElement === undefined) {

            upperElement = document.createElement("child");
            document.body.append(upperElement);
        }

        const tokenElement = document.createElement("token"); // a container for everything below
        const titleElement = document.createElement("title"); // title bar [type value notes place]
        const valueElement = document.createElement("value"); // the token's `value`
        const notesElement = document.createElement("notes"); // the token's `notes`
        const placeElement = document.createElement("place"); // the token's line and column
        const childElement = document.createElement("child"); // a container for nesting children

        titleElement.innerText = this.is(Keyword) ? "Keyword" : this.constructor.name;
        valueElement.innerText = this.is(StringLiteral, TextLiteral) ? empty : this.value;
        notesElement.innerHTML = Array.from(this.notes).map(note => `<note>${note}</note>`).join(space);
        placeElement.innerText = `[${(this.location >> 8n) + 1n}:${(this.location % 256n) + 1n}]`;

        upperElement.append(tokenElement); /// <token>
        tokenElement.append(titleElement); ///   <title> Type <value/> <notes/> <place/> </title>
        titleElement.append(valueElement); ///   <child?> <token/> <token/> <token/> ... </child>
        titleElement.append(notesElement); /// </token>
        titleElement.append(placeElement);

        if (this.length === 0) return; // only include a `child` element when required...

        tokenElement.append(childElement);

        for (const operand of this) operand.render(childElement);
    }
}

// MARK: THE ABSTRACT BASE CLASSES

export class Terminal extends Token {

    /* This is the base class for all terminal tokens (including the various words, literals and
    terminators). */
}

export class Terminator extends Terminal {

    /* This is the base class for the various statement-terminators (line-feeds, commas and the
    end-of-file token (see `EOF`) implicitly inserted at the end of every token stream). It is
    imported by the Lexer Stage to tokenize terminators, as well as the Parser Stage, which
    uses it to classify them. */

    static * lex({on}, location) {

        if (on(newline)) yield new LineFeed(location, "<LF>");
        else if (on(comma)) yield new Comma(location, comma);
        else yield new EOF(location, "<EOF>");
    }
}

export class Delimiter extends Terminal {

    /* This is the abstract base class for all delimiters. The class is also imported by the
    Lexer Stage for tokenizing delimiters. */

    static * lex({read}, location) {

        const value = read();

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

    js(writer, opener, closer) {

        /* Take a reference to the Writer Stage API, an opening string and a closing string, and use
        them to output a compound expression, optionally prefixed by an expression. This is used by
        the `OpenParen` and `OpenBracket` classes to write grouped expressions, invocations, array
        literals and bracketed notation. */

        const join = operands => operands.map(operand => operand.js(writer, true)).join(comma + space);

        if (this.led) return this[0].js(writer) + opener + join(this[1]) + closer;
        else return opener + join(this[0]) + closer;
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
}

export class Word extends Terminal {

    /* This is the abstract base class for every type of word and name. It is also imported by the
    Lexer Stage for word-tokenization. */

    static * lex({advance, at, read}, location) {

        let value = read();

        while (at(wordCharacters)) value += advance();

        if (keywords.includes(value)) yield Keyword.subclass(location, value);
        else if (operators.includes(value)) yield Operator.subclass(location, value);
        else if (constants.includes(value)) yield Constant.subclass(location, value);
        else if (reserved.includes(value)) yield new Reserved(location, value);
        else yield new Variable(location, value);
    }
}

export class Constant extends Word {

    /* This is the abstract base class for constant words, (like `Infinity`, `NaN`, `true` etc).
    It is also used (internally) by the `Word` class (see the `subclass` method below). */

    safe = true;
    expression = true;

    static subclass(location, value) {

        switch (value) {
            case "false": return new FalseConstant(location, value);
            case "Infinity": return new InfinityConstant(location, value);
            case "NaN": return new NaNConstant(location, value);
            case "null": return new NullConstant(location, value);
            case "super": return new SuperConstant(location, value);
            case "this": return new ThisConstant(location, value);
            case "true": return new TrueConstant(location, value);
            case "void": return new VoidConstant(location, value);
        }
    }

    prefix() { return this }
}

export class Keyword extends Word {

    /* This abstract base class is used by all of the keyword classes. It is also used (internally)
    by the `Word` class (see `subclass`). */

    static subclass(location, value) {

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

    /* This is the abstract base class for flow-transfer statements that accept an optional
    `Variable` label (in practice, just `break` and `continue`). */

    prefix({gatherVariable, label, on}) {

        /* Gather a `break` or `continue` statement, with an optional label, validating the
        label when present. */

        if (on(Variable)) {

            this.note("labelled").push(gatherVariable());

            if (label(this[0]) !== null) return this;
            else throw new LarkError(`undefined Label '${this[0].value}'`, this[0].location);

        } else return this;
    }

    js(writer) { return `${this.spelling}${this.labelled ? space + this[0].js(writer) : empty}` }
}

export class CommandStatement extends Keyword {

    /* This is the abstract class for keywords that are followed by a required, arbitrary
    expression (`await` and `throw`). */

    prefix({gatherExpression}) { return this.push(gatherExpression()) }
}

export class Declaration extends Keyword {

    /* This abstract base class implements `let` and `var` declarations. Lark's `let` declarations
    compile to `const` with freezing, while `var compiles to `let`. For example:

        let x = y       -> const x = Object.freeze(y);
        var x = y       -> let x = y;

    Note: Lark also shadows variables that would otherwise create a temporal dead zone, so there
    is nothing like hoisting in Lark.

    Note: Freezing true primitives (which are intrinsically immutable) is always a noop. However,
    freezing a *primitive object*, freezes the object (though the resulting value is still equal
    to its primitive equivalent, according to Lark equality). */

    static declare(validator, lvalue) {

        /* Take a reference to the Validator Stage API and an lvalue belonging to a declaration
        or parameter. Note "lvalue" for the lvalue, then figure out its place within the block,
        updating the `scopestack` as required.

        If the name of the lvalue's not present in the current namespace, it's declared there
        with its value set to "<declared>".

        If the name exists in the current namespace, and has the value "<declared>" or the name
        of a register ("ƥ0", "ƥ1", "ƥ2"...), then it's a syntax error to declare it again.

        If the name exists, and the value is "<shadowed>", it's nonlocal, and there are one or
        more references to it inside some nested block above this declaration, so declaring it
        here would create a temporal dead zone. Instead, we generate a register name, then set
        the `value` property of this declaration to the register name, and store *that* value
        (instead of "<declared") in the namespace on top of the `scopestack`.

        The `Variable` class walks the `scopestack` backwards (from innermost to outermost) and
        adds its name to every namespace that does not declare it, setting the value associated
        with that name to "<shadowed>", up to (but not including) whichever namespace declares
        the name, if any do, and tagging every namespace on the `scopestack` when `Variable`
        fails to find its declaration.

        If a given `Variable` finds its declaration (and breaks), it first checks for any value
        other than "<declared>", and when present, the `Variable` instance copies the value, so
        names that use registers get copied to all of their references (updating their `value`
        properties to the name of the register). */

        const [value, scope] = [lvalue.value, validator.scopestack.top];

        if (scope[value] === "<shadowed>") { // the same name is referenced through this block

            scope[value] = Variable.register();
            lvalue.value = scope[value];

        } else if (scope[value] !== undefined) { // the name is declared or registered here

            throw new LarkError(`duplicate declaration (${lvalue.value})`, lvalue.location);

        } else scope[value] = "<declared>"; // the name is not referenced here (in any sense)

        return lvalue;
    }

    static walk(validator, lvalues, results=[]) {

        /* Take a reference to the Validator Stage API object, an lvalues token and an optional
        array of `results`. Recursively walk the `lvalues` and register any declared names with
        the `Declaration.declare` helper, gathering the declared names (each an instance of
        Variable`) into the `results` array, which is returned. */

        if (lvalues.is(Variable)) results.push(Declaration.declare(validator, lvalues));
        else for (const lvalue of lvalues) {

            if (lvalue.is(Variable, OpenBrace, OpenBracket, CompoundExpression, Parameters)) {

                Declaration.walk(validator, lvalue, results);

            } else if (lvalue.is(Assign, Spread)) {

                Declaration.walk(validator, lvalue[0], results);

            } else if (lvalue.is(Label)) {

                Declaration.walk(validator, lvalue[1], results);
            }
        }

        return results;
    }

    static validate(lvalues) {

        /* Recursively walk the lvalues, and ensure correctly positioned slurps and labels are
        validated, and that everything notes "lvalue" (which needs to happen before we validate
        the operands, else slurps and labels in breakdowns will not parse correctly).

        Note: This is used by declarations, plain assignments and for-loops, while `Parameters`
        implements its own, similar logic. */

        lvalues.note("lvalue");

        if (lvalues.is(Variable)) return;

        for (const lvalue of lvalues) {

            if (lvalue.is(OpenBrace, OpenBracket, CompoundExpression)) {

                Declaration.validate(lvalue);

            } else if (lvalue.is(Spread) && lvalue.nud && lvalue === lvalues.at(-1)) {

                Declaration.validate(lvalue.note("validated")[0]);

            } else if (lvalue.is(Label)) {

                Declaration.validate(lvalue.note("validated")[1]);
            }
        }
    }

    prefix({advance, gatherAssignees, gatherBlock, gatherExpression, on}) {

        /* Gather a `let` or `var` declaration, which always has an lvalues before the assign
        operator, and always has an expression after it. */

        this.push(gatherAssignees());

        if (on(Assign)) advance();
        else throw new LarkError("expected the Assigment Symbol", advance(false).location);

        this.push(gatherExpression());

        if (on(Then)) { advance() } else return this;

        return this.note("then_statement").push(gatherBlock(true));
    }

    validate(validator) {

        /* Validate the lvalue, then validate each operand, then call `Declaration.walk` on the
        lvalue to walk it and declare any variables.

        Note: The operands must be validated before passing the lvalue to `Declaration.walk`, else
        TDZ elimination will fail on edgecases like `let x = x` (this is analogous to `For`, which
        has the same issue with stuff like `for n in n { etc }`). */

        Declaration.validate(this[0]);

        for (const operand of this) operand.validate(validator, this);

        this.push(...Declaration.walk(validator, this[0]));
    }

    js(writer) {

        /* Render a Lark `let` or `var` declaration, freezing anything declared with `let`. Either
        declarator may include a `then` clause (`let <lvalues> = <value> then <block>`). In that
        case, this method wraps the declaration and the block that follows it in a block statement
        that ensures the declared names are cleared up as soon as flow exits the block.

        Note: When rendering declarations with `then` statements, we use `unshift` to insert `this`
        at the start of the `Block` operand (`this[2]`), then delete the "then_statement" note, so
        this instance looks like a regular declaration, then call `writeBlock` to render the block,
        and return the result. When `writeBlock` renders `this`, this method is invoked again, and
        branches to the code for declarations without `when` statements, which only references the
        first two operands (ignoring the block). The declaration's followed by the contents of the
        block, so the block that follows `then` is rendered directly below the declaration.

        Note: The recursion described above is also why noting "terminated" is deferred until after
        we write the block: If we did it before, the declaration would not be terminated properly,
        and if we omitted it entirely, the block would be followed by a redudant empty statement.

        TODO: Freezing the values of `let` declared names is broken when destructuring. */

        if (this.then_statement) {

            const block = new Block(this.location);

            iife(this, function compile(declaration) {

                /* This helper pushes the `let` or `var` statement to the nonlocal `block`, then
                either recurs on a nested declaration or stops recuring, and directly pushes the
                block that follows the declaration. The result is a single block with all of the
                declarations, followed by the block that follows `then`. This is instead of each
                declaration nesting the next declaration and the final block in a redundant tree
                of block statements. */

                const location = declaration[0].location;
                const args = declaration.slice(0, 2);
                const names = declaration.slice(3);

                if (declaration instanceof Var) block.push(new Var(location).push(...args, ...names));
                else block.push(new Let(location).push(...args, ...names));

                if (declaration[2]?.[0]?.is(Token)) {

                    if (declaration[2][0].then_statement) compile(declaration[2][0]);
                    else block.push(declaration[2][0]);
                }
            });

            return writer.writeBlock(block);

        } else {

            const [names, declarator] = [this.slice(2), this instanceof Let ? "const" : "let"];

            if (this instanceof Let && this[0].is(Variable)) {

                return `const ${this[0].value} = Object.freeze(${this[1].js(writer, true)})`;
            }

            writer.preamble(`${declarator} ${this[0].js(writer)} = ${this[1].js(writer)}`);

            if (this instanceof Let) names.map(name => writer.proamble(`Object.freeze(${name.value})`));
        }
    }
}

export class Functional extends Keyword {

    /* Used to group `Async`, `FunctionLiteral`, `ClassLiteral` and `SubclassLiteral`. */
}

export class ClassQualifier extends Keyword {

    /* This is the abstract base class for the `static` and `private` qualifiers that prefix
    declarations inside classes. */
}

export class Header extends Keyword {

    /* This is the abstract base class for statements that have a block. It is imported by the
    parser, which uses it to implement LIST correctly.*/

    notes = new Set(["terminated"]);
}

export class PredicatedBlock extends Header {

    /* This is the abstract base class for the predicated blocks (`if`, `else if`, and `while`,
    but not `else` on its own, as it has no predicate). */

    prefix({gatherBlock, gatherExpression}, context=undefined) {

        /* Gather the predicate, then the block. If the `context` is `Do`, use a functional
        block (so `return`, `yield` and `yield from` are legal). */

        return this.push(gatherExpression(), gatherBlock(context?.is(Do, OpenBrace)));
    }

    js(w) { return `${this.value} (${this[0].js(w)}) ${w.writeBlock(this[1])}` }
}

export class Operator extends Token {

    /* This is the abstract base class for all of the operator subclasses. It is used by the
    lexer to gather an unbroken sequence of our symbolic operator characters, split it into
    individual operators (see the static `slice` helper below), then yield the operators
    as concrete tokens, one at a time. */

    expression = true;

    static subclass(location, value) {

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
            case "...": return new Spread(location, value);
            case "~": return new SkipAssignee(location, value);
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
            case "and": return new And(location, value);
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
            case "then": return new Then(location, value);
            case "type": return new Type(location, value);
            case "when": return new When(location, value);
        }
    }


    static slice(value, offset, location) {

        /* Take a `value` string containing a contiguous sequence of symbolic operator characters,
        and break it into an array of individual operators, greedily, from left to right, based on
        a given starting `offset` into the string (`offset` is a `BigInt`).

        The helper repeatedly yields the longest token it can make (starting from the beginning of
        the string), before recuring on the remaining characters. If it is unable to exhaust the
        string that way, the function complains instead (requiring the `location` argument).

        This is the lexer's algorithm for symbolic operator disambiguation. */

        if (not(value)) return [];

        if (operators.includes(value)) return [value];

        if (offset > BigInt(value.length)) throw new LarkError(`invalid Operator (${value})`, location);

        const [start, end] = [value.slice(0, Number(-offset)), value.slice(Number(-offset))];

        if (operators.includes(start)) return [start, ...Operator.slice(end, 1n, location)];
        else return Operator.slice(value, offset + 1n, location);
    }

    static * lex({advance, at, read}, location) {

        /* This API method tokenizes and yields as many operators as it can gather (refer to the
        `Operator.slice` method above for an explanation of how it works). */

        let values = read();

        while (at(operationals)) values += advance();

        for (const value of Operator.slice(values, 1n, location)) {

            yield Operator.subclass(location, value);

            location += BigInt(value.length);
        }
    }

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

    prefix({gatherExpression}) { return this.push(gatherExpression(this.RBP)) }

    js(writer) { return `${this.spelling} ${this[0].js(writer)}` }
}

export class InfixOperator extends Operator {

    /* This abstract class provides general functionality for operators that are only valid in the
    infix denotation. */

    infix({gatherExpression}, left) { return this.push(left, gatherExpression(this.LBP)) }

    js(writer) { return `${this[0].js(writer)} ${this.spelling} ${this[1].js(writer)}` }
}

export class DotOperator extends InfixOperator {

    /* This abstract class provides general functionality for dot-operators, which must have a
    property name (which can be any type of word token) for their righthand operand. */

    LBP = 17;

    infix({gatherProperty}, left) { return this.push(left, gatherProperty().note("lvalue")) }

    js(writer) { return `${this[0].js(writer)}${this.spelling}${this[1].value}` }
}

export class GeneralOperator extends Operator {

    /* This abstract class provides general functionality for operators that are valid both in the
    prefix and infix denotation. It combines (by repeating the code) the functionality of both the
    `PrefixOperator` and `InfixOperator` base classes (as we cannot use multiple-inheritance) */

    prefix({gatherExpression}) { return this.push(gatherExpression(this.RBP)) }

    infix({gatherExpression}, left) { return this.push(left, gatherExpression(this.LBP)) }

    js(writer) {

        /* Render a generic prefix or infix operation, as required. */

        if (this.led) return `${this[0].js(writer)} ${this.spelling} ${this[1].js(writer)}`;

        const separator = lowers.includes(this.spelling[0]) ? space : empty;

        return `${this.spelling}${separator}${this[0].js(writer)}`;
    }
}

export class ArrowOperator extends GeneralOperator {

    /* This is the abstract base class for the arrow operators. Both operators support prefix
    and infix grammars, which are right-associative. */

    LBP = 2;

    infix({gatherExpression}, left) {

        /* This method overrides the inherited version to ensure that the `left` parameter
        (when present) is either a variable name or is wrapped in parens, and that the
        operator is right-associative. */

        const message = "the Arrow Operator requires parenthesized arguments";

        if (not(left.is(OpenParen, Variable))) throw new LarkError(message, left.location);

        return this.push(left, gatherExpression(this.LBP - 1));
    }
}

export class AssignmentOperator extends InfixOperator {

    /* This is the abstract base class used by all assignment operators, which are right-
    associative. */

    LBP = 2;

    infix({gatherExpression}, left) { return this.push(left, gatherExpression(this.LBP - 1)) }

    validate(validator) {

        /* If this instance is a plain assignment operation (using `=`, so not including inplace
        assignment operations), then iterate over the lvalues, and make sure that every nested
        breakdown notes "lvalue", and that slurps note "validated", if they're the last lvalue
        within their respective breakdown, complaining if they are not.

        Plain assignments pass their lvalues to `Declaration.walk`, unless they're qualified
        (using a breadcrumb or bracket notation) or the assignment is a parameter (as the
        `Parameter` class already declares and validates it).

        The same logic is applied to the lvalues of `let` and `var` declarations. */

        if (not(this.is(Assign))) {

            for (const operand of this) operand.validate(validator);

            return;
        }

        if (this[0].is(DotOperator) || this[0].is(OpenBracket) && this[0].led); // qualified lvalue
        else if (not(validator.paramstack.top)) { // ignoring default-argument assignments...

            Declaration.validate(this[0]);
            Declaration.walk(validator, this[0]);
        }

        for (const operand of this) operand.validate(validator);

        // now that the lvalue has been validated, check if it's a property descriptor, and if so,
        // note "property_descriptor" on this instance too, so `js(writer)` knows what to do (we
        // also accept property descriptions, as empty brackets note "property_description" even
        // though it might be a descriptor with no flags, as there's no way to know that until
        // this method establishes that the empty 'bracket notation' is being assigned to...

        if (this[0].property_descriptor || this[0].property_description) this.note("property_descriptor");
    }

    js(writer) {

        /* If the lvalue is a property descriptor, render it here, else pass `writer` along to the
        `super.js` method (for infix operators), and let that method render the operation. Special-
        casing descriptors is required as they will bind more tightly than the assignment, which is
        both required in the Lark source and redudant in the JS output. We need to turn expressions
        like `x.y.z[enumerable: true] = 1_000` into this:

            Object.defineProperty(x.y, "z", {enumerable: true, value: 1_000})

        The `OpenBracket` class implements the renderer for the `defineProperty` invocation, but is
        not able to access the assignment (its parent node) to get the expression to provide as its
        `value` key in the third argument, so must depend on this class to handle that situation as
        a special-case, synthesize an extra label (`value: 1_000`) for it, then render and return
        the `OpenBracket` instance (instead of the assignment). */

        if (this.property_descriptor) {

            const location = this[0][1].location;
            const [key, value] = [new Variable(location, "value"), this[1]];

            this[0][1].push(new Label(location).note("validated").push(key, value));

            return this[0].js(writer);

        } else return super.js(writer);
    }
}

export class GeneralDotOperator extends DotOperator {

    /* This base class extends the dot-operator class with functionality specific to the bang
    and ask operators, which are also bitwise prefix operators. */

    RBP = 14;

    prefix({gatherExpression}) { return this.push(gatherExpression(this.RBP)) }
}

// MARK: THE CONCRETE TOKEN CLASSES

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

        ch:   n => `"${n}ch"`,                      // css unit strings...
        cm:   n => `"${n}cm"`,
        em:   n => `"${n}em"`,
        ex:   n => `"${n}ex"`,
        mm:   n => `"${n}mm"`,
        p:    n => `"${n}%"`,
        pc:   n => `"${n}pc"`,
        pt:   n => `"${n}pt"`,
        px:   n => `"${n}px"`,
        rem:  n => `"${n}rem"`,
        vh:   n => `"${n}vh"`,
        vmax: n => `"${n}vmax"`,
        vmin: n => `"${n}vmin"`,
        vw:   n => `"${n}vw"`,

        K:    n => `${n * 1_000}`,                  // numeric units of magnitude...
        M:    n => `${n * 1_000_000}`,
        B:    n => `${n * 1_000_000_000}`,
        T:    n => `${n * 1_000_000_000_000}`,
        Q:    n => `${n * 1_000_000_000_000_000}`,

        n(n, float, location) {                     // bigint literals...

            if (float) throw new LarkError("invalid (fractional) BigInt Literal", location);
            else return `${n}n`;
        }
    };

    safe = true;
    expression = true;

    static * lex(...args) { yield new NumberLiteral(...args) }

    constructor({advance, at, gatherWhile, on, peek, read}, location) {

        /* This generator tokenizes a number literal, ensuring that dots are only included when
        they are valid (permitting number literals to be followed by a dot-operator without any
        parens), and handling unit prefixes and exponentiation. */

        super(location, read());

        // establish the base, and create a reference to the appropriate digit-set...

        if (on("0") && at(bases)) {

            this.value += advance();

            var [digits, isDecimal] = [on("xX") ? hexadecimal : binary, false];

        } else var [digits, isDecimal] = [decimal, true];

        // gather as many digits and underscores as possible from the appropriate set...

        gatherWhile(digits + underscore, this);

        // validate the value so far, requiring that it does not start with a zero, unless the zero
        // introduces a base-prefix or the *whole value* is a single zero, as well as checking that
        // it's not just a base-prefix (without any significant digits afterwards), nor ends on an
        // underscore separator (which is illegal in JavaScript)...

        if (isDecimal && this.value[0] === "0" && this.value !== "0") { // leading zeros...

            throw new LarkError("leading zeros are invalid", location);
        }

        if (not(isDecimal) && this.value.length === 2) { // loner base-prefix...

            if (at(decimal)) throw new LarkError("invalid digit for notation", location);
            else throw new LarkError("incomplete base-prefix", location);
        }

        if (this.value.at(-1) === underscore) { // trailing separator...

            throw new LarkError("a Number Literal cannot end on an underscore", location);
        }

        // if a dot is present and followed by a decimal digit, continue to lex this number literal
        // as a float (leaving the dot to be parsed as a breadcrumb operator otherwise)...

        if (at(dot) && isDecimal && this.value[0] !== dot && peek(2n, decimal)) {

            this.note("float_notation").value += advance();
            gatherWhile(digits + underscore, this);
        }

        if (this.float_notation && this.value.at(-1) === underscore) { // trailing separator...

            throw new LarkError("a Number Literal cannot end on an underscore", this.location);
        }

        // now, check for a numeric unit xor an exponentiation pseudo-operator - on units, use the
        // appropriate unit-helper to expand the `value` property to a JavaScript literal, and on
        // operators, lex the characters and convert to JavaScript exponentiation notation - in
        // either case, update the `value` property with the result...

        const atDouble = character => at(character) && peek(2n, character);

        if (at(alphas)) { // units...

            const unit = gatherWhile(alphas, new Token(location));
            const helper = NumberLiteral.units[unit.value];

            this.note("unit_notation").value = helper(eval(this.value), this.float_notation, location);

        } else if (atDouble(slash) || atDouble(backslash)) { // exponentiation...

            const operator = at(slash) ? "e-" : "e";
            const exponent = new Token(location);

            advance(2n);
            gatherWhile(digits, exponent);

            this.note("exponentiation_notation").value += operator + exponent.value;

        } else if (not(this.float)) this.note("integer_notation");

        // finally, check for illegal, adjacent underscore separators, and complain if found...

        if (this.value.indexOf(underscore + underscore) < 0) return;

        const index = BigInt(this.value.indexOf(underscore + underscore));

        throw new LarkError("invalid adjacent Separators", location + index);
    }

    prefix(_) { return this }
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

    static * lex({advance, at, gather, locate, on, read, terminate}, location) {

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

            while (at(character) && advance()) value += read();

            return value;
        }

        const [head, characters] = [match(quote, quote), []];
        const StringType = head.length > 1 ? TextLiteral : StringLiteral;

        advance();

        if (StringType === TextLiteral && not(on(newline))) {

            // this block throws on anything that follows the opening quotes of a text literal,
            // except for insignificant whitespace and commentary (which is discarded)...

            while (on(space)) advance();

            if (on(pound)) do { advance() } while (not(on(newline)));
            else if (not(on(newline))) throw new LarkError("invalid Text Literal", locate());
        }

        while (read()) {

            // this loop can yield any number of tokens, as it yields every token within each
            // interpolation, as well any number of substrings between them...

            if (on(backslash) && at(openParen)) {

                // this block handles streams of interpolated tokens...

                yield new StringType(location, characters.join(empty));

                advance(2n);
                characters.length = 0;

                yield new OpenInterpolation(locate());
                yield * gather(true);
                yield new CloseInterpolation(locate());

            } else if (on(quote)) {

                // this block handles one or more quotes, which may close the literal, may be
                // part of the literal, or may just be too many quotes for the literal...

                const candidate = match(quote, quote);  // potential closing quotes
                const headCount = head.length;          // number of opening quotes
                const tailCount = candidate.length;     // number of potential closing quotes

                if (tailCount > headCount) {            // too many closing quotes...

                    const message = `${headCount} Opening Quotes with ${tailCount} Closing Quotes`;

                    throw new LarkError(message, location);
                }

                if (tailCount === headCount) {          // exactly the right number of quotes...

                    return yield new StringType(location, characters.join(empty));

                } else characters.push(candidate);      // too few quotes (part of the string)

            } else if (on(newline)) {

                // this block handles newlines, followed by zero or more spaces...

                terminate();
                characters.push(read() + match(space));

            } else if (on(backtick) || on(dollar) && at(openBrace)) {

                // this block handles characters that are meaningful in a js template-literal, but
                // have no special meaning in a lark string or text literal...

                characters.push(backslash, read());

            } else characters.push(read()); // this block handles a regular character

            advance();
        }
    }

    prefix({advance, gatherCompoundExpression, on}) {

        /* Gather one or more pairs of operands, each containing an interpolation array, followed
        by a (required) `StringLiteral` substring (that the lexer ensures will be there). */

        while (on(OpenInterpolation)) {

            advance();
            this.push(gatherCompoundExpression(CloseInterpolation), advance(false));
        }

        return this;
    }

    infix(parser, left) { return this.note("tagged_template").push(left).prefix(parser) }

    js(writer) {

        /* Generate a template literal from the various parts of the string literal (text literals
        have their own `js` method), reproducing the interpolations, and possibly also including a
        tag-expression. */

        const chunks = [this.value];
        const prefix = this.tagged_template ? this.shift().js(writer) : empty;

        for (const operand of this) {

            if (operand.is(CompoundExpression)) for (const interpolation of operand) {

                chunks.push("${" + interpolation.js(writer) + "}");

            } else chunks.push(operand.value);
        }

        return prefix + backtick + chunks.join(empty) + backtick;
    }
}

export class TextLiteral extends StringLiteral {

    /* This class implements text literals, inheriting a lot from `StringLiteral`. */

    js(writer) {

        /* Generate a template literal from the various parts of the text literal, reproducing
        the interpolations, and possibly including a tag-expression. */

        const strip = string => string.replaceAll(indentation, newline);

        const indentation = iife(() => {

            /* This IIFE closure computes the indentation to remove from each line (a string that
            begins with a newline, followed by zero or more spaces), then returns it, so it gets
            assigned to `indentation` in the outer scope.

            Note: Using the `strip` helper above with the `indentation` returned here fixes every
            line, but leaves the result with a superfluous newline at the very beginning and end.
            They are manually removed (the first newline is removed at the beginning of the code,
            following this function, and the last newline towards the end (see `chunks`). */

            return this.value.slice(this.value.lastIndexOf(newline));
        });

        // initialize the array of string `chunks` using the `value` property (though removing the
        // superfluous leading newline), as well as the `prefix`, which is the tag-expression when
        // there is one, else the `empty` string...

        const chunks = [strip(this.value).slice(1)];
        const prefix = this.tagged_template ? this.shift().js(writer) : empty;

        // now, iterate over the token's (remaining) operands, convert them to js, and wrapping any
        // interpolations appropriately, before concatenating the results to `chunks`...

        for (const operand of this) {

            if (operand.is(CompoundExpression)) for (const interpolation of operand) {

                chunks.push("${" + interpolation.js(writer) + "}");

            } else chunks.push(strip(operand.value));
        }

        // finally, remove the superfluous trailing newline, then concatenate everything together,
        // before returning the resulting string...

        chunks.push(chunks.pop().slice(0, -1));

        return prefix + backtick + chunks.join(empty) + backtick;
    }
}

export class FunctionLiteral extends Functional {

    /* This class implements function literals. */

    LBP = 1;
    expression = true;

    prefix({advance, gatherBlock, gatherParameters, gatherVariable, on}, context) {

        /* This method parses functions, based on the given context (either `Async` or `undefined`,
        with the later implying no qualifier). */

        if (context?.is(Async)) this.note("async_qualifier");

        if (on(Variable)) this.push(gatherVariable());
        else this.push(new SkipAssignee(this.location));

        if (on(Of)) { advance(); return this.push(gatherParameters(), gatherBlock(true)) }
        else return this.push(new Parameters(this.location), gatherBlock(true));
    }

    validate(validator) {

        /* Push `true` to the `yieldstack` (indicating that this function are ready to be converted
        to a generator), then pop it after fixing the child operands and see if the stack-top holds
        a `Yield` instance afterwards. That will only be the case when a `yield` or `yield from`
        expression is nested below (and the instance will be the first expression encountered).

        Update the `awaitstack` based on whether this function literal expresses an async function
        (`true`) or not (`false`), so `await` statements can validate themselves.

        Update the `callstack`, so `return` statements know they're inside a function, then update
        the `blockstack` and `loopstack` so `break` and `continue` statements know that they cannot
        reach any higher block than this.

        With the stacks updated, validate the second and third operands (the parameters and block),
        but not the function's name (if any), before restoring the previous state of the stacks and
        noting "yield_qualifier" as appropriate, before returning. */

        const {awaitstack, yieldstack, blockstack, loopstack, callstack, atstack} = validator;

        awaitstack.top = this.async_qualifier;
        blockstack.top = false;
        loopstack.top  = false;
        atstack.top    = false;
        yieldstack.top = true;
        callstack.top  = true;

        for (const operand of this.slice(1)) operand.validate(validator);

        callstack.pop;
        loopstack.pop;
        blockstack.pop;
        awaitstack.pop;

        if (yieldstack.pop.is?.(Yield)) this.note("yield_qualifier");

        if (atstack.top.is?.(At)) {

            if (this[1].length === 0) {

                atstack.pop;
                this.note("at_parameter");

            } else throw new LarkError("unexpected Attribute", atstack.pop.location);
        }
    }

    js(writer, safe=false) {

        /* Render a function that may be async, a generator, both or neither. */

        const keyword = this.async_qualifier ? "async function" : "function";
        const modifier = this.yield_qualifier ? space + asterisk : empty;
        const name = this[0].is(Variable) ? space + this[0].value : empty;
        const params = this[1].map(parameter => parameter.js(writer)).join(comma + space);
        const parameters = this.at_parameter ? "...ƥargs" : params;
        const block = writer.writeBlock(this[2]);

        const javascript = `${keyword}${modifier}${name}(${parameters}) ${block}`;

        return safe ? javascript : openParen + javascript + closeParen;
    }

    static synthesize(location, body, yielding) {

        /* This static helper generates a function literal instance, based on a location, a block
        of (zero or more) statements that constitute the function body, and a boolean that's used
        to indicate whether to synthesize a generator function (`true`) or not (`false`). */

        const literal = new FunctionLiteral(location);
        const name = new SkipAssignee(location);
        const parameters = new Parameters(location);
        const block = new Block(location).push(body);

        if (yielding) literal.note("yield_qualifier");

        return literal.push(name, parameters, block);
    }
}

export class ClassLiteral extends Functional {

    /* This class implements `class` literals, which cannot extend anything, as we use `subclass`
    for that. */

    expression = true;
}

export class SubclassLiteral extends Functional {

    /* This class implements `subclass` literals, which are used to extend one class with another
    (`subclass of Foo {}` -> `class extends Foo {}`). */

    expression = true;
}

export class At extends Token {

    /* This concrete class implements at-names (like `@foo` and `@bar`), which are reserved for use
    with decorators, and attributes (like `@0` and `-@1`), which reference arguments to the current
    invocation (when the `of` part is ommited from the function header).

    Note: Signed attributes are parsed by `Plus` and `Minus`. */

    expression = true;

    static * lex({at, gatherWhile}, location) {

        /* Gather and yield an at-name xor an at-parameter. */

        const token = new At(location, atSign);

        if (at(wordInitials)) yield gatherWhile(wordCharacters, token).note("at_name");
        else if (at(decimal)) yield gatherWhile(decimal, token).note("at_parameter");
    }

    prefix() { return this }

    validate(validator) {

        if (this.at_name) throw new LarkError("decorators are not implemented yet", this.location);
        else if (not(validator.callstack.top)) throw LarkError("unexpected Attribute", this.location);
        else if (validator.atstack.top === false) validator.atstack.top = this;
    }

    js() { return `ƥargs[${this.value.slice(1)}]` }
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

    prefix({gather, on}) {

        if (on(FunctionLiteral)) return this.push(gather(0, this));
        else throw new LarkError("Async Qualifier without a Function Literal", this.location);
    }

    js(writer) { return this[0].js(writer) }
}

export class Await extends CommandStatement {

    /* This class implements the `await` operator. */

    RBP = 14;
    expression = true;

    prefix({gatherExpression}) { return this.push(gatherExpression(this.RBP)) }

    validate(validator) {

        /* Validate this `await` expression, then fix its expression node. */

        const {awaitstack, paramstack} = validator;

        if (awaitstack.top === true && paramstack.top === false) {

            for (const operand of this) operand.validate(validator);

        } else throw new LarkError("unexpected Await Operation", this.location);
    }

    js(writer) { return `await ${this[0].js(writer)}` }
}

export class Bang extends GeneralDotOperator {

    /* This class implements the `!` operator, which compiles to the bitwise `~` operator in a
    prefix context, and JavaScript's `.#` pseudo-dot-operator in an infix context. */

    spelling = ".#";
}

export class Block extends Token {

    /* Used to group statements into a block. */

    validate(validator) {

        /* Push a new namespace to the `scopestack`, then `validate` the statements inside this
        block, before popping the `scopestack`. This is enough to maintain the correct stack of
        blocks on the `scopestack`, leaving it to `Variable` and `Declaration` to eliminate any
        TDZs and that kind of thing. */

        validator.scopestack.top = Object.create(null);

        for (const operand of this) operand.validate(validator);

        validator.scopestack.pop;
    }
}

export class Break extends BranchStatement {

    /* This class implements the `break` statement. */

    validate({loopstack, blockstack}) {

        /* Validate this `break` statement (any label has already been validated). */

        if (loopstack.top) return;
        else if (blockstack.top && this.labelled) return;
        else throw new LarkError( "unexpected Break Statement", this.location);
    }
}

export class CloseBrace extends Closer {

    /* This class implements the `}` delimiter, which is used for closing blocks, as well as object
    expressions and destructured lvalues. */
}

export class CloseBracket extends Closer {

    /* This class implements the `]` delimiter, which is used for closing array expressions, as well
    as destructured lvalues. */
}

export class CloseInterpolation extends Closer {
    
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
    notes = new Set(["terminated"]);

    infix(parser, left) {

        /* If this label labels a control-flow block, use `left` to register a label as `true` (on
        a loop) or `false` (on a simple block). Then, gather the statement, before cleaning up the
        label. If it's not a control-flow block, just treat the colon like a normal infix operator,
        as it's a key-value pair (where the key and value can be arbitrary expressions). */

        const {gather, label, on} = parser;

        if (on(If, Else, While, For)) { // a control-flow label...

            if (not(left.is(Variable))) throw new LarkError("invalid Label", this.location);

            this.note("validated").expression = false;
            label(left, on(For, While));
            this.push(left, gather());
            label(left, null);

            return this;

        } else { // a key-value pair (note proto labels, which may or may not be significant)...

            super.infix(parser, left);

            if (left.is(Variable, StringLiteral) && left.length === 0 && left.value === "__proto__") {

                return this.note("proto_label");

            } else return this;
        }
    }

    js(writer) {

        if (this.validated) return `${this[0].js(writer)}: ${this[1].js(writer)}`;
        else throw new LarkError("unexpected Label", this[0].location);
    }
}

export class Comma extends Terminator {

    /* This class implements the `,` delimiter, used for separating expressions within compound
    expressions and function parameters, as well as multiple statements on the same line. */
}

export class Continue extends BranchStatement {

    /* This class implements the `continue` statement, just like JavaScript. */

    validate({loopstack}) {

        /* Validate this `continue` statement (any label has already been validated). */

        if (loopstack.top) return;
        else throw new LarkError("unexpected Continue Statement", this.location);
    }
}

export class Debug extends Keyword {

    /* This class implements the `debug` statement, which compiles to `debugger`. */

    spelling = "debugger";

    prefix() { return this }
}

export class Delete extends Keyword {

    /* This class implements the `delete` operator, which looks like JavaScript, except it requires
    the expression is (at the top) an unconditional dot operation or bracket notation. */

    LBP = 14;
    expression = true;

    prefix({gatherExpression}) {

        const expression = gatherExpression();

        if (expression.is(Dot, OpenBracket) && expression.led) return this.push(expression);
        else throw new LarkError("can only delete Properties, Keys and Indices", this.location);
    }

    js(writer) { return `delete ${this[0].js(writer)}` }
}

export class Dev extends Keyword {

    /* This class implements the `dev` qualifier, which can prefix any statement, whether formal or
    informal, ensuring it will only be included in the compiled JavaScript when the `dev` flag
    (which is `false` by default) is truthy. */

    prefix({dev, gather}) {

        /* Gather an arbitrary statement, while noting the `dev` flag, so this statement can be
        omitted from the output correctly (without the semicolon we would get if `js` simply
        returned an empty string). */

        if (dev) this.note("ignored");

        return this.push(gather());
    }

    js(writer) { return this[0].js(writer) }
}

export class Do extends Keyword {

    /* This class implements the `do` keyword, which prefixes blocks and functions to create IIFEs
    (which Lark uses as a more flexible version of block-statements). */

    expression = true;

    prefix({on, gather, gatherBlock}) {

        if (on(If, While, For, Functional)) return this.push(gather(0, this));
        else return this.push(gatherBlock(true));
    }

    validate(validator) {

        /* Configure the `yieldstack`, `awaitstack` and `callstack` just like a function literal,
        noting `yield`, as a function will need to be synthesized by the Writer Stage to compile
        the block to an IIFE (which automatically becomes a generator if the block yields). This
        allows blocks inside do-blocks to use `return`, `yield` and `yield from` statements that
        are normally only valid in `function` literals.

        Note: The actual `FunctionLiteral` class has its own, more extensive version of the logic
        in this method, as a proper `function` can be named, take arguments, use attributes (like
        `@0`) and be `async` qualified. */

        const {yieldstack, awaitstack, callstack, atstack} = validator;

        callstack.top = true;
        yieldstack.top = true;
        awaitstack.top = false;
        atstack.top = false;

        for (const operand of this) operand.validate(validator);

        callstack.pop;
        awaitstack.pop;

        if (atstack.top.is?.(At)) throw new LarkError("unexpected Attribute", atstack.pop.location);

        if (yieldstack.pop.is?.(Yield)) this.note("yield_qualifier");
    }

    js(writer, context=undefined) {

        /* Render a statement with a `do` qualifier. */

        if (this[0].is(Block)) {

            if (this.yield_qualifier) {

                if (context === true) return `function *() ${writer.writeBlock(this[0])}.call(this)`;
                else return `(function *() ${writer.writeBlock(this[0])}.call(this))`;

            } else if (context === true) {

                return `function() ${writer.writeBlock(this[0])}.call(this)`;

            } else return `(function() ${writer.writeBlock(this[0])}.call(this))`;
        }

        if (this[0].is(Functional)) return `${this[0].js(writer)}.call(this)`;

        const literal = FunctionLiteral.synthesize(this.location, this[0], this.yield_qualifier).js(writer, context);

        return `${literal}.call(this)`;
    }
}

export class Dot extends DotOperator {

    /* This concrete class implements the breadcrumb operator (`.`), which can have a number
    literal as its lefthand operand (without parens) in Lark. */

    js(writer) {

        const target = this[0].is(NumberLiteral) ? `(${this[0].js(writer)})` : this[0].js(writer);

        return `${target}${this.spelling}${this[1].value}`;
    }
}

export class Else extends PredicatedBlock {

    /* This concrete class implements `else` and `else if` clauses. */

    prefix({gather, gatherBlock, on}) {

        /* Parse an `else` or `else if` block. */

        return on(If) ? this.note("else_if").push(gather()) : this.push(gatherBlock());
    }

    validate(validator) {

        /* Add a valid level to the block stack, validate the operands, then restore the stack to
        its original state. */

        validator.blockstack.top = true;

        for (const operand of this) operand.validate(validator);

        validator.blockstack.pop;
    }

    js(writer) { return `else ${this.else_if ? this[0].js(writer) : writer.writeBlock(this[0])}` }
}

export class SkipAssignee extends PrefixOperator {

    /* This class is used to represent an empty slot in an array literal or empty lvalue in
    a sequence. Lark uses a tilde instead of an empty space: `[x, ~, ~, y]` */

    prefix() { return this }

    js() { return empty }
}

export class Equal extends InfixOperator {

    /* This concrete class implements the `==` operator, which compiles to `===`. */

    LBP = 8;

    js(writer) { return `${this[0].js(writer)} === ${this[1].js(writer)}` }
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

    js(writer) { return `Math.floor(${this[0].js(writer)} / ${this[1].js(writer)})` }
}

export class For extends Header {

    /* This class implements for-loops (old school for-loops have not been designed yet, but will
    be added in some form). */

    prefix({advance, gatherAssignees, gatherBlock, gatherExpression, on}, context=undefined) {

        /* This method parses all four for-loops. It uses `p.gatherAssignees` to avoid parsing the
        operator (`in`, `of`, `on` or `from`) as an infix. When the loop is a for-in and the target
        uses a splat operation, the spread token gets a "validated" note. */

        this.push(gatherAssignees());

        if (on(In, Of, On, From)) this.note(`for_${advance(false).value}`);
        else throw new LarkError("incomplete For Statement", this.location);

        const expression = gatherExpression();

        if (expression.is(Spread)) {

            if (this.for_in && expression.led) expression.note("validated", "for_loop");
            else throw new LarkError("unexpected Slurp", expression.location);
        }

        return this.push(expression, gatherBlock(context?.is(Do)));
    }

    validate(validator) {

        /* Add another level to the block, loop and scope stacks, before validating the iterable,
        *then* passing the lvalues to `Declaration.walk`, and *then* validating the block, before
        restoring all of the stacks to their prior state.

        Note: The sequence iterable-declaration-block is important for TDZ to work correctly with
        for-loops. In JavaScript, `for (const x of x) { etc }` is illegal, but in Lark it's valid,
        simple and intuitive: All of Lark's declarators (`let` `var` and `for`) introduce bindings
        at the exact moment they execute, and the binding does not exist prior to that point, so
        references resolve to names in outer scopes. */

        const {blockstack, loopstack, scopestack} = validator;

        loopstack.top = true;
        blockstack.top = true;
        scopestack.top = Object.create(null);
        this[1].validate(validator);
        Declaration.validate(this[0]);
        Declaration.walk(validator, this[0]);
        this[2].validate(validator);
        scopestack.pop;
        blockstack.pop;
        loopstack.pop;
    }

    js(writer) {

        /* Render the appropriate for-loop, adding the extra code that implements the operator,
        unless the iterable is a spread operation, in which case, leave it to `Spread` to render
        the iterable, which knows it's inside a for-loop. */

        const [lvalues, iterable, statements] = this;
        const block = writer.writeBlock(statements);

        if (iterable.is(Spread)) return `for (const ${lvalues.js(writer)} of ƥentries(${iterable.js(writer)})) ${block}`;

        if (this.for_in) return `for (const ${lvalues.js(writer)} of ƥvalues(${iterable.js(writer)})) ${block}`;

        if (this.for_of) return `for (const ${lvalues.js(writer)} of ƥkeys(${iterable.js(writer)}))) ${block}`;

        const keyword = this.for_from ? "for await" : "for";
        const operator = this.for_on ? "in" : "of";

        return `${keyword} (const ${lvalues.js(writer)} ${operator} ${iterable.js(writer)}) ${block}`;
    }
}

export class Freeze extends PrefixOperator {

    /* This class implements the `freeze` operator, which compiles to an invocation of
    `Object.freeze`. */

    RBP = 1;

    js(writer) { return `Object.freeze(${this[0].js(writer)})` }
}

export class From extends Keyword {

    /* This class implements the `from` keyword, used by import and export statements. */
}

export class Frozen extends Operator {

    /* This concrete class implements the `frozen` operator, used by `Is` to implement the
    `is frozen` suffix operation, which compiles to an `Object.isFrozen` invocation. */
}

export class Greater extends InfixOperator {

    /* This class implements the greater-than operator (`>`). */

    LBP = 9;
}

export class If extends PredicatedBlock {

    /* This class implements `if` statements with optional `else` clauses.

    Note: If-statements simply have one optional else-clause. The `else if` case is just the*/

    prefix(parser, context) {

        /* Parse the `if` block as a conventional predicated block, then check for an `else` clause
        (which can follow the closing brace of the `if` block, when it's braced, and can be on the
        next line (ignoring whitelines) in any case. When present, the `else` clause is gathered
        as an operand, as Lark treats clauses as part of the same statement (so any clauses are
        included as part of a single formal statement in unbraced blocks). */

        super.prefix(parser, context);

        const {advance, at, gather, on} = parser;

        if (on(Else) || on(Terminator) && at(Else)) {

            if (on(Terminator)) advance();

            return this.note("if_else").push(gather(0, context));

        } else return this;
    }

    validate(validator) {

        /* Add a valid level to the block stack, validate the operands, then restore the stack to
        its original state. */

        validator.blockstack.top = true;

        for (const operand of this) operand.validate(validator);

        validator.blockstack.pop;
    }

    js(writer) {

        /* Render the `if` block as a predicated block, adding an `else` clause when
        one is present. */

        if (this.if_else) return super.js(writer) + space + this[2].js(writer);
        else return super.js(writer);
    }
}

export class Import extends Keyword {

    /* This class implements the `import` statement, with its various grammars. */
}

export class In extends InfixOperator {

    /* This concrete class implements the `in` infix-operator, which is used for membership
    tests on collections of any type. */

    LBP = 8;

    js(writer) { return `ƥin(${this[0].js(writer)}, ${this[1].js(writer)})` }
}

export class InfinityConstant extends Constant {

    /* This class implements the `Infinity` floating-point constant. */
}

export class Is extends InfixOperator {

    /* This class implements the `is`, `is not`, `is packed`, `is sealed`, `is frozen`,
    `is not packed`, `is not sealed` and `is not frozen` operators. */

    LBP = 8;

    infix({advance, gatherExpression, on}, left) {

        /* Parse one of the infix or suffix operations that start with `is` (as listed above). */

        this.push(left);

        if (on(Not)) { this.note(`${advance(false).value}_qualifier`) }

        if (on(Packed, Sealed, Frozen)) return this.note(`${advance(false).value}_qualifier`);

        return this.push(gatherExpression(this.LBP));
    }

    js(writer) {

        /* Render an `is` or an `is not` infix operation or an `is packed`, `is not packed`,
        `is sealed`, `is not sealed`, `is frozen` or `is not frozen` suffix operation. */

        const [x, y] = this.map(operand => operand.js(writer));

        if (this.not_qualifier) {

            if (this.packed_qualifier) return `ƥisNotPacked(${x})`;
            if (this.sealed_qualifier) return `ƥisNotSealed(${x})`;
            if (this.frozen_qualifier) return `ƥisNotFrozen(${x})`;

            return `ƥisNot(${x}, ${y})`;
        }

        if (this.packed_qualifier) return `ƥisPacked(${x})`;
        if (this.sealed_qualifier) return `ƥisSealed(${x})`;
        if (this.frozen_qualifier) return `ƥisFrozen(${x})`;

        return `ƥis(${x}, ${y})`;
    }
}

export class Lesser extends InfixOperator {

    /* This class implements the less-than-operator (`<`). */

    LBP = 9;
}

export class Let extends Declaration {

    /* This class implements let-statements, which compile to const-statements that also freeze the
    initializer (see the `Declaration` base-class).*/
}

export class LineFeed extends Terminator {

    /* This class implements the line-feed characters, used to define newlines, which may or may
    not be significant, depending on the current LIST state, which is maintained by the parser
    implicitly (removing line-feed instances from the token stream as required). */

    prefix() { throw new LarkError("unexpected Newline", this.location) }

    infix() { throw new LarkError("unexpected Newline", this.location) }
}

export class LSHIFT extends InfixOperator {

    /* This class implements the `<<` infix operator (bitwise zero-shift-left). */

    LBP = 10;
}

export class Minus extends GeneralOperator {

    /* This class implements the `-` operators (unary and binary), as well as negative attributes
    (like `-@1` (which refers to the last argument)). */

    LBP = 14;
    RBP = 11;

    js(writer) {

        if (this[0].at_parameter) return `ƥargs.at(${this.value}${this[0].value.slice(1)})`;
        else return super.js(writer);
    }
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

    /* This class implements the `not` prefix operator, and the `not in` and `not of` infix
    operators. */

    LBP = 9;
    RBP = 14;
    spelling = "!";

    infix({advance, gatherExpression, on}, left) {

        /* Parse a `not in` or `not of` operation. */

        if (on(In, Of)) return this.note(`not_${advance(false).value}`).push(left, gatherExpression(this.LBP));
        else throw new LarkError("unexpected Not Operator", this.location);
    }

    js(writer) {

        /* Render a `not` prefix operation or a `not in` or `not of` infix operation. */

        if (this.not_of) return `!ƥof(${this[0].js(writer)}, ${this[1].js(writer)})`;
        else if (this.not_in) return `!ƥin(${this[0].js(writer)}, ${this[1].js(writer)})`;
        else return super.js(writer);
    }
}

export class NotEqual extends InfixOperator {

    /* This class implements the `!=` operator, which compiles to the `!==` operator. */

    LBP = 8;

    js(writer) { return `${this[0].js(writer)} !== ${this[1].js(writer)}` }
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

    /* This class implements the `of` infix operator, and is also used by functions to prefix their
    (optional) arguments, as well as `for-of` loops. */

    LBP = 8;

    js(writer) { return `Object.hasOwn(${this[1].js(writer)}, ${this[0].js(writer)})` }
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

    /* This class implements the open-brace delimiter, which is used for control-flow blocks and
    function bodies as well as set, hash, object and map literals, and object breakdowns. */

    expression = true;

    prefix({gatherCompoundExpression}) {

        /* Parse a set, a map or an object literal or breakdown, splatting all of the operands from
        the resulting `CompoundExpression` into the current instance, while noting the grammar used
        (either "set_literal" or "map_literal", leaving object literals and breakdowns implicit).

        In Lark's grammar, objects use braces, sets use brackets inside braces, and maps use braces
        inside braces. Due to recursion, each grammar naturally produces results that are nested to
        different degrees: With object literals (and breakdowns), the operands are the operands, as
        they are not nested. When it's a set, the operands we need are stored in the first operand,
        and when it's a map, we need the first operand of the first operand. Without destructuring,
        everything would be far more complicated to validate and convert to JavaScript during the
        latter stages.

        Note: The `OpenBracket` implementation keeps the `CompoundExpression` as an operand, due to
        brackets having a `left` value when used in bracket notation and the fact that brackets are
        only used to express bracket notation and array literals and breakdowns (so the resulting
        `CompoundExpression` directly contains the operands). */

        const operands = gatherCompoundExpression(CloseBrace);

        if (operands.length === 1 && operands[0].is(OpenBracket)) {

            return this.note("set_literal").push(...operands[0][0]);

        } else if (operands.length === 1 && operands[0].is(OpenBrace)) {

            return this.note("map_literal").push(...operands[0]);

        } else return this.push(...operands);
    }

    validate(validator) {

        /* Validate a set, map, object literal or object breakdown, iterating over its operands
        and checking labels (particularly `__proto__`), as well as any splats and slurps. */

        let prototypes = 0;

        if (this.set_literal) {                                     // set literals...

            if (this.lvalue) throw new LarkError("a Set cannot be an lvalue", this.location);
            else for (const operand of this) if (operand.is(Spread) && operand.led) operand.note("validated");

        } else if (this.map_literal) {                              // map literals...

            if (this.lvalue) throw new LarkError("a Map cannot be an lvalue", this.location);
            else for (const operand of this) {

                if (operand.is(Label) || (operand.is(Spread) && operand.led)) operand.note("validated");
                else throw new LarkError("expected a Label or Splat", operand.location);
            }

        } else if (this.lvalue) for (const operand of this) {       // object breakdowns...

            if (operand.is(Variable, OpenBrace, OpenBracket)) {

                operand.note("lvalue");

            } else if (operand.is(Label)) {

                const [left, right] = operand.note("validated");

                if (left.is(Variable)) left.note("lvalue");
                else if (left.is(StringLiteral, Constant));
                else if (left.is(OpenBracket) && left.nud && left.length === 1);
                else if (left.is(NumberLiteral) && left.integer_notation);
                else throw new LarkError("expected a Key or Slurp", left.location);

                if (right.is(Variable, OpenBrace, OpenBracket)) right.note("lvalue");
                else throw new LarkError("expected a Name or nested Breakdown", right.location);

            } else if (operand.is(Spread) && operand.nud) {

                if (this.at(-1) === operand) operand.note("validated");
                else throw new LarkError("a Slurp must always be the last operand", operand.location);

            } else throw new LarkError("expected a Name, Label or Slurp", operand.location);

        } else operandsLoop: for (const operand of this) {          // object literals...

            if (operand.is(Variable)) { operand.note("lvalue"); continue operandsLoop }

            operand.note("validated");

            if (operand.is(Spread) && operand.led);
            else if (operand.is(Label)) {

                const left = operand[0];

                if (left.is(Variable)) left.note("lvalue");
                else if (left.is(StringLiteral, Constant));
                else if (left.is(OpenBracket) && left.nud && left.length === 1);
                else if (left.is(NumberLiteral) && left.integer_notation);
                else throw new LarkError("expected a Key or Spread", left.location);

            } else throw new LarkError("expected a Key or Splat", operand.location);

            if (operand.proto_label) {

                if (++prototypes < 2) { this.note("oo_literal"); continue operandsLoop }

                throw new LarkError("duplicate magic __proto__ keys", operand[0].location);
            }
        }

        for (const operand of this) operand.validate(validator);
    }

    js(writer) {

        /* Render a set, map, array literal or array breakdown. */

        if (this.set_literal) {

            if (this.length === 0) return "new Set()";
            else return `new Set([${this.map(operand => operand.js(writer)).join(comma + space)}])`;

        } else if (this.map_literal) {

            if (this.length === 0) return "new Map()";

            const operands = this.map(function(operand) {

                if (operand.is(Label)) return `[${operand[0].js(writer)}, ${operand[1].js(writer)}]`;
                else return operand.js(writer);
            });

            return `new Map([${operands.join(comma + space)}])`;

        } else {

            if (this.length > 0) {

                const expressions = this.map(operand => operand.js(writer)).join(comma + space);

                if (this.lvalue || this.oo_literal) return `{${expressions}}`;
                else return `{__proto__: null, ${expressions}}`;

            } else return this.lvalue || this.oo_literal ? "{}" : "{__proto__: null}";
        }
    }
}

export class OpenBracket extends Caller {

    /* This class implements the open-bracket delimiter, which is used for array literals, array
    breakdowns, bracket notation, property descriptors and property descriptions. */

    prefix({gatherCompoundExpression}) { return this.push(gatherCompoundExpression(CloseBracket)) }

    infix({gatherCompoundExpression}, left) { return this.push(left, gatherCompoundExpression(CloseBracket)) }

    validate(validator) {

        /* Validate an array literal or breakdown, bracket notation or property descriptor, by
        iterating over its operands and validating any splats, slurps, labels etc. */

        const badSlurp = location => { throw new LarkError("unexpected Slurp", location) }
        const badSplat = location => { throw new LarkError("unexpected Splat", location) }

        const notation = this.led;
        const operands = this[notation ? 1 : 0];

        const flags = Object.freeze(new Set(["configurable", "enumerable", "writable"]));

        // begin by iterating over the operands (without recursion), branching on any operand types
        // that need to be validated, and making sure that they're valid in the current context and
        // that the operands and this instance have any notes they're going to need downstream...

        for (const operand of operands) {

            if (operand.is(Label)) {

                if (not(notation)) throw new LarkError("unexpected Label", operand[0].location);

                if (operand[0].is(Variable, StringLiteral) && flags.has(operand[0].value)) {

                    this.note("property_descriptor");
                    operand.note("validated");

                } else throw new LarkError("unrecognized Descriptor", operand[0].location);

            } else if (operand.is(Variable)) {

                if (this.lvalue) operand.note("lvalue");

            } else if (operand.is(Spread)) {

                const slurpOperation = operand.note("validated").nud;
                const splatOperation = not(slurpOperation);
                const lvalue = this.lvalue || operand.lvalue;
                const rvalue = not(lvalue);

                if (notation) slurpOperation ? badSlurp(operand.location) : badSplat(operand.location);
                else if (lvalue && slurpOperation && operands.at(-1) !== operand) badSlurp(operand.location);
                else if (lvalue && splatOperation) badSplat(operand.location);
                else if (rvalue && slurpOperation) badSlurp(operand.location);

            } else if (this.lvalue && operand.is(OpenBrace, OpenBracket)) operand.note("lvalue");
        }

        // if the method has established that the instance is bracket notation and not a property
        // descriptor, check its length is exactly `1`, and complain otherwise...

        if (notation && not(this.property_descriptor)) {

            if (operands.length < 1) this.note("property_description");
            else if (operands.length > 1) throw new LarkError("too many operands", operands[1].location);
        }

        // if the instance has been classified as a property descriptor, go over its operands again,
        // and confirm that they are all labels (in case the label that causes the bracket notation
        // to be reclassified as a descriptor appears after a non-label)...

        if (this.property_descriptor) for (const operand of operands) if (operand.is(Label)) continue;
        else throw new LarkError("expected a Label", operand.location);

        // finally, call `validate` to continue the recursion, validating the operands (as the loop
        // above only validates the operands of the compound expression, and only in that context...

        for (const operand of this) operand.validate(validator);
    }

    js(writer) {

        /* Render a property descriptor or property description, passing array literals, breakdowns
        and bracket notation up to the generic renderer that `Opener` provides (`super.js`) for
        compound expressions.

        Note: It's not possible to render property descriptors here without the `AssignmentOperator`
        class assisting, as the assigned value is that node's rvalue (this instance is its lvalue),
        so the `js` method of the `AssignmentOperator` class checks for property descriptors, and
        adds a label to the descriptor, where the key is "value" and the value is the rvalue of
        the assignment, then renders and returns this `OpenBracket` instead of an assignment.

        Note: Property descriptions do not have an assignment (and must be empty). */

        if (this.property_descriptor || this.property_description) {

            const left = this[0];
            const recurring = left.property_descriptor || left.property_description;

            if (left.is(Dot) || (left.is(OpenBracket) && left.led && not(recurring))) {

                if (this[1].map(label => label[0].value).includes("value")) {               // descriptor...

                    const target = left[0].js(writer);
                    const name = left.is(Dot) ? `\`${left[1].value}\`` : left[1][0].js(writer);
                    const descriptor = this[1].map(label => label.js(writer)).join(colon + space);

                    return `Object.defineProperty(${target}, ${name}, {${descriptor}})`;

                } else if (this.property_description) {                                    // description...

                    const target = left[0].js(writer);
                    const name = left.is(Dot) ? `\`${left[1].value}\`` : left[1][0].js(writer);

                    return `Object.getOwnPropertyDescriptor(${target}, ${name})`;

                } else throw new LarkError("Property Descriptor without Assignment", this.location); // fail

            } else throw new LarkError("required a Breadcrumb Operation or Bracket Notation", left.location);

        } else return super.js(writer, openBracket, closeBracket);
    }
}

export class OpenInterpolation extends Opener {

    /* This class is used by `StringLiteral` for delimiting the tokens within string interpolations
    (allowing them to be parsed as compound expressions). */
}

export class OpenParen extends Caller {

    /* This class implements the open-paren delimiter, which is used for grouped expressions and
    invocations. */

    prefix({gatherCompoundExpression}) { return this.push(gatherCompoundExpression(CloseParen)) }

    infix({gatherCompoundExpression}, left) { return this.push(left, gatherCompoundExpression(CloseParen)) }

    validate(validator) {

        if (this.nud) {

            if (this[0].length !== 1) throw new LarkError("unexpected Tuple Literal", this.location);

        } else for (const operand of this[1]) if (operand.is(Spread) && operand.led) operand.note("validated");

        for (const operand of this) operand.validate(validator);
    }

    js(writer) { return super.js(writer, openParen, closeParen) }
}

export class Pack extends PrefixOperator {

    /* This class implements the `pack` operator, which compiles to an invocation of
    `Object.preventExtensions`. */

    RBP = 1;

    js(writer) { return `Object.preventExtensions(${this[0].js(writer)})` }
}

export class Packed extends Operator {

    /* This class implements the `packed` operator, used by `Is` to implement the `is packed` and
    `is not packed` suffix operators, which compile to expressions using `Object.isExtensible`. */
}

export class Parameters extends Token {

    /* This class is used to group function parameters. */

    validate(validator) {

        /* Update the `paramstack`, so any `await` and `yield` expressions that've been used as
        default arguments know to complain, validate all of the operands (as declarations), then
        restore the previous state of the stack. */

        iife(this, function walk(parameters) {

            /* Recursively walk the parameters, and ensure correctly positioned slurps and labels
            are validated, and that everything notes "lvalue" (which needs to happen before we
            validate the operands, else slurps and labels in breakdowns will not parse). */

            parameters.note("lvalue", "parameter");

            if (parameters.is(Variable)) return;

            for (const parameter of parameters) {

                if (parameter.is(OpenBrace, OpenBracket, CompoundExpression)) {

                    walk(parameter);

                } else if (parameter.is(Spread) && parameter.nud && parameter === parameter.at(-1)) {

                    walk(parameter.note("validated")[0]);

                } else if (parameter.is(Assign)) {

                    walk(parameter[0]);
                }
            }
        });

        const {paramstack, scopestack} = validator;

        paramstack.top = true;
        scopestack.top = Object.create(null);
        Declaration.walk(validator, this);

        for (const operand of this) operand.validate(validator);

        scopestack.pop;
        paramstack.pop;
    }
}

export class Pass extends Keyword {

    /* This class implements the `pass` keyword, used to create an explicitly empty statement. */

    prefix() { return this }
}

export class Plus extends GeneralOperator {

    /* This class implements the `+` operators (unary and binary), as well as explicitly positive
    attributes (`+@1` etc).

    Note: In Lark, explicitly positive attributes are required when negating an argument (`-+@0`
    would negate the first argument, and `--@1` would negate the last argument, for example).
    This is one reason for the lack of `++` and `--` operators in Lark. */

    LBP = 11;
    RBP = 14;

    js(writer) {

        if (this[0].at_parameter) return `ƥargs[${this.value}${this[0].value.slice(1)}]`;
        else return super.js(writer);
    }
}

export class Private extends ClassQualifier {

    /* This class implements private-statements, used inside classes to declare private properties,
    which are referenced using the `!` infix dot operator. */
}

export class Raise extends InfixOperator {

    /* This class implements the raise operator (`**`), which is right-associative. */

    LBP = 13;

    infix({gatherExpression}, left) { return this.push(left, gatherExpression(this.LBP - 1)) }
}

export class Reserved extends Word {

    /* This class implements reserved words, which always make it as far as the parser, as they are
    valid property names (so are only invalid in any other context). */

    prefix() { throw new LarkError(`unexpected Reserved Word (${this.value})`, this.location) }

    infix() { throw new LarkError(`unexpected Reserved Word (${this.value})`, this.location) }
}

export class Return extends Keyword {

    /* This class implements the `return` keyword, which is followed by an optional expression. */

    prefix({on, gatherExpression}) {

        /* Gather a `return` statement, with an optional expression, which can optionally use a
        labelled expression. */

        return on(Terminator, Closer) ? this : this.push(gatherExpression());
    }

    validate(validator) {

        /* Validate this `return` statement, then fix its expression node. */

        if (validator.callstack.top) for (const operand of this) operand.validate(validator);
        else throw new LarkError("unexpected Return Statement", this.location);
    }

    js(writer) { return `return${this.length ? space + this[0].js(writer) : empty}` }
}

export class RSHIFT extends InfixOperator {

    /* This class implements the `>>` infix operator (bitwise zero-shift-right). */

    LBP = 10;
}

export class Seal extends PrefixOperator {

    /* This class implements the `seal` operator, which uses `Object.seal`. */

    RBP = 1;

    js(writer) { return `Object.seal(${this[0].js(writer)})` }
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

    /* This class implements the *slurp* (or *rest*) operator (`...`), which is a prefix operator
    in Lark (like JavaScript), used to gather otherwise superfluous values into an array. It also
    implements the *spread* operator, which is spelt the same (`...`), but is a suffix operator
    in Lark, used to spread the contents of an object or array across a range of slots.

    Lark also uses the spread operator to invoke the static `Object.entries` function on the target
    of a for-in loop. */

    LBP = 2;

    prefix({gather}) { return this.push(gather().note("lvalue")) }

    infix(_, left) { return this.push(left) }

    js(writer) {

        if (this.validated) {

            if (this.for_loop) return this[0].js(writer);
            else return this[0].safe ? `...${this[0].js(writer)}` : `...(${this[0].js(writer)})`;

        } else if (this.nud) {

            throw new LarkError("unexpected Slurp", this.location);

        } else throw new LarkError("unexpected Splat", this.location);
    }
}

export class Star extends InfixOperator {

    /* This class implements the star operator (`*`), used for multiplication. */

    LBP = 12;
}

export class Static extends ClassQualifier {

    /* This class implements static-statements, used inside classes. */
}

export class SuperConstant extends Constant {

    /* This class implements the `super` constant. */
}

export class Then extends Constant {

    /* This class implements the `then` operator, used by `let-when`, `var-when`, `when-then`
    etc. It's always handled by some other token. */
}

export class Type extends PrefixOperator {

    /* This class implements Lark's `type of` operator, which is similar to JavaScript's `typeof`,
    except better, supporting tag strings (including "Null" and "Undefined"), as well as API types
    (like "HTMLAudioElement" and "GPUInternalError"), hidden types (like "AsyncGeneratorFunction"),
    the names of user defined classes (like "Player" and "SpaceInvader"), as well as returning the
    type "Object" for any object with a null prototype. */

    prefix({advance, on, gatherExpression}) {

        if (on(Of)) advance();
        else throw new LarkError("expected an Of Operator", advance(false).location);

        return this.push(gatherExpression());
    }

    js(writer) { return `ƥtypeOf(${this[0].js(writer)})` }
}

export class Throw extends CommandStatement {

    /* This class implements `throw`, which is an operative keyword in Lark, but has the same
    semantics as in JavaScript. */

    expression = true;

    js(writer, context=undefined) {

        return (context === null) ? `throw ${this[0].js(writer)}` : `(()=>{throw ${this[0].js(writer)}})()`;
    }
}

export class TrueConstant extends Constant {

    /* This class implements the `true` constant. */
}

export class ThisConstant extends Constant {

    /* This class implements the `this` constant. */
}

export class Var extends Declaration {

    /* This class implements `var` declarations, which compile to `let` declarations. */
}

export class Variable extends Word {

    /* This class implements (variable) names, which can optionally prefix other names to form an
    expression that invokes the first name on the expression that the second name introduces. The
    expression may just be the second name, but can be any expression that begin with a name, and
    this works recursively (so `a b c d(1, 2, 3)` compiles to `a(b(c(d(1, 2, 3))))`). */

    static #counter = 0; // used to generate lark register names (ƥ0, ƥ1, ƥ2...) - see `register`

    static register() {

        /* This static helper generates and returns incrementing Lark register names (as strings),
        one per invocation. */

        return lark(this.#counter++);
    }

    safe = true;
    expression = true;

    prefix({gatherExpression, on}) {

        /* Gather a name, as well as the expression that follows it, if (and only if) the following
        expression also begins with a name (which also makes this expression unsafe to reuse). */

        if (not(on(Variable))) return this;

        this.safe = false;

        return this.push(gatherExpression());
    }

    validate(validator) {

        /* Update any variables that have not noted "lvalue" on their way through the parser, so
        the `scopestack` is correct. */

        const {scopestack} = validator;

        if (not(this.lvalue)) loop: for (let index = scopestack.end; index >= 0; index--) {

            const scope = scopestack[index];
            const value = scope[this.value];

            if (Object.hasOwn(scope, this.value)) {

                if (value === "<shadowed>" || value === "<declared>") break loop;
                else this.value = value;

            } else scope[this.value] = "<shadowed>";
        }

        for (const operand of this) operand.validate(validator);
    }

    js(writer) {

        /* Render a name, or an invocation of the name (passing the expression that followed it) if
        the implicit invocation grammar was accepted during the Parser Stage. */

        return this.length ? `${this.value}(${this[0].js(writer)})` : this.value;
    }
}

export class VoidConstant extends Constant {

    /* This class implements the `void` constant, which compiles to `undefined`. */
}

export class When extends Operator {

    /* This concrete class implements the when-else ternary operator (`x when y else z`),
    which is right-associative. */

    LBP = 2;

    infix({advance, on, gatherExpression}, left) {

        this.push(left, gatherExpression());

        if (on(Else)) { advance() } else throw new LarkError("incomplete When-Else Operation", this.location);

        return this.push(gatherExpression(this.LBP - 1));
    }

    js(writer) { return `(${this[1].js(writer)} ? ${this[0].js(writer)} : ${this[2].js(writer)})` }
}

export class While extends PredicatedBlock {

    /* This class implements the `while` keyword. */

    validate(validator) {

        /* Add a valid level to the block and loop stacks, affix the operands, then restore both
        stacks to their original state. */

        const {blockstack, loopstack} = validator;

        blockstack.top = true;
        loopstack.top = true;

        for (const operand of this) operand.validate(validator);

        blockstack.pop;
        loopstack.pop;
    }
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

    prefix({advance, on, gatherExpression}) {

        /* Parse a `yield` operation with an optional expression that may optionally be a labelled
        expression, or parse a `yield from` operation with a required (unlabelled) expression. */

        if (on(Terminator, Closer)) return this;

        if (on(From)) { advance(); return this.note("yield_from").push(gatherExpression()) }
        else return this.push(gatherExpression());
    }

    validate(validator) {

        /* Require that there is a function above us to convert to a generator, and that we're not
        a default argument. Then, if the function is available for conversion (`yieldstack.top` is
        `true`, so no other `yield` or `yield from` expression has converted it already), swap the
        top of the `yieldstack` with `this`. We only push `this` if the top is `true`, so that any
        problems can be immediately associated with the first yield-token that was encountered. */

        const {yieldstack, paramstack} = validator;

        if (yieldstack.length === 0  || paramstack.top === true) {

            throw new LarkError("unexpected Yield Operator", this.location);

        } else if (yieldstack.top === true) yieldstack.pop = this

        for (const operand of this) operand.validate(validator);
    }

    js(writer) {

        if (this.yield_from) return `yield * ${this[0].js(writer)}`;
        else return `yield${this.length ? space + this[0].js(writer) : empty}`;
    }
}
