// this module implements all of the concrete classes in the token hierarchy...

import { put, not, iife } from "../core/helpers.js"

import { LarkError } from "../core/error.js"

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
    newline,
    openBrace,
    openBracket,
    openParen,
    quote,
    space,
} from "../core/ascii.js"

import {
    LOOPBLOCK,
    SIMPLEBLOCK,
    FUNCTIONBLOCK,
    ASYNCFUNCTIONBLOCK,
    CLASSBLOCK,
} from "../core/blocktypes.js"

import {
    ArrowOperator,
    AssignmentOperator,
    BranchStatement,
    Caller,
    ClassQualifier,
    Closer,
    CommandStatement,
    Constant,
    Declaration,
    Delimiter,
    DotOperator,
    GeneralDotOperator,
    GeneralOperator,
    Header,
    InfixOperator,
    Keyword,
    Opener,
    Operator,
    PredicatedBlock,
    PrefixOperator,
    Terminal,
    Terminator,
    Token,
    Word
} from "../user/abstract.js"
import { parse } from "../core/parser.js"

export {
    ArrowOperator,
    AssignmentOperator,
    BranchStatement,
    Caller,
    ClassQualifier,
    Closer,
    CommandStatement,
    Constant,
    Declaration,
    Delimiter,
    DotOperator,
    GeneralDotOperator,
    GeneralOperator,
    Header,
    InfixOperator,
    Keyword,
    Opener,
    Operator,
    PredicatedBlock,
    PrefixOperator,
    Terminal,
    Terminator,
    Token,
    Word
}

export class NumberLiteral extends Terminal {

    /* This is the concrete class for all number-literals (integers and floats, with or without a
    unit-suffix). It is also imported by the Lexer Stage for number tokenization.

    This class defines an associated-type, named `Unit` that is used for unit-suffixes (as in `1n`
    and `22.4KHz`). Units must immediately follow the last digit of a regular number literal, and
    when present, always consist of one or more alphas. */

    expression = true;

    static Unit = class extends Terminal {}

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

        if (lexer.at(dot)) {

            if (isDecimal && this.value[0] !== dot && lexer.peek(+2, decimal)) {

                this.value += lexer.advance();
                lexer.gatherWhile(digits, this);

            } else throw new LarkError("fractional numbers must use decimal notation", location);
        }

        // finally, check for a unit, and when present, store it in the `operands` array as an
        // instance of the associated type `NumberLiteral.Unit`...

        if (lexer.at(alphas)) {

            const unit = new NumberLiteral.Unit(location, empty);

            lexer.gatherWhile(alphas, unit);
            this.push(unit);
        }
    }

    prefix(_) { return this }

    validate(_) { return true }

    js(writer) {

        /* Render a number literal, and if it has a unit, wrap it in a unit-invocation. */

        if (this.operands.length === 0) return super.js(writer);
        else return `Number[Symbol.for(\`ƥu.${this.at(0).js(writer)}\`)].bind(${super.js(writer)})`;
    }
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

                lexer.advance();
                lexer.advance();
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

    infix(parser, prefix) {

        /* Take a prefix expression, push it to the `operands` array, then call `prefix(parser)`
        to parse the actual string literal. This exists to support tagged-literals. */

        this.push(prefix, null);

        return this.prefix(parser);
    }

    validate(_) { return true }

    js(writer) {

        /* Generate a template literal from the various parts of the string literal, reproducing
        the interpolations, and possibly including a tag-expression.
        
        Note: Text literals have their own `js(writer)` method. */

        if (this.at(1) === null) { // tagged string literal...

            var prefix = this.at(0).js(writer)
            var string = this.value;

            this.operands = this.operands.slice(2);

        } else { // plain string literal...

            var prefix = empty;
            var string = this.value;
        }

        for (const operand of this.operands) {

            if (operand instanceof Array) for (const interpolation of operand) {

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

            /* Get the value of the last string operand and the index of the last newline in that
            string. Use the results to return the indentation to remove (a newline, followed by
            zero or more spaces, as a string). */

            const value = (self.at(-1) ?? self).value;
            const index = value.lastIndexOf(newline);

            return value.slice(index);
        });

        // first, prepare the `prefix` string (either a tag-expression or an empty string) and the
        // `string` that the rest of the tokens will be concatented to, as well as the appropriate
        // slice of the `operands` array (which depends on whether the literal is tagged or not)...

        if (this.at(1) === null) { // tagged text literal...

            var prefix = this.at(0).js(writer)
            var string = strip(this.value);

            this.operands = this.operands.slice(2);

        } else { // plain text literal...

            var prefix = empty;
            var string = strip(this.value);
        }

        // remove superfluous leading newline (from replacing indentation with a newline)...

        string = string.slice(1);

        // now, iterate over the (remaining) operands, convert them to js, wrapping any
        // interpolations appropriately, and concatenate the results to `string`...

        for (const operand of this.operands) {

            if (operand instanceof Array) for (const interpolation of operand) {

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
    operator (`//=`). */
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

export class Async extends Keyword {

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

    LBP = 14;
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

        if (this.operands.length) { // labeled break...

            let label = this.operands[0];

            if (parser.label(label) === null) {

                throw new LarkError(`undefined label '${label.value}'`, label.location);
            }

            return parser.check($ => true, $ => $ < FUNCTIONBLOCK);

        } else return parser.check($ => $ !== SIMPLEBLOCK, $ => $ === LOOPBLOCK);
    }
}

export class Class extends Header {

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

export class CloseInterpolation extends Opener {
    
    /* This concrete class implements the `)` delimiter within string interpolations, and is
    used by `StringLiteral` (allowing interpolations to be parsed as compound expressions). */
}

export class CloseParen extends Closer {

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

export class Continue extends BranchStatement {

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

export class Do extends Header {

    /* This concrete class implements the `do` keyword, which prefixes blocks and functions
    to create IIFEs (which Lark uses as a more flexible version of block-statements). */

    prefix(parser) {

        /* If the next token is `async` or `function`, make this instance valid as an
        expression, then gather whatever follows. Otherwise, gather a control-flow block. */

        if (parser.on(Async, FunctionLiteral)) {
            
            this.expression = true;
            this.push(parser.gather());

        } else this.push(parser.gatherBlock(SIMPLEBLOCK));

        return this;
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

        if (parser.on(If)) return this.push(If, parser.gather());
        else return this.push(parser.gatherBlock(SIMPLEBLOCK));
    }

    js(writer) {

        if (this.at(0) === If) return `else ${this.at(1).js(writer)}`;
        else return `else ${writer.writeBlock(this.at(0))}`;
    }
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

export class FalseConstant extends Constant {

    /* This concrete class implements the `false` constant. */
}

export class FatArrow extends ArrowOperator {

    /* This concrete class implements the fat-arrow function operator (`=>`). */
}

export class Floor extends InfixOperator {

    /* This concrete class implements the floor division operator (`//`). */

    LBP = 12;

    js(writer) {

        return `Math.floor(${this.at(0).js(writer)} / ${this.at(1).js(writer)})`;
    }
}

export class For extends Header {

    /* This concrete class implements for-loops (old school for-loops have not been
    designed yet, but will be added in some form). */

    prefix(parser) {

        /* This method parses all four for-loops. It uses `parser.gatherAssignee` to
        avoid parsing the operator (`in`, `of`, `on` or `from`) as an infix. */

        this.push(parser.gatherAssignee());

        if (parser.on(In, Of, On, From)) this.push(parser.advance(true).constructor);
        else throw new LarkError("incomplete for-statement", this.location);

        return this.push(parser.gatherExpression(), parser.gatherBlock(LOOPBLOCK));
    }

    js(writer) {

        const kind = this.at(1);
        const assignees = this.at(0).js(writer);
        const keyword = kind === From ? "for await" : "for";
        const operator = kind === On ? "in" : "of";
        const block = writer.writeBlock(this.at(3));

        let expression = writer.register(this.at(2));

        if (kind === In) expression = `${expression}?.ƥvalues?.() ?? Object.values(${expression} ?? [])`;
        if (kind === Of) expression = `${expression}?.ƥkeys?.() ?? Object.keys(${expression} ?? {})`;

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

export class FunctionLiteral extends Keyword {

    /* This is the concrete class for function literals. */

    LBP = 1;
    expression = true;

    prefix(parser, context) {

        /* This method parses functions, based on the given context (either `Async` or
        `undefined`, with the later implying no qualifier). */

        let blockType = context?.is(Async) ? ASYNCFUNCTIONBLOCK : FUNCTIONBLOCK;

        // having established the blocktype, parse the optional function name, which may,
        // when present, use a computed (runtime) value...

        if (parser.on(Variable)) {

            this.push(parser.gatherVariable());

        } else if (parser.on(OpenParen)) {

            parser.advance();
            this.push(parser.gatherCompoundExpression(CloseParen));

        } else this.push(null);

        // now, handle the optional parameters and required function body...

        if (parser.on(Of)) {

            parser.advance();

            this.push(parser.gatherParameters(), parser.gatherBlock(blockType));

        } else this.push([], parser.gatherBlock(blockType));

        function walk(statements) {

            /* Takes an array of statements, walks the operands array of each statement, recurring
            on blocks and compound expressions, but ignoring anything functional, looking for any
            `yield` (or `yield from`) expressions that belong to the current function. Returns a
            bool that indicates whether it found `yield`, and by extension, whether the caller
            should compile to a generator or just a regular function. */

            for (const statement of statements) for (const operand of statement.operands) {

                if (operand instanceof Array && walk(operand)) return true;

                if (not(operand instanceof Token)) continue;

                if (operand.is(ArrowOperator, FunctionLiteral, Class)) continue;

                if (statement instanceof Yield) return true;

            } return false;
        }

        return this.push(walk(this.at(2)));
    }

    js(writer) {

        /* Render a function literal (not handling arrow grammar). */

        let name = empty;

        // having assumed that `this.at(0)` is `null` (indicating an anonymous function), now
        // check if it is an `Array` (indicating a computed name) or a `Variable` (indicating
        // a regular named-function), and update `name` accordingly...

        if (this.at(0) instanceof Array) {

            name = space + openBracket + this.at(0).at(0).js(writer) + closeBracket;

        } else if (this.at(0) instanceof Variable) name = space + this.at(0).js(writer);

        // prerender the keyword, parameters and the function body, then interpolate them into
        // a literal, and return the result...

        const keyword = "function" + (this.at(-1) ? asterisk : empty);
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

    static block = SIMPLEBLOCK;
}

export class Import extends Keyword {

    /* This conrete class implements the import-statement, with its various grammars. */

    prefix(parser) {

        this.push(parser.gatherExpression());

        if (parser.on(Comma)) this.push(parser.advance(true), parser.gatherExpression());

        if (parser.on(From)) this.push(parser.advance(true), parser.gatherExpression());

        if (parser.on(Assert)) this.push(parser.advance(true), parser.gatherExpression());

        return this;
    }
}

export class In extends InfixOperator {

    /* This concrete class implements the `in` infix-operator, which is used for membership
    tests on collections of any type. */

    LBP = 8;
}

export class InfinityConstant extends Constant {

    /* This concrete class implements the `Infinity` floating-point constant. */
}

export class Is extends GeneralOperator {

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

            return this.push(left, parser.advance(true), parser.gatherExpression(this.LBP));

        } else throw new LarkError("unexpected not-operator", this.location);
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
    Operator. disambiguation). */

    LBP = 3;    // the infix precedence applies to the nullish operator
    RBP = 14;   // the prefix precedence applies to clz32 (and equals bitwise-not)
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

export class OpenInterpolation extends Opener {
    
    /* This token type is used by `StringLiteral` for delimiting the tokens within string
    interpolations (allowing them to be parsed as compound expressions). */
}

export class OpenParen extends Caller {

    /* This concrete class implements the open-paren delimiter, which is used for grouped
    expressions and invocations. */

    prefix(parser) {

        /* This method gathers a grouped expression. */

        this.operands = parser.gatherCompoundExpression(CloseParen);
        this.check({plain: true});

        return this;
    }

    infix(parser, left) {

        /* This method gathers and validates an invocation. */

        this.push(left, OpenParen, ...parser.gatherCompoundExpression(CloseParen));
        this.check({prefixed: OpenParen, plain: true});

        return this;
    }

    js(writer) {

        /* Output a grouped expression or an invocation. */

        if (this.at(1) === OpenParen) { // invocation...

            const args = this.operands.slice(2).map(operand => operand.js(writer));

            return `${this.at(0).js(writer)}(${args.join(comma + space)})`;

        } else { // grouped expression...

            const operands = this.operands.map(operand => operand.js(writer));

            return `(${operands.join(comma + space)})`;
        }
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

export class Pass extends Keyword {

    /* This concrete class implements the `pass` keyword, used to create an explicitly
    empty statement. */

    prefix(_) { return this }
}

export class Plus extends GeneralOperator {

    /* This concrete class implements the `+` operator (prefix and infix). */

    LBP = 11;   // this is the precedence of the infix operator
    RBP = 14;   // this is the precedence of the prefix operator
}

export class Private extends ClassQualifier {

    /* This contrete class implements private-statements, used inside classes. */
}

export class Raise extends InfixOperator {

    /* This concrete class implements the exponentiation infix operator, which is right-
    associative. */

    LBP = 13;
    RBP = 12;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.RBP));
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

export class Subclass extends Header {

    /* This concrete class implements the `subclass` statement, which is used to extend one
    class with another (like `extends` in JavaScript). */

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

export class Unit extends Terminal {

    /* This concrete class implements `unit` declarations, which register number literal units in
    the global symbol registry, using the Lark units-namespace (symbol keys beginning with `ƥu.`).
    The value is always expected to be a function, whether its assigned or defined by a block. */

    prefix(parser, context=null) {

        /* Parse a `unit` statement, which either registers a function as a numeric unit. When the
        context exists (it's not the default context), it will be an instance of either `Let` or
        `Var` (indicating a declaration as well). */

        this.push(context);

        if (parser.on(Variable)) this.push(parser.advance(true));
        else throw new LarkError("incomplete unit definition", this.location);

        if (parser.on(Assign)) return this.push(parser.advance(true), parser.gatherExpression());
        else return this.push(parser.gatherBlock(FUNCTIONBLOCK));
    }

    validate(parser) { return parser.check() }

    js(writer) {

        /* Render a unit declaration, preceded by a `let` or `var` preamble when one of the two
        was used as a qualifier (to declare the function as well as register it). */

        const name = this.at(1).js(writer);

        if (this.at(2) instanceof Assign) {

            const expression = this.at(3).js(writer);

            if (this.at(0) instanceof Declaration) {

                const declarator = this.at(0) instanceof Let ? "const" : "let";

                writer.preamble(`${declarator} ${name} = ${expression}`);

                return `Number[Symbol.for(\`ƥu.${name}\`)] = ${name}`;

            } else return `Number[Symbol.for(\`ƥu.${name}\`)] = ${expression}`;

        } else {

            const block = writer.writeBlock(this.at(2));

            if (this.at(0) instanceof Declaration) {

                const declarator = this.at(0) instanceof Let ? "const" : "let";

                writer.preamble(`${declarator} ${name} = function() ${block}`);

                return `Number[Symbol.for(\`ƥu.${name}\`)] = ${name}`;

            } else return `Number[Symbol.for(\`ƥu.${name}\`)] = function() ${block}`;
        }
    }
}

export class Unless extends PredicatedBlock {

    /* This concrete class implements the `unless` keyword, which compiles to an if-not
    construct (without any else-clauses). */

    static block = SIMPLEBLOCK;
}

export class Until extends PredicatedBlock {

    /* This concrete class implements the `until` keyword, which compiles to a while-not
    construct. */

    static block = LOOPBLOCK;
}

export class Var extends Declaration {

    /* This concrete class implements var-statements, which compile to let-statements. */
}

export class Variable extends Word {

    /* This concrete class implements variable names, as a kind of word. */

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
    RBP = 1;

    infix(parser, left) {

        this.push(left, parser.gatherExpression());

        if (parser.on(Else)) parser.advance();
        else throw new LarkError("incomplete when-operation", this.location);

        return this.push(parser.gatherExpression(this.RBP));
    }

    js(writer) {

        return `${this.at(1).js(writer)} ? ${this.at(0).js(writer)} : ${this.at(2).js(writer)}`;
    }
}

export class While extends PredicatedBlock {

    /* This concrete class implements the `while` keyword. */

    static block = LOOPBLOCK;
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

        if (parser.on(From)) { parser.advance(true); this.push(From, parser.gatherExpression()) }
        else if (not(parser.on(Terminator, Closer))) this.push(parser.gatherExpression());

        return this;
    }

    validate(parser) {

        /* Climb the block stack till something functional is found, then return `true`
        if it is anything other than a class block, and `false` if it is one. */

        return parser.check($ => $ > SIMPLEBLOCK, $ => $ < CLASSBLOCK);
    }

    js(writer) {

        /* Render a `yield` expression with its optional operand, or a `yield from` expression
        with its required operand. */

        if (this.at(0) === From) return `yield * ${this.at(1).js(writer)}`;

        const expression = this.at(0) ? space + this.at(0).js(writer) : empty;

        return `yield${expression}`;
    }
}
