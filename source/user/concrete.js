// this module implements all of the concrete classes in the token hierarchy...

import { LarkError } from "../core/error.js"

import {
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
    GENERATORBLOCK,
    ASYNCGENERATORBLOCK,
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
    Functional,
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
    Functional,
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

    Characters that need escaping (grave accents, dollars followed by opening braces etc) are all
    escaped by the lexer stage (see `static * lex` below). */

    LBP = 16;
    expression = true;

    constructor(location, value, head) {

        /* Take the usual values, plus the `head` of the literal (the single quote or three or more
        quotes that opened the literal). Pass the regular arguments to `super`, then push the `head`
        to the `operands` array. */

        super(location, value);
        this.push(head);
    }

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

        lexer.advance();

        while (lexer.read()) {

            // this loop can yield any number of tokens, as it yields every token within each
            // interpolation, plus one or more substrings...

            if (lexer.on(backslash) && lexer.at(openParen)) {

                // this block handles streams of interpolated tokens...

                yield new StringLiteral(location, characters.join(empty), head);

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

                    yield new StringLiteral(location, characters.join(empty), head);
                    return;

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
        the interpolations, and possibly including a tag-expression. */

        const strip = string => textLiteral ? string.replaceAll(indentation, newline) : string;

        const [textLiteral, indentation] = function(self) {

            /* Get the value of the last string operand, assuming there is one, and get the index
            of the last newline in that string, assuming there is one. Use the results to return 
            a bool that indicates whether to strip indentation from the start of every string
            operand (which is `true` for text literals and `false` otherwise), as well as
            the indentation to remove (a newline, followed by zero or more spaces).

            Note: The results are only valid when the literal is a text literal, but `strip` will
            return its argument unchanged otherwise, so that's ok. */

            const value = self.at(-1)?.value;
            const index = value?.lastIndexOf(newline);

            return [self.at(0).length > 1, value?.slice(index)];

        }(this); // iffe

        // first, prepare the `prefix` string (either a tag-expression or an empty string) and the
        // `string` that the rest of the tokens will be concatented to, as well as the appropriate
        // slice of the `operands` array (which depends on whether the literal is tagged or not)...

        if (this.at(2) === null) { // tagged literal...

            var prefix = this.at(1).js(writer)
            var string = strip(this.value);

            this.operands = this.operands.slice(3);

        } else { // plain string literal...

            var prefix = empty;
            var string = strip(this.value);

            this.operands = this.operands.slice(1);
        }

        // remove superfluous leading newline from text literals...

        if (textLiteral) { string = string.slice(1) }

        // now, iterate over the (remaining) operands, convert them to js, wrapping
        // any interpolations appropriately, and concatenate the results to `string`...

        for (const operand of this.operands) {

            if (operand instanceof Array) for (const interpolation of operand) {

                string += "${" + interpolation.js(writer) + "}";

            } else string += strip(operand.value);
        }

        // remove the superfluous trailing newline from text literals, then concatenate
        // everything together, and return the result...

        if (textLiteral) { string = string.slice(0, -1) }

        return prefix + backtick + string + backtick;
    }
}

export class AllConstant extends Constant {

    spelling = "*";

    /* This concrete class implements the `all` constant, used instead of an asterisk
    in import and export statements. */
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

export class Await extends CommandStatement {

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

export class DefaultConstant extends Constant {

    /* This concrete class implements the `default` constant, used by import and export
    statements. */
}

export class Delete extends CommandStatement {

    /* This concrete class implements the `delete` operator, exactly like JavaScript. */

    LBP = 14;
    expression = true;
}

export class Dev extends Keyword {

    /* This concrete class implements the `dev` qualifier, which can prefix any statement,
    formal or informal, ensuring it will only be included in the compiled output when the
    parser is in `devmode`. */// TODO: add a `devmode` parameter to each stage

    prefix(parser) {

        return this.push(parser.gather());
    }
}

export class Do extends Header {

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
}

export class From extends Keyword {

    /* This concrete class implements the `from` keyword, used by import and export
    statements. */
}

export class Frozen extends Operator {

    /* This concrete class implements the `frozen` operator, used by `Is` to implement the
    `is frozen` suffix operation, which compiles to an `Object.isFrozen` invocation. */
}

export class FullFunction extends Functional {

    /* This is the concrete class for function statements, which are also expressions. */

    prefix(parser, context) {

        /* This method parses functions, based on the given context (either an instance of
        `Async` or `undefined`). */

        let blockType = context?.is(Async) ? ASYNCFUNCTIONBLOCK : FUNCTIONBLOCK;

        return this.gatherFullHeader(parser, blockType);
    }
}

export class Generator extends Functional {

    /* This is the concrete class for generator statements, which are also expressions. */

    prefix(parser, context) {

        /* This method parses generators, based on the given context (which may be an
        instance of `Async` or `undefined`). */

        let blockType = context?.is(Async) ? ASYNCGENERATORBLOCK : GENERATORBLOCK;

        return this.gatherFullHeader(parser, blockType);
    }
}

export class GlobalConstant extends Constant {

    /* This concrete class implements the `global` constant, which compiles to
    `globalThis`. */
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

        if (this.at(1).is(Not)) {

            if (this.at(2).is(Packed)) return `Object.isExtensible(${this.at(0).js(writer)})`;
            if (this.at(2).is(Sealed)) return `!(Object.isSealed(${this.at(0).js(writer)}))`;
            if (this.at(2).is(Frozen)) return `!(Object.isFrozen(${this.at(0).js(writer)}))`;

            const [value, type] = [writer.register(this.at(0)), writer.register(this.at(2))];

            return `!(${value}?.ƥis?.(${type}) ?? ${value} instanceof ${type})`;
        }

        if (this.at(1).is(Packed)) return `!(Object.isExtensible(${this.at(0).js(writer)}))`;
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
    and generators to prefix their (optional) arguments, as well as `for-of` loops. */

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

        /* Output a grouped expression or an invocation (using the `js` method inherited from
        the `Opener` base class, unless the `value` string is "__proto__". In which case, the
        first operand gets expanded to a prototype declaration.
        
        Note: It is a syntax error to have more than one expression in a prototype expression. */

        if (this.value === "__proto__") return this.value + colon + space + this.at(0).js(writer);
        else return super.js(writer, openParen, closeParen);
    }
}

export class Pack extends PrefixOperator {

    /* This concrete class implements the `pack` operator, which compiles to an invocation
    of `Object.preventExtensions`. */

    RBP = 1;
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

    prefix(parser) {

        if (parser.on(Static, Local)) return this.push(parser.gather());
        else throw new LarkError("incomplete private-declaration", this.location);
    }
}

export class Raise extends InfixOperator {

    /* This concrete class implements the exponentiation infix operator, which is right-
    associative. */

    LBP = 13;

    infix(parser, left) {

        return this.push(left, parser.gatherExpression(this.LBP - 1));
    }
}

export class RandomConstant extends Constant {

    /* This concrete class implements the `random` constant, which always evaluates to a
    random `Number` between `0` and `1`, compiling to a `Math.random` invocation. */
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
}

export class RSHIFT extends InfixOperator {

    /* This concrete class implements the `>>` infix operator (bitwise zero-shift-right). */

    LBP = 10;
}

export class Seal extends PrefixOperator {

    /* This concrete class implements the `seal` operator, which compiles to an invocation
    of `Object.seal`. */

    RBP = 1;
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

        if (!parser.on(Of)) this.push(parser.gatherVariable());

        if (parser.on(Of)) {

            parser.advance();

            return this.push(parser.gatherExpression(), parser.gatherBlock(CLASSBLOCK));

        } else throw new LarkError("incomplete subclass", parser.advance(true).location);
    }
}

export class SuperConstant extends Constant {

    /* This concrete class implements the `super` constant. */
}

export class ThisConstant extends Constant {

    /* This concrete class implements the `this` constant. */
}

export class Throw extends CommandStatement {

    /* This concrete class implements the `throw` statement. */
}

export class TrueConstant extends Constant {

    /* This concrete class implements the `true` constant. */
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

    infix(parser, left) {

        this.push(left, parser.gatherExpression());

        if (parser.on(Else)) parser.advance();
        else throw new LarkError("incomplete when-operation", this.location);

        return this.push(parser.gatherExpression(this.LBP - 1));
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
