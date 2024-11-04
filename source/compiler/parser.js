import { put, not, Stack } from "../compiler/helpers.js"
import { LarkError } from "../compiler/error.js"
import { empty } from "../compiler/ascii.js"
import { lex } from "../compiler/lexer.js"

import {
    Block,
    CloseBrace,
    CloseBracket,
    CloseInterpolation,
    CloseParen,
    Comma,
    CompoundExpression,
    EOF,
    Header,
    Keyword,
    Label,
    LineFeed,
    OpenBrace,
    OpenBracket,
    Operator,
    Parameters,
    SkipAssignee,
    Spread,
    Terminator,
    Variable,
    Word
} from "../language/tokens.js"

export function * parse(source, {dev=false}={}) {

    /* This generator implements the parser stage, and yields an AST node for each top-level
    statement. Like the lexer stage, the specifics of parsing any given grammar are left to the
    corresponding grammar object in `objects.js`.

    The parser API object collects references to a subset of the functions defined below, and
    every token has `prefix` and `infix` methods that take a reference to the Parser API object.
    The `infix` methods also take a reference to the lefthand operand as a second argument. */

    function * LIST(nested=true) { // internal

        /* This is the (often) recursive, block-level parsing function that wraps the Pratt parser
        to implement a statement grammar that replaces ASI with LIST (Linewise Implicit Statement
        Termination). */

        let statement, skip = false;

        while (skip || advance()) {

            skip = false;

            if (on(EOF)) break;

            if (on(CloseBrace) && nested) return advance();

            if (on(Terminator)) continue;

            yield statement = gather();

            if (on(Terminator)) continue;

            if (on(CloseBrace) && nested) return advance();

            if (statement instanceof Header && previous instanceof CloseBrace) {

                skip = true; // statements can immediately follow a braced block

            } else throw new LarkError("expected a terminator", token.location);
        }

        if (nested) throw new LarkError("nested end of file", token.location);
    }

    function advance(returnNext=true) { // api function

        /* Advance the token stream by one token, updating the nonlocal `token`, then return the
        new token if the `returnNext` argument is truthy (the default). Otherwise, return the
        token that was current when the invocation was made. */

        [previous, token] = [token, tokens.next().value];

        ignoreInsignificantNewlines();

        return returnNext ? token : previous;
    }

    function on(...Classes) { // api function

        /* Optionally take one or more `Token` subclasses, if the current `token` is one of them,
        return it. If none of the subclasses match, return `false`. If no arguments are given, the
        function acts as a getter, just returning the current `token`. */

        if (Classes.length === 0) return token;

        for (const Class of Classes) if (token instanceof Class) return Class;

        return false;
    }

    function ignoreInsignificantNewlines() { // internal

        /* Advance the parser state until the current token is significant. When newlines are
        significant, all tokens are significant, so this is a noop. Otherwise, it skips over any
        newlines, stopping on the first non-newline token. */

        if (whitespace.top === false) while (on(LineFeed)) advance();
    }

    function gather(RBP=0, context=undefined) { // api function

        /* This function implements Pratt's Algorithm. It is textbook, except for some extra
        validation that ensures two things:

        1) Only valid expressions get passed to `infix` methods. Statements that cannot also
           be expressions are immediately returned.
        2) Statements (like `break` and `return`) and expressions (like `yield`) that must
           only appear in specific contexts are validated here, and an exception is raised
           if a given construct cannot appear on the current block stack.

        Point 1 prevents a statement (that ends without an expression) from being incorrectly
        passed to an operator that immediately follows the statement, which is a syntax error,
        unless it forms a valid expression (the statement is also a valid expression).

        Point 1 eliminates the issue that `function` has in JavaScript, where functions cannot
        also be expressions when the statement begins with the function-keyword (without valid-
        ating anything that should remain invalid). This also applies to all formal expressions
        (any statment beginning with `do`, `async`, `await`, `lambda`, `yield` etc).

        Note: The distinction between formal and informal statements only applies lexically
        (to blocks without braces). In general, a statement is a statement and an expression
        is an expression, without exception; they are just not mutually exclusive.
    
        The first (required) argument is the binding power. The second (optional) argument can
        be anything, as it is simply passed to the `prefix` method of the first token in the
        expression, as a context. In practice, it is used by `Async` to contextualize the
        function statement that follows (`Async.prefix` checks the next token begins a
        function literal before it invokes this function). */

        function validate(result, note) {

            /* Use the `validate` method of the token instance to check it, and complain if
            it is not valid (given the blocks it is nested within), also noting whether it's
            in the prefix or infix denotation. */

            if (result.validate(api)) return result.note(note);
            else throw new LarkError(`unexpected ${result.value} statement`, result.location);
        }

        let current, result;

        // use `current` to track the token we need to parse next when advancing the (nonlocal)
        // parser state to the next `token` in the stream, and use `result` to store the parse
        // tree is it's generated, first by an invocation of the `current` token's `prefix`
        // method, then by zero or more iterations of the following loop (which invokes
        // the `current` token's `infix` instead)...

        current = token;
        token = advance();
        result = validate(current.prefix(api, context), "prefix");

        if (result.expression) while (RBP < token.LBP) {

            current = token;

            // check for and break on operative keywords (like `yield` and `throw`), so they still
            // act like keywords when used to begin an unbraced control-flow block, despite also
            // being operators (that always introduce valid expressions)...

            if (current instanceof Keyword) return result;

            // advance to the next token, then pass everything that has been parsed so far (as a
            // prefix) to the the `infix` method of the `current` token instance, updating the
            // `result` with whatever it returns (assuming it also validates)...

            token = advance();
            result = validate(current.infix(api, result), "infix");
        }

        return result;
    }

    function gatherExpression(RBP, context=undefined) { // api function

        /* This function wraps the Pratt function to ensure that the result is an expression
        (not a formal statement), complaining otherwise. */

        const candidate = gather(RBP, context);

        if (candidate.expression) return candidate;
        else throw new LarkError("expected an expression", candidate.location);
    }

    function gatherCompoundExpression(closer, options={}) {

        /* This API function takes a subclass of `Closer` (not an instance) that it uses to gather
        a sequence of zero or more comma-separated expressions to form a `CompoundExpression` that
        it returns.

        Note: This function also handles empty maps and hashes (which look like `[:]` and `{:}`).

        The second (optional) argument is a hash of options:

        + `labels`: A ternary value (one of `true`, `false` or `null`) that determines whether to
           require labels (`true`), reject labels (`false`) or either (`null`).
        + `singular`: A boolean that determines whether there should be exactly one operand in the
           literal (`true`) or any number (`false`).
        + `skipable`: A boolean value that determines whether `SkipAssignee` instances (`~`) are
           acceptable (`true`) or not (`false`).
        + `spreadable`: A boolean value that determines whether spread (`...`) operations are
           allowed (`true`) or not (`false`).
        + `spreadDenotation`: A ternary value that determines whether spread operations must be in
           the null denotation (`null`), left denotation (`true`) or either (`false`), where null
           equates to prefix and left equates to infix.

        If any of the rules are broken, a `LarkError` will be thrown.

        Notable conditions trigger notes to be made on the `CompoundExpression` instance (which is
        bound to `operands` locally):

        + "empty": Noted when there are no operands in the literal.
        + "proto": Noted when the literal contains a `__proto__` label (a label with a `Variable`
           instance as its lvalue that is spelled `__proto__`).
        + "protos": Noted when the literal contains more than one `__proto__` label.
        + "labelled": Noted when there is at least one labelled field (which includes an empty
           literal with an empty `Label` instance (`[:]` or `{:}`).
        + "unlabelled": Noted when there is at least one unlabelled field, or the literal is
           entirely empty (`[]` or `{}`).
        + "prefix-spread": Noted when the literal contains a prefix spread operation.
        + "suffix-spread": Noted when the literal contains a suffix spread operation.
        + "suffix-spreads": Noted when the literal contains more than one suffix spread operation.

        Note: The aim is not to fully validate the compound literal (as that's not technically
        possible at this stage). We validate as much as we can, while also providing as much
        information as possible to the caller and the Fixer Stage.

        Note: It is important to restore the previous significance of newlines *before* advancing,
        to drop both the the closing token and any insignificant newlines, immediately before
        returning the results (see `finalize` at the beginning of the implementation). */

        const finalize = () => { whitespace.pop; advance(); return operands }

        const { labels = false, singular = false, skipable = false } = options;
        const { spreadable = false, spreadDenotation = false } = options;
        const operands = new CompoundExpression(token.location);

        whitespace.top = false;

        if (on(Label)) {

            // this block handles empty hashes and maps (`[:]` or `{:}`)...

            const operand = advance(false);

            operands.note("empty", "labelled");

            if (not(on(closer))) {

                const message = `expected a ${closer.name} character`;
                throw new LarkError(message, advance(false).location);

            } else if (singular) {

                throw new LarkError("expected (exactly) one operand", operand.location);

            } else return finalize();
        }

        while (not(on(closer))) {

            const operand = gatherExpression();

            operands.push(operand);

            if (singular && operands.length > 1) { // enforce the singular rule...

                const message = "unexpected (superfluous) field (when exactly one was expected)";
                throw new LarkError(message, operand.location);
            }

            if (operand.is(Label)) { // handle labelled operands...

                if (labels === false) {

                    const message = "unexpected labelled field in unlabelled compound expression";
                    throw new LarkError(message, operand[0].location);

                } else if (operand[0].is(Variable) && operand[0].value === "__proto__") {

                    if (operands.noted("proto")) operands.note("protos");
                    else operands.note("proto");

                } else operands.note("labelled");

            } else { // handled unlabelled operands...

                if (labels === true) {

                    throw new LarkError("expected a labelled expression", operand.location);

                } else operands.note("unlabelled");

                if (operand.is(SkipAssignee) && !skipable) { // handle skip operators...

                    throw new LarkError("unexpected skip operator", operand.location);
                }

                if (operand.is(Spread)) { // handle spread operators (prefix and suffix)...

                    if (spreadable === false) {

                        throw new LarkError("unexpected spread operation", operand.location);
                    }

                    if (operand.noted("prefix")) { // prefix spreads (null denotation)...

                        if (spreadDenotation !== null) {

                            const message = "unexpected `...slurp` operation";
                            throw new LarkError(message, operand.location);
                        }

                        if (operands.noted("prefix-spread")) {

                            const message = "unexpected (superfluous) `...slurp` operation";
                            throw new LarkError(message, operand.location);

                        } else operands.note("prefix-spread");

                    } else { // suffix spreads (left denotation)...

                        if (spreadDenotation !== true) {

                            const message = "unexpected `splat...` operation";
                            throw new LarkError(message, operand.location);
                        }

                        if (operands.noted("suffix-spread")) operands.note("suffix-spreads");
                        else operands.note("suffix-spread");
                    }
                }
            }

            if (on(Comma)) { advance(); continue } else if (on(closer)) break;

            const message = `expected a ${closer.name} character`;
            throw new LarkError(message, operands.at(-1).location);
        }

        if (operands.length > 0) return finalize();

        if (singular) {

            const message = "no operands found (when exactly one was expected)";
            throw new LarkError(message, operands.location);
        }

        operands.note("empty", "unlabelled");

        return finalize();
    }

    function gatherVariable() { // api function

        /* This function gathers a single variable token, then advances the parser, before
        returning the token (which is noted before advancing), complaining if the token is
        not a variable. */

        if (on(Variable)) return advance(false);
        else throw new LarkError("expected a variable", advance().location);
    }

    function gatherProperty() { // api function

        /* This function gathers a single property token, which can be a variable name or any
        kind of word (keyword, operator name, reserved word etc). If the token is a property,
        the function advances the parser, then returns the token (noting it before advancing),
        and simply complaining otherwise. */

        if (on(Word) || (on(Operator) && token.named)) return advance(false);
        else throw new LarkError("expected a property", token.location);
    }

    function gatherAssignees() { // api function

        /* This function is used for gathering a single group of one or more assignees, which
        need to be parsed carefully, as assignments, declarations and the various for-loops
        use grammars with an operator after the assignees. */

        token.expression = false;

        if (on(Variable)) return gatherVariable();

        if (on(OpenBracket, OpenBrace)) return gather();

        throw new LarkError("invalid assignee", token.location);
    }

    function gatherParameters() { // api function

        /* This function gathers the parameters for a function into a `Parameters` instance, which
        is returned. The operands will all be valid expressions, with no empty values, but are not
        otherwise validated (as function parameters).

        Note: Lark parameters do not use parens (so are not parsed as compound expressions).

        Note: Arrow functions use `gatherCompoundExpression` (implicitly, as the arrow will not be
        encountered until the parameters have already been parsed). */

        const result = new Parameters(token.location);

        if (on(OpenBrace)) return result;

        while (true) {

            result.push(gatherExpression());

            if (on(Comma)) advance();
            else if (on(OpenBrace)) break;
            else throw new LarkError("expected a comma or block", token.location);
        }

        return result;
    }

    function gatherBlock(type) { // api function

        /* This function takes a blocktype, and pushes it to the block stack, before gathering a
        formal statement or an array of statements (of any number and kind). Once the block has
        been parsed, the block type is popped from the stack, and the parsed block is returned.

        If the blocktype is functional, this function requires that the body is wrapped in braces
        (as only control-flow statements can have unbraced blocks). */

        function gatherFormalStatement() {

            /* This function wraps the Pratt function to ensure that the result is a formal
            statement, complaining otherwise. */

            const candidate = gather();

            if (candidate instanceof Keyword) return candidate;
            else throw new LarkError("expected a formal statement", candidate.location);
        }

        const [functional, braced, block] = [type > 0, on(OpenBrace), new Block(token.location)];

        if (functional && not(braced)) throw new LarkError("expected a body", token.location);

        blocktypes.top = type;
        whitespace.top = true;

        if (functional) labelspace.top = {};

        if (braced) block.push(...LIST());
        else block.push(gatherFormalStatement());

        if (functional) labelspace.pop;

        whitespace.pop
        blocktypes.pop;

        return block;
    }

    function check(doStop, isValid, fallback=false) { // api function

        /* This function allows statements (like `return` and `break`) that can only appear in
        specific contexts to establish whether they are in a valid context.

        If no arguments are given, the function returns `true` if the `blocktypes` stack is empty
        (indicating that the parser is at the top level), and `false` otherwise.

        When two or three arguments are givenm the function walks the nonlocal `blocktypes` stack
        backwards (from innermost to outermost), and calls the `doStop` callback on each type to
        establish when to stop (it should return `true` to stop, and `false` otherwise).

        If the loop stops, the `isValid` callback is invoked on the type that was stopped on, and
        the result of that invocation (which is expected to be a bool) is returned.

        The `fallback` is returned when the stack is exhausted without a match.

        The block types are enumerated as follows:

            + loop blocks = -1
            + simple blocks = 0
            + function blocks = 1
            + async function blocks = 2
            + class blocks = 3

        It is always possible to establish the validity of a statement by walking to the last
        related block, then checking whether its enumeration is within some range, assuming a
        `fallback` argument is returned when the top-level is reached.

        Note: Reaching the top would always result in `false`, except that top-level-await is
        valid (in modules), so the fallback must be `true` in that case. */

        if (arguments.length === 0) return blocktypes.length === 0;

        for (let i = blocktypes.length - 1; i >= 0; i--) {

            if (doStop(blocktypes[i])) return isValid(blocktypes[i]);
        }

        return fallback;
    }

    function label(name, value=undefined) { // api function

        /* This function is a getter-setter, used to get and set the state of labels. It takes a
        `Varaible` instance (`name`) and an optional ternary value (one of `true`, `false` or
        `null`).

        When the second argument is left undefined (getter mode), the function returns `true` when
        the given label is active and bound to a loop block, `false` when it is active and bound
        to a simple block, and `null` when it is inactive (as it cannot be found in the hash of
        labels on top of the `labelspace` stack).

        When the second argument is set (setter mode), its value is used to update the state of the
        given label on the `labelspace` stack.

        Note: The `labelspace` is a stack that has label-hashes pushed and popped automatically (by
        `gatherBlock`) whenever entering or leaving a function body. The hashes on the stack map
        label names (as strings) to their ternary state. */

        if (value === undefined) return labelspace.top[name.value] ?? null;
        else if (value === null) delete labelspace.top[name.value];
        else labelspace.top[name.value] = value;
    }

    // initialze a hash, mapping the four notes used by compound expressions to the corresponding
    // closing token class (used internally, see `gatherCompoundExpression`)...

    const closers = Object.create(null);

    closers.parenthesized = CloseParen;
    closers.interpolated = CloseInterpolation;
    closers.bracketed = CloseBracket;
    closers.braced = CloseBrace;

    // gather the api functions and flags to form the parser api object, initialize the internal
    // state, then invoke `LIST` to yield the resulting top-level statements, one by one...

    const api = {
        advance,
        check,
        dev,
        gather,
        gatherVariable,
        gatherProperty,
        gatherExpression,
        gatherCompoundExpression,
        gatherParameters,
        gatherAssignees,
        gatherBlock,
        label,
        on
    };

    const blocktypes = new Stack();
    const labelspace = new Stack({});
    const whitespace = new Stack(true).on(false, ignoreInsignificantNewlines);

    const tokens = lex(source, {dev});

    let token, previous;

    yield * LIST(false);
}
