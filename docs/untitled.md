TODO
====

Ensure active labels are never reassigned.

Add a new API getter-setter to the Parser API named `label` that takes a label string (getter mode)
or a label string and a bool (setter mode):

		parser.label("foo")        // getter
		parser.label("foo", true)  // setter

The getter returns `true` when the given label is *active* and bound to a loop block, `false` when
the label is *active* and bound to a simple block, and `null` when the label is *inactive*.

This will allow `break` and `continue` to ensure they only reference valid labels.

JS2: JavaScriptScript
=====================

**IGNORE | THIS IS AN IDEAS-DOC WITH TODO LISTS AND API SKETCHES ETC | IT IS OUTDATED AND WRONG**

JS2 is short for JavaScriptScript (this is not final, obviously).

JS2 is an improved grammar for JavaScript, which looks vaguely similar to Swift (but without
any type annotations). JS2 compiles to regular, old JavaScript syntax.

> I'm open to supporting optional type annotations (that are validated by JS2), but haven't
> given it much thought.

Statement Grammar
-----------------

JS2 has a simple, statement grammar with curly braces. Newlines are significant (generally
terminating the current statement). Indentation is insignificant.

### Significant Whitespace

JS2 replaces Automatic Semicolon Insertion (ASI) with Linewise Implicit Statement Termination
(LIST). The logic is explained later, but it is basically the same as Python: Newlines outside
of parens, brackets or braces (that are part of the current statement) terminate the statement.

Note: JS2 does not provide a continuation symbol for explicitly continuing logical lines (like
the `\` symbol in Python). In practice, there's always a better way to do it.

Note: Enforcing (less than) 120 columns (instead of 80) in source files is recommended.

JS2 does not use semicolons. However, it is still possible to have more than one statement on
the same line. You just use commas between the statements instead. This works especially well
with inline blocks:

    if legal(character) { count += 1, yield character }

### Keywords

All formal statements start with a keyword. Block-statements use `do` (like `do { ... }`), and
empty statements use `pass`. This avoids ambiguity with informal (expression) statements. For
example, destructuring assignments never require extra parenthesis:

    {x, y, z} = object

Informal statements do not *generally* start with a keyword, but they are grammatically arbitrary
expressions, so there are exceptions. For example, `yield` is a keyword, but `yield x` is still a
valid expression, so `yield x` could appear as a formal statement (by itself) or as an expression
(within a larger statement):

    yield x                // formal statement
    x = yield x            // expression

This observation is unimportant, except in regard to the way JS2 sugars blocks and predicates.

### Blocks & Predicates

JS2 implements something similar to Eich's *paren-free mode*. Our version still drops the parens
from around predicates, but does not require braces around every block.

Parens around predicates are always optional, so this kind of thing is all good:

    if x > y { return x }

    while x > 0 { console.log(x -= 1) }

Code is structured into *function bodies* and *control flow blocks* (*bodies* and *blocks*). Bodies
require braces, but blocks are a little more flexible.

Braces around blocks become optional when the block contains a single statement that begins with a
keyword. It does not matter whether the statement is formal or informal. The parser only cares
whether the statement begins with a keyword or not.

Revisiting the previous example, we can see that the braces are not required in the first case:

    if x > y return x

The nested statement forms a control-flow block (not a function body), and is a single statement
(`return x`) that begins with a keyword (`return`). Likewise, this is also allowed:

    if x > y yield x

The same does not apply to the second example from before. This would raise a syntax error:

    if x > y console.log(x)

The braces are still required here, as the block does not begin with a keyword:

    if x > y { console.log(x) }

### Compound Statements

Compound statements (those that contain blocks of other statements, like `if`, `while`, `for` etc)
are all formal statements (so they all begin with a keyword). Therefore, when we have a block that
only contains one other block, we can inline the headers. For example:

    for row in rows for column in columns { console.log(row, column) }

    for candidate in candidates if eligible(candidate) yield candidate

Note: This can recur to any degree (space permitting).

Declarations
------------

Declarations use  `let` and `var`, which compile to `const` and `let` respectively. There is no
way access JavaScript's `var` semantics. Everything is block-scoped, and is either a constant,
declared with `let`, or a variable, declared with `var`.

    let pi = 3.141
		var xp = 0

The grammar for unpacking is copied directly from JS:

    var [x, y, z] = [0, 0, 0]

Control Flow
------------

You've already seen that JS2 treats parens around predicates as optional, and you're familiar with
rules for bodies and blocks. You've also seen examples of if-statements, while-statements etc, so
you know how that works.

For-in loops in JS2 have the same semantics as for-of loops in JS, with a slightly nicer grammar:

    for <assignees> in <expression> <block>

Note that `for` is a declarator that declares one or more constants *for* each iteration. This
compliments the two principle declarators, `let` and `var`.

The grammar of `<assignees>` is exactly the same (unpacking) grammar that is used by declarations
and assignments (`let <assignees> = <expression>`).

Note: There is currently no support for declaring a loop variable as anything other than a
block-scoped, per-iteration constant (pending usecases for the wackier stuff that JavaScript
permits).

JS2 also supports unless-branches and until-loops (the inverse of if-blocks and while-loops,
respectively), though unless-blocks do not (and will never) support else-clauses:

    until game.over { animate() }

		unless result.error return

Note: Only use `unless` or `until` if you would otherwise need to negate the entire predicate.

Note: The logical operators use words (`not`, `and` and `or`), while the bitwise operators have
been redesigned. They now use different spellings for the operators (for example, the prefix
operators `!` and `?` are used for bitwise-not and count-leading-zeros). Bitwise operations
also infer unsigned semantics (twiddling bits in twos-compliment is just confusing).

## The `of` Operator

JS2 has an `of` operator with the following grammar:

    <identifier> of <expression>

This applies a simple (always inlined) *functional operator* (defined by JS2, and specified by
the identifier) to the expression on the right (as in *f of x*). This is used to sugar common
operations on single expressions. For example:

    keys of object          -> Object.keys(object)
    descriptors of object   -> Object.getOwnPropertyDescriptors(object)

This can be especially useful inside for-in loops. For example:

    for key in keys of object { console.log(key) }
    for value in values of object { console.log(value) }
    for [key, value] in entries of object { console.log(key, value) }

The of-operator is also useful for performing membership tests on the keys or values of an object:

    key in keys of object
    index in keys of array
    value in values of object
    value in values of array

Note: The `in` operator is an infix operator that calls the `includes` method of the rvalue,
passing the lvalue as the only argument (`x in y` translates to `y.includes(x)`).

While it may not be immediately obvious, the above code has some performance implications that are
more obvious when we consider what it would compile to. Having seen that `a in b` compiles to
`b.includes(a)`, and that `keys of o` compiles to `Object.keys(o)`, then we should expect
the previous example to compile to this:

    Object.keys(object).includes(key)
    Object.keys(array).includes(index)
    Object.values(object).includes(value)
    Object.values(array).includes(value)

We can now see that we need to create a new array, containing the keys or values of the operand
object (or array), to call its `includes` method, before immediately throwing away the array
we just created, and only keeping the boolean we needed.

Creating an extra array is currently unavoidable for the third and fourth cases (which check the
values), as that's just how it's done in JavaScript, but the first two cases (checking the keys)
could be replaced with a more optimal call to `Object.hasOwn`. Fortunately, this is the kind of
thing we can reliably detect and optimize automatically:

    key in keys of object          -> Object.hasOwn(object, key)
    index in keys of array         -> Object.hasOwn(array, index)

Note: Using something like `object?key` will often be a better choice still (see the section
on *Dot Operators* below).

For performance reasons, and to provide an escape hatch, the optimizer will never compile two
or more adjacent operations into a single operation, if they are explicitly separated by
(otherwise redundant) parenthesis:

    key in (keys of object)        -> Object.keys(object).includes(key)

This only applies to the operators actually being optimized away. Wrapping the overall expression,
or any of its sub-expressions, in parenthesis has no effect on optimization:

    ((key) in keys of (object))    -> Object.hasOwn(object, key)

## JSON Operators

JS2 provides two prefix operators for doing JSON, one named `serialize` and another named
`deserialize`.

The `serialize` operator compiles to `JSON.stringify`, while `deserialize` compiles to
`JSON.parse`. For example:

    deserialize serialize o        -> JSON.parse(JSON.stringify(o))
    serialize x in serialize y     -> JSON.stringify(y).includes(JSON.stringify(x))

## Mutablity Operators

JS2 provides prefix operators for *packing*, *sealing* and *freezing* objects, with a
corresponding set for checking whether an object is *packed*, *sealed* or *frozen*.

While the terms *seal* and *freeze* should be familiar to any JS dev, the term *pack* was
introduced by JS2 to refer to `Object.preventExtensions` and `Object.isExtensible`, fixing
an inconsistency in JavaScript, where `isExtensible` returns `true` if we *can* mutate the
object, while `isSealed` and `isFrozen` return `true` if we *cannot* mutate the object.

In JS2, pack, seal and freeze (consistently) define three levels of progressively more
immutable state:

    pack o                         -> Object.preventExtensions(o)
    seal o                         -> Object.seal(o)
    freeze o                       -> Object.freeze(o)

    packed o                       -> !Object.isExtensible(o)
    sealed o                       -> Object.isSealed(o)
    frozen o                       -> Object.isFrozen(o)

## The JS Namespace

You have seen that JS2 uses names for operators (and other things) that are valid variable
names in JavaScript. If this is ever an issue, you can access any varaiable in the underlying
JavaScript namespace by referencing it as a property of the `js` namespace:

	let js.serialize = js.pack				-> const serialize = pack;

Naturally, this works for the name of the namespace too:

	js.js = js.js.js								-> js = js.js

## Function Grammar

The function grammar has been revised to decouple the convenience of the arrow-grammar from the
semantics of JavaScript's various function types (so you can declare an arrow-function with a
header-block grammar, and a full-fat function with an arrow-operator etc).

As the term *arrow-function* would be a purely syntactic distinction in JS2, each of the three
principle function types is given its own simple name and corresponding keyword:

* Arrow Functions are called *lambdas*, and use the keyword `lambda`.
* Full Fat Functions are called *functions*, and use the keyword `function`.
* Generator Functions are called *generators*, and use the keyword `generator`.

Lambdas, functions and generators each support a (paren-free) general purpose, header-block
syntax, and each has an asynchronous variant that uses `async` as a *qualifier*.

Note: In JS2, a *qualifier* is a word that prefixes some other token to qualify it in some way.
Qualifiers look a bit like named prefix operators, but they are lexically attached to the very
next token, known as the *subject* (and not some arbitrary expression). Qualifiers are used
extensively in JS2, to qualify keywords, named operators, array and object literals,
arbitrary expressions (inside parens) and even other qualifiers (recursively), though
there is currently only one example of a qualified qualifier (which you will see
in a moment).

### Lambda Expressions

The grammar for lambda-statements (which are also valid expressions) looks like this:

    lambda <params> <body>

JS2 parameters are expressed exactly as they are in JavaScript (including support for
destructuring), just without the parenthesis (meaning that they can be omitted from function
definitions entirely when there are no parameters).

As always, bodies must be wrapped in braces. For example:

    let sum = lambda x, y { return x + y }

Note: In JS2, we generally use the arrow-operators (covered below) for lambdas/functions/etc
that return a single expression, so the header-block statement-grammars require explicit
return-statements.

### Function Expressions and Generator Expressions

The grammar for functions and generators is the same, except for the respective keywords, and is
similar to the grammar for lambdas, while also permitting the function to be (directly) named:

	function [<identifier>] [of <params>] <body>
	generator [<identifier>] [of <params>] <body>

The identifier is optional. When present, it immediately follows the keyword.

The parameters are also optional. When present, they are prefixed by the `of` keyword.

As in JavaScript, we can *declare* a function (assigning a name to an otherwise anonymous
function):

    let sum = function of x, y { return x + y }

We can also *define* a function, naming it directly:

    function sum of x, y { return x + y }

We generally recommend using the first syntax to declare top-level functions, and the latter to
define named methods inside class blocks. However, we also recommend using a lambda (over a
function) wherever a lambda would suffice, so in practice, most top-level functions are
actually declared as lambdas.

Note: The compiler ensures that all function definitions are valid expressions (any extra parens
are omitted from the examples in these docs).

### The `do` Qualifier

The do-qualifier can qualify the `lambda`, `function` or `generator` keywords to convert the
corresponding function to an IIFE (immediately invoking the function, and evaluating to the
returned value). For example:

    do function animate of delta = 0 {
        requestAnimationFrame(animate)
        renderer.render(delta)
    }

Note: The "dangling dogballs operator" is legal in JS2, but only implicitly, and it's always
redundant, so please, do not do this:

    function animate of delta = 0 {
        requestAnimationFrame(animate)
        renderer.render(delta)
    }()

### The `async` Qualifier

The async-qualifier can qualify the `lambda`, `function` or `generator` keywords to create an
asynchronous version of the respective function-type. A few examples:

    let read = async lambda url {
        let response = await fetch(url)
        return await response.text()
    }

    let read = async function of url { return await (await fetch(url)).text() }

    let read = async generator of urls {

        for url in urls { let response = await fetch(url), yield await response.data() }
    }

### The `do async` Qualfied Qualifier

As mentioned, JS2 has a *qualified qualifier*. The do-async-qualifier, spelt `do async`, can
qualify `lambda`, `function` or `generator`. This does exactly what you'd expect (immediately
invokes the function and evaluates to a promise).

    let promise = do async lambda {
        let response = await fetch("/britney/hitme.mp3")
        return await response.arrayBuffer()
    }

### The Arrow Operators

The language defines two arrow operators (`->` and `=>`), which both have prefix and infix forms:

    -> <expression>                 -> () => <expression>
    => <expression>                 -> function() { return <expression> }

    (<params>) -> <expression>      -> (<params>) => <expression>
    (<params>) => <expression>      -> function(<params>) { return <expression> }

Note: The params (when present) must be in parenthesis.

Note: The expression must be an expression. It must evaluate to something, which will be returned.

Note: Other potential arrow operators have been reserved for other function types, pending a valid
usecase. However, given that the body can only ever contain a single expression, arrow operators
for the other function types seem redundant in practice.

### The `await` Operator and Asynchronous Looping

In JS2, `await` works exactly like it does JavaScript:

    await <asynchronous-expression>

When invoking an async-function that returns a normal iterator, we can traverse the iterator with a
normal for-in-statement that waits for the expression to resolve first:

    for <assignees> in await <asynchronous-expression>

When we want to iterate over the values yielded by an async-generator, we use this grammar instead:

    wait for <assignees> in <asynchronous-generator-expression>

Note: We call that a *wait-for-in-loop*.

## Yielding from Generators

JS2 uses yield-statements (which are also valid expressions), just like in JavaScript, except
that in JS2, `yield *` is spelled `yield from`:

    yield                           -> yield;
    yield item                      -> yield item;
    yield from stream               -> yield * stream;
    item = yield item               -> item = yield item;

### Comments

Line comments start on a hash character (`#`), then run to end of the line.

Multiline comments generally just use multiple (single) line-comments, except docstrings, which
use a double-quoted string literal. For example:

    let sum = lambda ...args {

        "This is how we write docstrings for functions and classes in
        JS2. This style is only used for module, class and funtion
        docsstrings, to set them apart from inline commentry."

        return args.reduce((tally, previous) -> tally + previous, 0)
    }

Note: JS2 uses `//` for floor-division:

    x // y			-> Math.floor(x / y)

### Dot Operators

JS2 has three dot operators, `.`, `!` and `?` (using `not` for boolean inversion, and
`a if b else c` for ternaries).

Dot notation uses the period character (`.`), exactly like JavaScript, for general property
access. Private access uses `!` (replacing the JavaScript spelling of `.#`), and the nullish
coalescing operator is spelt `?` (replacing `?.`):

	this.foo 		-> this.foo
	this!foo 		-> this.#foo
	this?foo		-> this?.foo

	foo?()			-> foo?.()
	foo?[x]			-> foo?.[x]

	this.foo?()		-> this.foo?.()
	this.foo?[x]		-> this.foo?.[x]

Note: The `?!` operator has been reserved (to replace `?.#`), pending a usecase. We would prefer
to only use a single-character at any point in the chain (preserving the single-dot-per-step
vibe), but we will support the following, if shown to be useful:

	this?!foo		-> this?.#foo

Note: JavaScript's binary infix nullish operator (`??`) is supported (directly):

	a ?? b			-> a ?? b

## Equality

JS2 redid JavaScript's equality operations (using a qualified `is not` operator for inversion):

	not a					-> !a
	a is b 					-> Object.is(a, b)
	a is not b				-> !(Object.is(a, b))

In JS2, qualifiers always apply whenever they can. This simple rule allows us to use qualifiers
extensively, without running into the kinds of issues CoffeeScript had, where `a is not b` and
`a isnt b` meant two different things:

	a is not b				-> !(Object.is(a, b))
	a is (not b)			-> Object.is(a, !b)
	(not a) is b			-> Object.is(!a, b)

Note: The expression on the last line could drop the parens (`not a is b`), as logical-not has
far higher precedence than equality operators. The example just illustrates that this can also
be expressed symmetrically.

The traditional equality operators are reserved for defining shallow (`==`) and deep (`===`)
equality operators later, if we ever have anything we can compile them to (there is no
runtime (or preamble), so operator semantics must always be expressed inline, using
vanilla JS).

## String Literals

You can wrap string literals in accents, single-quotes or double-quotes.

Literals that use accents or single-quotes can contain interpolations that use the same syntax
as JavaScript (a dollar-prefixed pair of (properly nested) curly braces).

Strings that use double-quotes cannot contain interpolations.

We generally use accented literals for all string-literal-expressions, and use double-quoted
literals for docstrings.

Single-quoted literals are only supported because removing them seemed overly opinionated. They
are redundant.

Double-quoted string literals can span multiple lines, and contain everything between the open
double-quote and the closing one.

Accented literals are only single-line-strings if they contain no newlines. They are
multiline-strings if the literal contains a newline before any visible character or
interpolation (you open the string literal, then immediately start a new line).

Note: Multiline, accented literals with visible characters on the first line raise a
syntax error.

Note: Trailing spaces after the opening accent (before the newline) are ignored.

The parser counts the number of spaces at the beginning of the first, new line in a multiline,
accented literal (immediately following the line that the literal began on), and ignores that
many spaces at the start of that line, and every line that follows it.

Note: The number of spaces can be zero.

Note: If you need to begin a multiline string with whitespace, use an interpolation (at least,
at the beginning).

Note: The first character after the spaces on the first, new line must not be a (hard) newline
character (it may be one or more *interpolated* whitespace characters). It is a syntax error to
begin a multiline string with a whiteline.

If the last line of a multiline, accented literal only contains whitespace, it is ignored.

For example, this string contains one line (`spam and eggs`), with no leading or trailing spaces
in the value of the expression:

		let string = """
				spam and eggs
				"""

		let string = '''
		spam and eggs
		'''

		let string = ```
				spam and eggs, then so more
				this is some more data, obviously
				```

## Compound Expressions

The *subject* of a qualifier (the token being qualified) can be any valid word (a keyword, named
operator or another qualifier, but not a variable name or reserved word).

The subject of a qualifier can also be an open paren, bracket or brace. In which case, the
qualifier applies to the entire expression (up to the corresponding (properly matched) closing
paren, bracket or brace).

Unqualfied compound expressions work exactly as you'd expect from JavaScript, compiling to
grouped expressions, array literals and object literals. However, there is one exception here:
Objects defined with object literals have null prototypes by default:

	let o = {x: 1}							-> const o = {__proto__: null, x: 1}

If you want a different prototype, you have to set it explicitly:

	let oo = {__proto__: Object, x: 1}		-> const oo = {__proto__: Object, x: 1}

Qualified compund expressions are used to create other kinds of compound types (typed arrays,
maps etc):

	let bytes = u8 [1, 2, 3]				-> const bytes = new Uint8Array([1, 2, 3]);
	let data = map {"x": 1, 2: 3}			-> const data = new Map([["x", 1], [2, 3]]);

To provide for this functionality (and the fact that destructuring assignments do not require
parenthesis), the JS2 Parser treats (parenthesized) *sequence expressions* and (bracketed)
*array expressions* as sequences of (zero or more) comma-separated, arbitrary expressions, and
(braced) *object expressions* are, likewise, parsed as sequences of comma-separated pairs of
colon-separated expressions.

The keys in an object expression are interpreted and compiled, based on the qualifier (or the
absence of one). This means that certain types of keys will be valid in, for example, an object
expression with the map-qualifier that are not valid in a regular (unqualified) object literal,
and others that will be interpreted differently:

	{x: 1}									-> {x: 1}
	map {x: 1}								-> ReferenceError: x is not defined
	map {Function: f}						-> new Map([[Function, f]])
	{Function: f}							-> {Function: f}

## Literate Programming

Literate programming allows you to separate all of your commentry and docstring from your code,
generally improving the readability of both.

The JS2 Parser has a literate programming flag, which causes files to be parsed using the
literate syntax, which is very simple: Lines that begin with whitespace are code lines and
every other line is commentary.

Code lines are parsed as normal (leading whitespace is insignificant, even in multiline strings).
Everything else is ignored (and thrown away by the parser).

We do not stipulate that comments must be Markdown, or anything like that. You are free to put
anything you like on comment lines, and to use them however you wish.

Note: This simple approach limits you to simple markup. For example, nested bullet points or
HTML sections would not work, even if they started onside and contained no whitelines, as any
indented lines would be interpreted as JS2 code.









For Example:

	const skillset = a |> (predicate ? % : alternative) |> %.skills;

	let skillset = a >> $ if predicate else alternative >> $.skills

	a >> b		-> b(a)
	a >>> b(%)	-> b(a)

	getUserData(user) | processUserData | console.log

	getUserData(user) || processUserData($) || console.log($)

	getUserData(user) || processUserData($) | console.log

	let validateArray = generator rows, columns {

		for r in rows for c in cols if invalid(r, c) throw new CustomError(r, c)
		else yield {row: r, column: c}
	}

	for (const r of rows) for (const c of cols) if (invalid(r, c)) throw new RangeError(message);
	else yield {row: r, column: c};

	if x > 0 console.log(x)						-> ERROR: subordinate statement cannot be an expression

	if x > 0: console.log(x) 					-> if (x > 0) console.log(x);

	if x > 0 put x								-> if (x > 0) console.log(x);

	if x > 0 put x, y, z							-> ERROR: unexpected or superfluous comma

	if x > 0 put [x, y, z]						-> if (x > 0) console.log([x, y, z]);

	if x > 0: console.log(x, y, z)				-> if (x > 0) console.log(x, y, z);

	if x > 0									-> ERROR: expected block (found newline)
	if x > 0:									-> ERROR: expected block (found newline)

	if x > 0 pass

	put js.put									-> console.log(put)
	js.peg = js.js								-> peg = js;
	import { js.foo as bar } from "./main.js"		-> import { foo as bar } from "./main.js";
	export js.not								-> export not;

	let a = I64 [1, 2, 3]
	let b = record {a: 3, b: 2}

	let sum = closure ...args {
		let result = 0
		for arg in args: result += arg
		return result
	}

	let sum = function {
		const args = Array.from(arguments)
		return args.reduce((arg, tally) -> tally + arg, 0)
	}

	let enumerate = generator tally=0, step=1 {	-> const enumerate = function * (tally=0, step=1) {
		yield tally								-> 	   yield tally;
		repeat if x > 4 yield tally += step		->     while (true) if (x > 4) yield tally += step;
	}											-> }

	let read = async function url {
		let response = await fetch(url)
		return await response.text()
	}

	let erm = async generator x {
		# need to come up with a short example
	}

The `do` qualifier is always less offensive:

	if predicate do function { ... }

Note; More functional languages often use`do` as a general purpose invocation operator. However,
here it is a *qualifier*, and is strictly used to prefix statements that *define* functions, and cannot prefix
an arbitray expression (whether it evaluates to a function or not). Neither of the following lines parse:

	let context = do new AudioContext
	let oscillator = do context.createOscillator

Note: You cannot use `do` with arrow syntax.

	let Sprite = class {
		static directory = "../sprites"
	}

	let SpaceInvader = class extends Sprite {

		static sfx = new SFX("pew")

		static put this.sfx
		static put super.directory

		static if this.sfx.name is "pew" and super.directory is "../sprites" { ... }
		else { ... }

		static foo
		static {
			let x = 0
			this.foo = x
		}
	}

	Object..shuffle 				-> Object.prototype.shuffle
	Object.!shuffle				-> Object.prototype.#shuffle
	Object.?shuffle				-> Object.prototype?.shuffle
	Object.?[x]					-> Object.prototype?.[x]
	Object.?()					-> Object.prototype?.()
	Object.?!shuffle				-> Object.prototype?.#shuffle

## Exception Handling

	sketchy if predicate return dodgyFunction()
	on error if predicate { ... }
	capture error on ReferenceError { ... }
	capture { stacktrace } on RangeError { ... }

	capture error trying return dodgyFunction()
	on TypeError { ... }
	on { stacktrace } from ReferenceError { ... }
	on error in [InputError, ParseError] { ... }
	on in [InputError, ParseError] { ... }
	on Error { ... }
	whatever { ... }

	catch { stacktrace } from TypeError

	on <ErrorClass>
	on <param> from <ErrorClass>
	on in <ErrorClassArray>
	on <param> in <ErrorClassArray>

	try { dodgyFunction() }
	catch error {
		const $$predicate = [SyntaxError, RangeError];
		if (Array.isArray($$.predicate) && $$.predicate.includes(error.constructor) || )
		} else if (error instanceof $$predicate)
	}

	on SyntaxError then {
		# can handle error, but not access it...
	}
	capture error SyntaxError: dodgyFunction()
	on SyntaxError then {
		# can handle and reference error as `error`...
	}

	capture { stacktrace } trying: dodgyFunction()
	on SyntaxError, RangeError then {
		# can handle error and access the `stacktrace`
		# attribute as `statcktrace`...
	}

	do function animate of delta=performance.now() {	-> void function animate(delta=performance.now()) {
		requestAnimationFrame(animate)				->    requestAnimationFrame(animate);
		renderer.render()							->	  renderer.render();
	}			  									-> }();

# Membership Tests

	a in b					-> b.includes(a)
	a not in b				-> !(b.includes(a))

## Functional Stuff


## METHODS ??

	method getNames of void { ... }
	method setLocation of x, y { ... }

	method <name> of <args> { ... }
	generator method <name> of <args> { ... }
	async method <name> of <args> { ... }
	async generator method <name> of <args> { ... }

	getter <name> { ... }
	setter <name> of <arg> { ... }
	async getter <name> { ... }
	async setter <name> of <arg> { ... }

# Iteration

	for <params> in <expression> <block> 		-> for (const <param> of <expression>) { <block> }

Currently, there is no support for loop variables that vary within a given iteration, or reusing nonlocal
variables etc (pending a usecase).

Examples:

	let array = [1, 2, 3]
	let object = {a: 1, b: 2}

	for n in array: put(n)
	1
	2
	3

	for key in keys of object: put(key)
	"a"
	"b"

	for [index, value] in entries of array: put(index, value)
	0 1
	1 2
	2 3

	for [key, value] in entries of object: put(key, value)
	"a" 1
	"b" 2

	for <names> in array: ...				#   for (const $MCFLY23 of array) {
													const <names> = $MCFLY23;
													...
												}

	for <names> in keys of obj: ...			# 	for (const $MCFLY24 of Object.keys(obj)) {
													const <names> = $MCFLY24;
													...
												}

	for <names> in entries of obj: ...		# 	for (const $MCFLY25 of Object.entries(obj)) {
													const <names> = $MCFLY25;
													...
												}

	for <names> in names of obj: ...			# 	for (const $MCFLY26 of Object.getOwnPropertyNames(obj)) {
													const <names> = $MCFLY26;
													...
												}

	for <names> in symbols of obj: ...			# 	for (const $MCFLY27 of Object.getOwnPropertySymbols(obj)) {
													const <names> = $MCFLY27;
													...
												}

	for <names> in descriptors of obj: ...	# 	for (const $MCFLY28 of Object.entries(Object.getOwnPropertyDescriptors(obj)) {
													const <names> = $MCFLY28;
													...
												}

## Iteration

McFly rethinks the whole `in` header-block grammars for iteration, for-in loops and a for-in-of loops.

1) The `equals` operator is defined using the deep-equals logic
   from [How to Get a Perfect Deep Equal in JavaScript][1].
2) The `mimics` operator is defined using the shallow-equals
   logic from [How to Get a Perfect Deep Equal in JavaScript][1].

[1]: https://levelup.gitconnected.com/how-to-get-a-perfect-deep-equal-in-javascript-b849fe30e54f


## Switch Blocks and Case Expressions

	switch predicate {

		let a = true if case 0 else false

		if case 1: put "you're dead!!"
		else if case(2, 3, 4): put "you survived!"
		else: put "you defeated the goblin!!"

		let x = 20
		until case x or x is 0 {
			put "case is not {x}"
			x -= 1
		}
	}

	let bigLongFunctionName = function 						...
		a=someLongDefaultArgumentName,						...
		b=some(long, compounded, expression) + withExtras, 	...
		c=[a, literal, expressing, some, compound, value] {
			put(a, b, c)
	}

	[1 to 10]
	let pots = [2 ** n for n in [0 to 32 by 2]]

	or			-> ||
	and			-> &&
	not			-> !

	OR			-> |
	XOR			-> ^
	AND			-> &
	NOT			-> ~
	ROR?
	ROL?
	ASR?
	ZSR?
	ZSL?
	CLZ

	function classifyWord(token) {

		const value = token.value;

		if (keywords.includes(value)) token.type = "keyword";
		else if (operators.includes(value)) token.type = "operator";
		else if (reserved.includes(value)) token.type = "reserved";
		else token.type = "variable";

		return token;
	}

	function specialCaseNot(token) {

		/* The `not` keyword can be a prefix or suffix, but cannot appear by itself.
		This helper throws a syntax error if `not` appears somewhere it shouldn't. */

		if (token.value === "not") throw new SyntaxError("there is no not-operator");
	}

	while (token = gatherToken()) {

		// only special-case words, which defer full classification to permit this loop
		// to combine certain pairs of tokens (`is not`, `not in` etc) into a single
		// token (all other token types are classified as they are tokenized)...

		if (token.type !== "word") { yield token; continue }

		// if this word token cannot be a prefix, it only needs classifying...

		if (not(token.value in prefixes)) { yield classifyWord(token); continue }

		// the word could be a prefix, so we need to gather another token...

		const nextToken = gatherToken();

		// if the next token is not a word, this token cannot prefix it, so classify
		// this token, then yield both tokens, one by one, in order, then continue...

		if (nextToken.type !== "word") {

			specialCaseNot(token); // `not` is only valid as part of two-word pair

			yield classifyWord(token); yield nextToken; continue;
		}

		// we now know that both tokens are words (of some type, but importantly, the
		// values of these tokens are not inside comments or string literals), so if
		// this token can prefix the next, they need to be combined into a single
		// token that is then classified and yielded, before continuing...

		if (prefixes[token.value].includes(nextToken.value)) {

			token.value = `${token.value} ${nextToken.value}`;

			yield classifyWord(token); continue;
		}

		// while both tokens are words, they do not combine to form a single token, so
		// check that we are not yielding an invalid `not` operator, before classifying
		// and yielding each token, one by one, in order...

		specialCaseNot(token);

		yield classifyWord(token); yield classifyWord(nextToken);
	}

### Revised Bitwise Operators

Note: Logic uses `and`, `or` and `not`.

+ `!`       Bitwise NOT                         ! x         -> ~x
+ `?`       Count Leading Zeros                 ? x         -> Math.clz32(x)

+ `&`       Bitwise AND                         x & y       -> x & y
+ `|`       Bitwise OR                          x | y       -> x | y
+ `||`      XOR                                 x || y      -> x ^ y
+ `<<`      Zero Left Shift                     x << y      -> x << y
+ `>>`      Zero Right Shift                    x >> y      -> x >>> y
+ `>>>`     Arithmetic Right Shift              x >>> y     -> x >> y

+ `&=`      Bitwise AND Assignment
+ `|=`      Bitwise OR Assignment
+ `||=`     XOR Assignment
+ `<<=`     Zero Shift Left Assignment
+ `>>=`     Zero Shift Right Assignment
+ `>>>=`    Arithmetic Shift Right Assignment

### Operators

++ Addition (+)
+ Addition assignment (+=)
+ Assignment (=)
+ await
+ Bitwise AND (&)
+ Bitwise AND assignment (&=)
+ Bitwise NOT (~)
+ Bitwise OR (|)
+ Bitwise OR assignment (|=)
+ Bitwise XOR (^)
+ Bitwise XOR assignment (^=)
++ Comma operator (,)
+ Conditional (if-else) operator
+ Decrement (--)
+ delete operator
+ Destructuring assignment
++ Division (/)
++ Floor Division
+ Division assignment (/=)
--- Equality (==)
++ Exponentiation (**)
+ Exponentiation assignment (**=)
++ Greater than (>)
++ Greater than or equal (>=)
++ Grouping operator ( )
++ import.meta
+ import()
+ in operator
++ JS2-in operator
+ JS2-of operator
+ Increment (++)
--- Inequality (!=)
+ instanceof
+ Left shift (<<)
+ Left shift assignment (<<=)
++ Less than (<)
++ Less than or equal (<=)
+ Logical AND (&&)
+ Logical AND assignment (&&=)
++ Logical NOT (!)
+ Logical OR (||)
+ Logical OR assignment (||=)
++ Multiplication (*)
+ Multiplication assignment (*=)
+ new operator
++ new.target
++ null
+ Nullish coalescing assignment (??=)
++ Nullish coalescing operator (??)
++ Operator precedence parens
++ Optional chaining (?.)
++ Property accessors
++ Remainder (%)
+ Remainder assignment (%=)
+ Right shift (>>)
+ Right shift assignment (>>=)
+ Spread syntax (...)
++ Strict equality (===)
++ Strict inequality (!==)
++ Subtraction (-)
+ Subtraction assignment (-=)
+ super
+ this
+ typeof
++ Unary negation (-)
++ Unary plus (+)
+ Unsigned right shift (>>>)
+ Unsigned right shift assignment (>>>=)
+ void operator
+ yield // TODO: implement as an expression
++ yield from

### Declarations

+ var
+ let
+ for
+ for-in
+ for-async-in

### Control Flow

++ break
++ continue
+ switch
+ throw
+ try
+ catch
+ finally
++ if
++ else
++ unless
++ while
++ until
++ pass
++ do
++ return
++ yield
++ yield from

### Functions

+ lambda
+ function
+ generator
+ async lambda
+ async function
+ async generator
+ do lambda
+ do function
+ do generator
+ do async lambda
+ do async function
+ do async generator
+ class

## Misc

++ debug
+ import
+ export
+ labels
