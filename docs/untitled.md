# JSII: JavaScript with Hindsight

JSII is short for JavaScript 2, and is normally pronounced *Jessie* (like Toy Story 2).

JSII provides a nice, new JavaScript syntax that compiles to regular, old JavaScript syntax, minus the warts and all.

## Statement Grammar

JSII has a simple, statement grammar with curly braces.

All *formal statements* start with a *keyword*, while *informal statements* (expression statements) never contain any keywords (they are just expressions). This rule has a few implications:

1) Block-statements start with a keyword (they use `do { ... }`).
2) Empty statements also start (and end) with a keyword (they use `pass`).
3) Because `yield` is a keyword (used to begin yield-statements and yield-from-statements), JSII has a *trade-operator* for yield-expressions. See the section Yielding & Trading for more information.

Note: JSII does not have (and will never have) a do-while statement.

Note: Per Item 1, destructuring assignments do not require parenthesis. For example, this is a legal expression (and therefore, a valid informal statement, when on its own line):

	{x, y, z} = object

JSII also supports unless-branches and until-loops (the inverse of if-blocks and while-loops, respectively), though unless-blocks do not support else-clauses.

## Statement Termination

Automatic Semicolon Insertion (ASI) has been replaced with significant newlines. The logic is explained elsewhere, but it basically just works.

The language does not use semicolons. However, you can still have more than one statement on the same line (using commas to separate them instead). You can also join two or more lines to form a longer logical line with an explicit *continuation indicator*.

## Paren-Free Mode

JSII implements a variation of Eich's *paren-free mode*, without requiring braces around every block.

Predicates never require parens. The following statements are legal:

	if x > y { return x }

	while x > 0 { console.log(x -= 1) }

JSII tidies up some edgecases regarding blocks: In JSII, all code-blocks are either *function bodies* (or just *bodies*) that must always be wrapped in braces, or *control flow blocks* (or just *blocks*) that can either be a single formal statement (starting with a keyword), or any number of statements (of any kind) wrapped in braces.

Revisiting the previous example, we can see that the braces are not required in the first case:

	if x > y return x

However, this would be illegal:

	if x > y console.log(x)

An informal statement must always be wrapped in braces:

	if x > y { console.log(x) }

Statements can (still) be recursively subordinated by simple concatentation, so this kind of thing is legal:

	while x >= 0 if x % 2 { console.log(x) }

Note: You are still free to wrap predicate expressions in logically redudant parens wherever it improves readability, as with any expression.

JSII uses a slightly modified grammar for iteration:

	for <param> in <expression> <block>

For example:

	for row in rows for column in columns { console.log(row, column) }

Note that JSII uses for-in grammar to express a JavaScript for-of loop (and redefines the `in` operator elsewhere).

Also note that `for` is a declarator that declares a constant *for* each iteration. This compliments the two principle declarators, `let` and `var`, which compile to `const` and `let` respectively.

Note: There is currently no support for declaring a loop variable as anything other than a block-scoped, per-iteration constant (pending usecases for the wackier stuff that JavaScript permits).

## The `of` Operator

JSII has an `of` operator with the following grammar:

	<identifier> of <expression>

This applies a simple (always inlined) *language function*, identified by the lefthand identifier to the expression on the right (as in *f of x*). This can be used to transform values at runtime. For example:

	keys of object				->	Object.keys(object)
	descriptors of object		->	Object.getOwnPropertyDescriptors(object)

This can be especially useful inside for-in loops. For example:

	for key in keys of object { console.log(key) }
	for value in values of object { console.log(value) }
	for [key, value] in entries of object { console.log(key, value) }

The of-operator is also useful for performing membership tests on the keys or values of an object:

	key in keys of object
	index in indices of array
	value in values of object

Note: The `indices` language function is just an alias for the `keys` function, and was provided to improve readability when applied to arrays.

While it may not be obvious if you're new to JSII, the above code has some performance ramifications that are more obvious when we consider what it would compile to. Having seen that `a in b` compiles to `b.includes(a)`, and that `keys in o` compiles to `Object.keys(o)`, then we should expect the previous example to compile to this:

    Object.keys(object).includes(key)
    Object.keys(array).includes(index)
    Object.values(object).includes(value)

Now, it should be pretty obvious that we need to create a new array (from the keys or values of the object), just to call its `includes` method (because that's how `in` works), to get back a bool, before immediately throwing away the array we just created.

Creating an extra array is currently unavoidable for the third case (checking the values), as that's just how it's done in JavaScript, but the first two cases (checking the keys) could be replaced with a more optimal call to `Object.hasOwn`. Fortunately, this is the kind of thing we can reliably detect and optimize automatically:

    key in keys of object						-> Object.hasOwn(object, key)
	index in indices of array					-> Object.hasOwn(array, index)
    value in values of object					-> Object.values(object).includes(value)

Note: Using something like `object?key` will often be a better choice still (see the section on*Dot Operators* below).

For performance reasons, and to provide an escape hatch, the optimizer will never compile two or more adjacent operations into a single operation, if they are explicitly separated by (otherwise redundant) parenthesis:

    key in (keys of object)						-> Object.keys(object).includes(key)

This only applies to the operators actually being optimized away. Wrapping the overall expression, or any of its sub-expressions, in parenthesis has no effect on optimization:

    ((key) in keys of (object))					-> (Object.hasOwn((object), (key)))

## JSON Operators

JSII provides two prefix operators for doing JSON, one named `serialize` and another named `deserialize`.

The `serialize` operator compiles to `JSON.stringify`, while `deserialize` compiles to `JSON.parse`. For example:

	deserialize serialize object					-> JSON.parse(JSON.stringify(object))
	serialize {a: 1} in serialize {a: 1, b: 2}	-> JSON.stringify({a: 1, b: 2}).includes(JSON.stringify({a: 1}))

## Mutablity Operators

JSII provides prefix operators for *packing*, *sealing* and *freezing* objects, with a corresponding set for checking whether an object is *packed*, *sealed* or *frozen*.

While the terms *seal* and *freeze* should be familiar to any JS dev, the term *pack* was introduced by JSII to refer to `Object.preventExtensions` and `Object.isExtensible`, fixing an inconsistency in JavaScript, where `isExtensible` returns `true` if we *can* mutate the object, while `isSealed` and `isFrozen` return `true` if we *cannot* mutate the object.

In JSII, pack, seal and freeze (consistently) define three levels of progressively more immutable state:

	pack object									-> Object.preventExtensions(object)
	seal object									-> Object.seal(object)
	freeze object								-> Object.freeze(object)

	packed object								-> !Object.isExtensible(object)
	sealed object								-> Object.isSealed(object)
	frozen object								-> Object.isFrozen(object)

## The JS Namespace

You have seen that JSII uses names for operators (and other things) that are valid variable names in JavaScript. While this rarely matters these days (now that our code is generally composed from modules), it is still possible to access any varaiable in underlying the JavaScript namespace by referencing it as a property of the `js` namespace:

	let js.serialize = js.pack				-> const serialize = pack;

Naturally, this works for the name of the namespace too:

	js.js = js.js.js								-> js = js.js

## Function Grammar

The function grammar has been revised. We decoupled the convenience of the arrow-grammar from the semantics of JavaScript's various function types (so you can declare an arrow-function with a header-block grammar, and a full-fat function with an arrow-operator).

As the term *arrow-function* would be a purely syntactic distinction in JSII, each of the three principle function types is given its own simple name, and corresponding keyword:

* Arrow Functions are called *lambdas*, and use the keyword `lambda`.
* Full Fat Functions are called *functions*, and use the keyword `function`.
* Generator Functions are called *generators*, and use the keyword `generator`.

Lambdas, functions and generators each support a (paren-free) general purpose, header-block syntax, and each has an asynchronous variant that uses `async` as a *qualifier*.

Note: In JSII, a *qualifier* is a word that prefixes some other token to qualify it in some way. Qualifiers look a bit like named prefix operators, but they are lexically attached to the very next token, known as the *subject* (and not some arbitrary expression). Qualifiers are used extensively in JSII, to qualify keywords, named operators, array and object literals, arbitrary expressions (inside parens) and even other qualifiers (recursively), though there is currently only one example of a qualified qualifier (which you will see in a moment).

### Lambda Expressions

The grammar for lambda blocks looks like this:

	lambda <params> <body>

JSII parameters are expressed exactly as they are in JavaScript (including support for destructuring), just without the parenthesis (meaning that they can be omitted from function definitions entirely when there are no parameters).

As always, function bodies must be wrapped in braces. For example:

	let sum = lambda x, y { return x + y }

### Function Expressions and Generator Expressions

The grammar for functions and generators is the same, except for the respective keywords, and is similar to the grammar for lambdas, while also permitting the function to be (directly) named:

	function [<identifier>] [of <params>] <body>
	generator [<identifier>] [of <params>] <body>

The identifier is optional. When present, it immediately follows the keyword.

The parameters are also optional. When present, they are prefixed by the `of` keyword.

As in JavaScript, we can *declare* a function (assigning a name to an otherwise anonymous function):

	let sum = function of x, y { return x + y }

We can also *define* a function, naming it directly:

	function sum of x, y { return x + y }

We generally recommend using the first syntax to declare top-level functions, and the second to define named methods inside class blocks. However, we also recommend using a lambda (over a function) wherever a lambda would suffice, so most top-level functions are actually declared as lambdas.

Note: The compiler never generates *function statements*, instead ensuring that all function definitions are expressions (any extra parens and void operatiors are omitted from the examples in these docs).

### The `do` Qualifier

The do-qualifier (spelt `do`) can qualify the `lambda`, `function` or `generator` keywords to convert the corresponding function to an IIFE (immediately invoking the function, and evaluating to the returned value). For example:

	do function animate of delta=0 {
		requestAnimationFrame(animate)
		renderer.render(delta)
	}

Note: Technically, "dangling dogballs" are legal in JSII, but only implicitly, and they are always redudant. Do not do this:

	function {
		...
	}()

### The `async` Qualifier

The async-qualifier (spelt `async`) can qualify the `lambda`, `function` or `generator` keywords to create an asynchronous version of the respective function-type. For example:

	let read = async function of url {
		let response = await fetch(url)
		return await response.text()
	}

### The `do async` Qualfied Qualifier

As mentioned, JSII has a single qualified qualifier, the do-async qualifier, spelt `do async`. It can qualify `lambda`, `function` or `generator`. This does exactly what you'd expect (immediately invokes the function and evaluates to a promise).

	let promise = do async lambda {
		let response = await fetch("/britney/hitme.mp3")
		return await response.arrayBuffer()
	}

### The Arrow Operators

The language defines two arrow operators (`->` and `=>`) with the following (prefix and infix) syntax:

	-> <expression> 							() => <expression>
	=> <expression>							function() { return <expression> }

	(<params>) -> <expression> 				(<params>) => <expression>
	(<params>) => <expression>				function(<params>) { return <expression> }

Note: The params (when present) must be in parenthesis.

Note: The expression is an expression. It cannot be a formal statement. There is no arrow-block grammar in JSII.

Note: Other potential arrow operators (like `<->` and `<=>`) have been reserved for other function types, pending a usecase. However, given that the body can only contain a single expression, arrow operators for any of the other function types seem useless.

### The `await` operator and For-Await Loops

In JSII, `await` works almost exactly like it does JavaScript:

	await <promise-expression>

To await a promise, then iterate over each item in the result, you can use this syntax:

	for <param> in await <promise-expression>

To iterate over each promise yielded by an asnychronous generator, use this syntax:

	for await <param> in <promise-generator-expression>

## Yielding & Trading

In JSII, `yield` is a keyword, used to begin formal statements (`yield` and `yield from`). As such, `yield` cannot also be an operator. For this reason, JSII generators can *yield* and *trade*, using *yield-statements* and *trade-operations*. For example, take the following four JavaScript statements:

	yield;
	yield item;
	yield * items;
	item = yield item;

In JSII, the previous four statements would look like this:

	yield
	yield item
	yield from items
	item = trade item

### Comments

Line comments start on a hash character (`#`) or a leftward-skinny-arrow (`<-`) or a caret (`^`), and run to end of the line. They can appear after a line continuation marker (without affecting it), but can (obviously) not contain one.

The hash (`#`) is used for comments that appear on their own line, effectively creating headers and subheaders that introduce the code beneath.

The arrow (`<-`) is used to append a comment to the end of a line of code, to comment on that line specifically.

The caret (`^`) is used (often as a bunch of carets) to point to a specific group of character within a line of code, by adding a comment below the line.

Below are examples of all three types of line-comment:

	# this block of code illustrates how line-commentry works

	let numbers = [1, 2, 3]	<- set up some data to play with
	let results = funkyOperation(numbers)
	              ^^^^^^^^^^^^^^ this may throw a `RangeError`

Multiline comments generally just use multiple (single) line-comments, except docstrings, which use a double-quoted string literal. For example:

	let sum = lambda ...args {
	
		"This is how we write docstrings for functions and classes in
		JSII. This style is only used for module, class and funtion
		docsstrings, to set them apart from inline commentry."

		return args.reduce((tally, previous) -> tally + previous, 0)
	}

Note: JSII uses `//` for floating-point floor division:

	x // y			-> Math.floor(x / y)

### Dot Operators

JSII has three dot operators, `.`, `!` and `?` (using `not` for boolean inversion, and `a if b else c` for ternaries).

Dot notation uses the period character (`.`), exactly like JavaScript, for general property access. Private access uses `!` (replacing the JavaScript spelling of `.#`), and the nullish coalescing operator is spelt `?` (replacing `?.`):

	this.foo 		-> this.foo
	this!foo 		-> this.#foo
	this?foo		-> this?.foo

	foo?()			-> foo?.()
	foo?[x]			-> foo?.[x]
	
	this.foo?()		-> this.foo?.()
	this.foo?[x]		-> this.foo?.[x]

Note: The `?!` operator has been reserved (to replace `?.#`), pending a usecase. We would prefer to only use a single-character at any point in the chain (preserving the single-dot-per-step vibe), but we will support the following, if shown to be useful:

	this?!foo		-> this?.#foo

Note: JavaScript's binary infix nullish operator (`??`) is supported (directly):

	a ?? b			-> a ?? b

## Equality

JSII redid JavaScript's equality operations (using a qualified `is not` operator for inversion):

	not a					-> !a
	a is b 					-> Object.is(a, b)
	a is not b				-> !(Object.is(a, b))

In JSII, qualifiers always apply whenever they can. This simple rule allows us to use qualifiers extensively, without running into the kinds of issues CoffeeScript had, where `a is not b` and `a isnt b` meant two different things:

	a is not b				-> !(Object.is(a, b))
	a is (not b)				-> Object.is(a, !b)
	(not a) is b				-> Object.is(!a, b)

Note: The expression on the last line could drop the parens (`not a is b`), as logical-not has far higher precedence than equality operators. The example just illustrates that this can also be expressed symmetrically.

The traditional equality operators are reserved for defining shallow (`==`) and deep (`===`) equality operators later, if we ever have anything we can compile them to (there is no runtime (or preamble), so operator semantics must always be expressed inline, using vanilla JS).

## String Literals

You can wrap string literals in accents, single-quotes or double-quotes.

Literals that use accents or single-quotes can contain interpolations that use the same syntax as JavaScript (a dollar-prefixed pair of (properly nested) curly braces).

Strings that use double-quotes cannot contain interpolations.

We generally use accented literals for all string-literal-expressions, and use double-quoted literals for docstrings.
Single-quoted literals are only supported because removing them seemed overly opinionated. They are redundant.

Double-quoted string literals can span multiple lines, and contain everything between the open double-quote and the closing one.

Accented literals are only single-line-strings if they contain no newlines. They are multiline-strings if the literal contains a newline before any visible character or interpolation (you open the string literal, then immediately start a new line).

Note: Multiline, accented literals with visible characters on the first line raise a syntax error.

Note: Trailing spaces after the opening accent (before the newline) are ignored.

The parser counts the number of spaces at the beginning of the first, new line in a multiline, accented literal (immediately following the line that the literal began on), and ignores that many spaces at the start of that line, and every line that follows it.

Note: The number of spaces can be zero.

Note: If you need to begin a multiline string with whitespace, use an interpolation (at least, at the beginning).

Note: The first character after the spaces on the first, new line must not be a (hard) newline character (it may be one or more *interpolated* whitespace characters). It is a syntax error to begin a multiline string with a whiteline.

If the last line of a multiline, accented literal only contains whitespace, it is ignored.

For example, this string contains one line (`spam and eggs`), with no leading or trailing spaces in the value of the expression:

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

The *subject* of a qualifier (the token being qualified) can be any valid word (a keyword, named operator or another qualifier, but not a variable name or reserved word).

The subject of a qualifier can also be an open paren, bracket or brace. In which case, the qualifier applies to the entire expression (up to the corresponding (properly matched) closing paren, bracket or brace).

Unqualfied compound expressions work exactly as you'd expect from JavaScript, compiling to grouped expressions, array literals and object literals. However, there is one exception here: Objects defined with object literals have null prototypes by default:

	let o = {x: 1}							-> const o = {__proto__: null, x: 1}

If you want a different prototype, you have to set it explicitly:

	let oo = {__proto__: Object, x: 1}		-> const oo = {__proto__: Object, x: 1}

Qualified compund expressions are used to create other kinds of compound types (typed arrays, maps etc):

	let bytes = u8 [1, 2, 3]					-> const bytes = new Uint8Array([1, 2, 3]);
	let data = map {"x": 1, 2: 3}			-> const data = new Map([["x", 1], [2, 3]]);

To provide for this functionality (and the fact that destructuring assignments do not require parenthesis), the JSII Parser treats (parenthesized) *sequence expressions* and (bracketed) *array expressions* as sequences of (zero or more) comma-separated, arbitrary expressions, and (braced) *object expressions* are, likewise, parsed as sequences of comma-separated pairs of colon-separated expressions.

The keys in an object expression are interpreted and compiled, based on the qualifier (or the absence of one). This means that certain types of keys will be valid in, for example, an object expression with the map-qualifier that are not valid in a regular (unqualified) object literal, and others that will be interpreted differently:

	{x: 1}									-> {x: 1}
	map {x: 1}								-> ReferenceError: x is not defined
	map {Function: f}						-> new Map([[Function, f]])
	{Function: f}							-> {Function: f}

## Literate Programming

Literate programming allows you to separate all of your commentry and docstring from your code, generally improving the readability of both.

The JSII Parser has a literate programming flag, which causes files to be parsed using the literate syntax, which is very simple: Lines that begin with whitespace are code lines and every other line is commentary.

Code lines are parsed as normal (leading whitespace is insignificant, even in multiline strings). Everything else is ignored (and thrown away by the parser).

We do not stipulate that comments must be Markdown, or anything like that. You are free to put anything you like on comment lines, and to use them however you wish.

Note: This simple approach limits you to simple markup. For example, nested bullet points or HTML sections would not work, even if they started onside and contained no whitelines, as any indented lines would be interpreted as JSII code.









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


## Statements

### Declarations

+ var
+ let
+ for
+ for-in
+ for-async-in

### Control Flow

++ return
+ break
+ continue
+ throw
++ if-else
++ unless
+ switch
+ try-catch-finally
++ while
++ until
+ pass
+ do

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

+ debugger
+ import
+ export
+ labels
