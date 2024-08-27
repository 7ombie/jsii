Lark: A Modern JavaScript Dialect 
=================================

**IGNORE | THIS IS AN IDEAS-DOC WITH TODO LISTS AND API SKETCHES ETC | IT IS OUTDATED AND WRONG**

Lark has two goals:

+ Design a nice, modern dialect of JavaScript that still looks, feels and acts like JavaScript.
+ Properly encapsulate the various components of the (self-hosted) implementation behind small,
  simple and declarative APIs, so anyone can easily design and maintain their own Lark dialect,
  adding or removing features as required, without needing to dive into the guts of the code.

The ability to define your own dialect (potentially, for each project) exploits the fact that
Lark is not really a *language*. It's just a better grammar for the JavaScript language. This
allows Lark code to seamlessly interoperate with code written in Vanilla JS, as well as every
other dialect (or version) of Lark, TypeScript, CoffeeScript *et cetera*. It's all JavaScript.

Note: Lark doesn't need an ecosystem. Browsers already support source maps, so an LSP, a short
tutorial, and a wiki should be enough to quickly onboard anyone from the JavaScript community.

Note: Lark doesn't need a community. Lark is used in-house to develop games for the Web, with
PHANTASM (an in-house language that compiles to WASM) and WGSL (the WebGPU Shader Language).
You're totally welcome to get involved, but I'm not up for the 'community' thing.

The Lark Grammar
================

Lark has a simple, statement grammar with curly braces. Newlines are significant (generally
terminating the current statement). Indentation is insignificant.

Lark replaces Automatic Semicolon Insertion (ASI) with Linewise Implicit Statement Termination
(LIST). The logic is similar to languages like Python and Swift: Newlines terminate the current
statement, unless the newline is nested within parens, brackets or braces.

Note: LIST only considers parens, brackets or braces that are part of the current statement. The
braces around control-flow blocks and function bodies are obviously not relevant here.

Note: Lark does not provide a continuation symbol for escaping newlines (like the `\` symbol in
Python). In practice, there are just always better options.

Note: Using 100-column source files is recommended.

Lark does not use semicolons. However, it is still possible to have more than one statement on
the same line. You just use commas between the statements instead. This works especially well
with inline blocks:

    if valid(candidate) { count += 1, yield candidate }

Keywords
--------

Every proper statement (every statement aside from expression statements) starts with a keyword.
Block-statements use `do` (like `do { ... }`), and empty statements use `pass`. This avoids any
ambiguity with expression statements. For example, destructuring assignments never require
extra parenthesis in Lark:

    {x, y, z} = object

Expression statements do not *generally* start with a keyword, but they're arbitrary expressions,
so there are exceptions. For example, `yield` is a keyword and a valid expression. This is not
normally important, but is worth noting before considering the way Lark sugars its blocks and
predicates...

Paren-Free Predicates
---------------------

Lark implements something similar to Eich's *paren-free mode*. However, while Lark drops the parens
from around predicates, it does not require braces around every block.

Parens around predicates are always optional, so this kind of thing is always acceptable:

    if x > y { return x }

    while x > 0 { console.log(x -= 1) }

Blocks & Bodies
---------------

Code is structured into *function bodies* and *control flow blocks* (*bodies* and *blocks*). Bodies
always require braces, but braces around blocks become optional when the block contains a single
*formal* statement. In Lark, statements are considered *formal* when they begin with a keyword,
and *informal* otherwise.

Note: When establishing whether a given statement is formal or informal, the parser only considers
whether the statement begins with a keyword or not. There are keywords that open formal statements
(such as `yield`, `await`, `function` *et cetera*) that are also valid expressions.

Note: The distinction between formal and informal statements only matters when establishing whether
or not braces are required around a control-flow block. The concept is entirely lexical.

Revisiting the previous example, we can see that the braces are not required in the first case:

    if x > y return x

The nested statement forms a control-flow block (not a function body), and is a single formal
statement (`return x`). Likewise, this is also allowed:

    if x > y yield x

The same does not apply to the second example from before. This would raise a syntax error:

    if x > y console.log(x)

The braces are still required here, as the block does not begin with a keyword:

    if x > y { console.log(x) }

Compound Statements
-------------------

Compound statements (those that contain blocks of other statements, like `if`, `while`, `for` etc)
are all proper statements (which are always formal). Therefore, when we have a block that only
contains one other block, we can inline the headers. For example:

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

You've already seen that Lark treats parens around predicates as optional, and you're familiar with
rules for bodies and blocks. You've also seen examples of if-statements, while-statements etc, so
you know how that works.

For-in loops in Lark have the same semantics as for-of loops in JS, with a slightly nicer grammar:

    for <assignees> in <expression> <block>

Note that `for` is a declarator that declares one or more constants *for* each iteration. This
compliments the two principle declarators, `let` and `var`.

The grammar of `<assignees>` is exactly the same (unpacking) grammar that is used by declarations
and assignments (`let <assignees> = <expression>`).

Note: There is currently no support for declaring a loop variable as anything other than a
block-scoped, per-iteration constant (pending usecases for the wackier stuff that JavaScript
permits).

Lark also supports unless-branches and until-loops (the inverse of if-blocks and while-loops,
respectively), though unless-blocks do not (and will never) support else-clauses:

    until game.over { animate() }

		unless result.error return

Note: Only use `unless` or `until` if you would otherwise need to negate the entire predicate.

Note: The logical operators use words (`not`, `and` and `or`), while the bitwise operators have
been redesigned. They now use different spellings for the operators (for example, the prefix
operators `!` and `?` are used for bitwise-not and count-leading-zeros). Bitwise operations
also infer unsigned semantics (twiddling bits in twos-compliment is just confusing).

The `of` Operator
-----------------

Lark has an `of` operator with the following grammar:

    <identifier> of <expression>

This applies a simple (always inlined) *functional operator* (defined by Lark, and specified by
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

The `of` operator supports a range of builtin operations. The list is not finalized, but will
include stuff like this (as well as the stuff above):

    type of x                      -> typeof x
		prototype of x                 -> Object.getPrototypeOf(x)
		names of x                     -> Object.getOwnPropertyNames(x)
		symbols of x                   -> Object.getOwnPropertySymbols(x)

JSON Operators
--------------

Lark provides two prefix operators for doing JSON, one named `serialize` and another named
`deserialize`.

The `serialize` operator compiles to `JSON.stringify`, while `deserialize` compiles to
`JSON.parse`. For example:

    deserialize serialize o        -> JSON.parse(JSON.stringify(o))
    serialize x in serialize y     -> JSON.stringify(y).includes(JSON.stringify(x))

Mutablity Operators
-------------------

Lark provides prefix operators for *packing*, *sealing* and *freezing* objects, with a
corresponding set for checking whether an object is *packed*, *sealed* or *frozen*.

While the terms *seal* and *freeze* should be familiar to any JS dev, the term *pack* was
introduced by Lark to refer to `Object.preventExtensions` and `Object.isExtensible`, fixing
an inconsistency in JavaScript, where `isExtensible` returns `true` if we *can* mutate the
object, while `isSealed` and `isFrozen` return `true` if we *cannot* mutate the object.

In Lark, pack, seal and freeze (consistently) define three levels of progressively more
immutable state:

    pack o                         -> Object.preventExtensions(o)
    seal o                         -> Object.seal(o)
    freeze o                       -> Object.freeze(o)

    o is packed                    -> !Object.isExtensible(o)
    o is sealed                    -> Object.isSealed(o)
    o is frozen                    -> Object.isFrozen(o)

    o is not packed                -> Object.isExtensible(o)
    o is not sealed                -> !Object.isSealed(o)
    o is not frozen                -> !Object.isFrozen(o)

The JS Namespace
----------------

You have seen that Lark uses names for operators (and other things) that are valid variable
names in JavaScript. If this is ever an issue, you can access any varaiable in the underlying
JavaScript namespace by referencing it as a property of the `js` namespace:

    let js.serialize = js.pack     -> const serialize = pack;

Naturally, this works for the name of the namespace too:

    js.js = js.js.js               -> js = js.js

Functions
---------

The function grammar has been revised to decouple the convenience of the arrow-grammar from the
semantics of JavaScript's various function types (so you can declare an arrow-function with a
header-block grammar, and a full-fat function with an arrow-operator etc).

As the term *arrow-function* would be a purely syntactic distinction in Lark, each of the three
principle function types is given its own simple name and corresponding keyword:

* Arrow Functions are called *lambdas*, and use the keyword `lambda`.
* Full Fat Functions are called *functions*, and use the keyword `function`.
* Generator Functions are called *generators*, and use the keyword `generator`.

Lambdas, functions and generators each support a (paren-free) general purpose, header-block
syntax, and each has an asynchronous variant that uses `async` as a *qualifier*.

Note: In Lark, a *qualifier* is a word that prefixes some other token to qualify it in some way.
Qualifiers look a bit like named prefix operators, but they are lexically attached to the very
next token, known as the *subject* (and not some arbitrary expression). Qualifiers are used
extensively in Lark, to qualify keywords, named operators, array and object literals,
arbitrary expressions (inside parens) and even other qualifiers (recursively), though
there is currently only one example of a qualified qualifier (which you will see
in a moment).

### Lambda Expressions

The grammar for lambda-statements (which are also valid expressions) looks like this:

    lambda <params> <body>

Lark parameters are expressed exactly as they are in JavaScript (including support for
destructuring), just without the parenthesis (meaning that they can be omitted from function
definitions entirely when there are no parameters).

As always, bodies must be wrapped in braces. For example:

    let sum = lambda x, y { return x + y }

Note: In Lark, we generally use the arrow-operators (covered below) for lambdas/functions/etc
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

Note: The "dangling dogballs operator" is legal in Lark, but only implicitly, and it's always
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

As mentioned, Lark has a *qualified qualifier*. The do-async-qualifier, spelt `do async`, can
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

The `await` Operator and Asynchronous For-Loops
-----------------------------------------------

In Lark, `await` works exactly like it does JavaScript:

    await <asynchronous-expression>

When we want to iterate over the values yielded by an async-generator, Lark has a for-from loop
(instead of JavaScript's for-await-of loop):

    for <assignees> from <asynchronous-generator-expression>

Note: Lark does not use the asterisk for generators. It has a `generator` keyword, and copies the
`yield from <expression>` grammar (described below) from Python.

Yielding from Generators
------------------------

In Lark, `yield *` is spelled `yield from`:

    yield                           -> yield;
    yield item                      -> yield item;
    yield from stream               -> yield * stream;
    item = yield item               -> item = yield item;

Commentary
----------

Line comments start on a hash character (`#`), then run to end of the line.

Multiline comments generally just use multiple (single) line-comments, except docstrings, which
use a double-quoted string literal. For example:

    let sum = lambda ...args {

        "This is how we write docstrings for functions and classes in
        Lark. This style is only used for module, class and funtion
        docsstrings, to set them apart from inline commentry."

        return args.reduce((tally, previous) -> tally + previous, 0)
    }

Note: Lark uses `//` for floor-division:

    x // y			-> Math.floor(x / y)

Dot Operators
-------------

Lark has three dot operators, `.`, `!` and `?` (using `not` for boolean inversion, and
`x when predicate else y` for ternaries).

Dot notation uses the period character (`.`), exactly like JavaScript, for general property
access. Private access uses `!` (replacing the JavaScript spelling of `.#`), and the nullish
coalescing operator is spelt `?` (replacing `?.`):

    this.foo        -> this.foo
    this!foo        -> this.#foo
    this?foo        -> this?.foo

    foo?()	        -> foo?.()
    foo?[x]	        -> foo?.[x]

    this.foo?()		  -> this.foo?.()
    this.foo?[x]		-> this.foo?.[x]

Note: The `?!` operator has been reserved (to replace `?.#`), pending a usecase. I'd prefer
to only use a single-character at any point in the chain (preserving the single-dot-per-step
vibe), but will support the following, if shown to be useful:

    this?!foo       -> this?.#foo

Note: JavaScript's binary infix nullish operator (`??`) is supported (directly):

    a ?? b          -> a ?? b

Equality Operators
------------------

Lark redid JavaScript's equality operations to use `==` and `!=`, but using the `Object.is`
function:

    a == b          -> Object.is(a, b)
    a != b          -> !Object.is(a, b)

Note: The JavaScript `==` and `===` operators, along with `!=` and `!==`, are not available in
Lark.

Lark uses the `is` operator instead of JavaScript's `instanceof`, and includes an `is not`
operator for checking the opposite:

    message is String        -> message instanceof String
		message is not String    -> !(message instanceof String)

String & Text Literals
----------------------

Lark string literals are inspired by Swift. They exclusively use quotes as delimiters (you cannot
use apostrophes or grave accents):

    let name = "Lark"

Lark also supports triple-quoted literals (that begin and end with `"""`):

    let name = """Lark"""

Any string literal can span multiple lines, but triple-quoted literals have extra features that
simplify expressing multiple lines of text. For this reason, Lark uses the term *string literal*
to describe single-quoted strings, and *text literal* for triple-quoted strings.

Text literals exclude the lines that include the quotes, so the expressed string begins on the
first line after the opening quotes, and ends on the last line before the closing quotes. The
indentation level of the closing quotes indicates how much indentation to remove from each of
the lines within the literal.

Note: It is a syntax error for a text literal to have any characters (ignoring insignificant
trailing whitespace) after the opening quotes, or to have any characters, except indentation,
before the closing quotes.

For example, here `string` contains a single, unindented line that reads `spam and eggs`:

    let string = """
        spam and eggs
        """

Indenting the literal (as in the previous example) is entirely optional. For example:

    let string = """
    this line is onside
      this line is indented by two spaces
    this line is onside again
    """

The ability to indicate where the text begins is especially useful when the literal is nested
inside indented code. Otherwise, you get this kind of thing:

    do {
        let string = "this line is onside
      this line is indented by two spaces
    this line is onside again"
    }

### Escaping

Simple, single-character escape sequences work exactly like JavaScript. This string literal
contains two lines:

    "Line1\nLine2"

None of the other kinds of escape sequence are supported. Interpolations are used instead.

### Interpolation

Lark uses the same escape character to introduce interpolations (`\`). The interpolation is
expressed as a tuple of zero or more expressions:

    "1 + 2 == 3"
		"1 + 2 == \(1 + 2)"

The values of the expressions in the tuple are treated as adjacent interpolations (without
spaces), so the following string literals are equivalent (and the same would be true for
the corresponding text literals):

		"\(x, y, z)"

    "\(x)\(y)\(z)"

### Tagged Literals

In Lark, you can follow an expression with a string or text literal, and it will create a
tagged literal, just like JavaScript, except that Lark permits whitespace between the
expression and the literal that follows it:

    foo "bar" -> foo`bar`

Compound Expressions
--------------------

Array and object literals are similar to JavaScript, with a few exceptions.

Objects defined with object literals have `null` prototypes by default:

    let o = {x: 1}                        -> const o = {__proto__: null, x: 1}

If you want a different prototype, you have to set it explicitly:

    let oo = {__proto__: Object, x: 1}    -> const oo = {__proto__: Object, x: 1}

Furthermore, unlike JavaScript, you cannot use a name to express a key and value with that
name. This will not do what it would do in JavaScript:

    let o = {foo, bar}

This limitation is a compromise that Lark makes to permit the introduction of map-literals
and set-literals. Map literals use colon-separated expressions in square brackets:

    let map = [1: "x", 2: "y"]             -> const map = new Map([[1, "x"], [2, "y"]]);

You can do a similar thing with curly braces to express sets:

    let set = {1, 2, 3, 4}                 -> const set = new Set([1, 2, 3, 4]);

To disambiguate between the overloaded characters, empty objects and maps must use a colon:

    []     -> empty array
		{}     -> empty set
		[:]    -> empty map
		{:}    -> empty object

Literate Programming
--------------------

Literate programming allows you to separate all of your commentry and docstrings from your
code, generally improving the readability of both.

The Lark Parser has a literate programming flag, which causes files to be parsed using the
literate syntax, which is very simple: Lines that begin with whitespace are code lines and
every other line is commentary.

Code lines are parsed as normal (leading whitespace is insignificant, even inside multiline
strings). Everything else is ignored (and thrown away by the parser).

Lark does not stipulate that comments must be Markdown, or anything like that. You are free
to put anything you like on comment lines, and to use them however you wish.

Note: This simple approach limits you to simple markup. For example, nested bullet points or
HTML sections would not work, even if they started onside and contained no whitelines, as
any indented lines would be interpreted as Lark code.

### Operators

++ Addition (+)
++ Addition assignment (+=)
++ Assignment (=)
++ await
++ Bitwise AND (&)
++ Bitwise AND assignment (&=)
++ Bitwise NOT (~)
++ Bitwise OR (|)
++ Bitwise OR assignment (|=)
++ Bitwise XOR (^)
++ Bitwise XOR assignment (^=)
++ Comma operator (,)
++ Ternary (when-else) operator
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
+ of operator
+ Increment (++)
--- Inequality (!=)
+ is       `x is Type     -> x instanceof Type`
+ is not   `x is not Type -> !(x instanceof Type)`
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

++ var
++ let
+ for
++ for-in
++ for-from

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

++ lambda
++ function
++ generator
++ async lambda
++ async function
++ async generator
++ do lambda
++ do function
++ do generator
++ do async lambda
++ do async function
++ do async generator
+ class

## Misc

++ debug
+ import
+ export
+ labels

Promotions
==========

+ groupBy         -> Object.groupBy
+ isFinite        -> Number.isFinite
+ isInteger       -> Number.isInteger
+ isNaN           -> Number.isNaN
+ isSafeInteger   -> Number.isSafeInteger

