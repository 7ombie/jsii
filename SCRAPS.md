Lark: A Modern JavaScript Dialect
=================================

**IGNORE | THIS IS AN IDEAS-DOC WITH TODO LISTS AND API SKETCHES ETC | IT IS OUTDATED AND WRONG**

Lark combines a very high-level, dynamically-typed dialect (optimized for productivity) with a very
low-level, statically-typed dialect (optimized for performance and tight control over memory), in a
single language.

The high-level dialect is substantially higher-level than JavaScript (while retaining all of its
important performance characteristics). The grammar and semantics are both less complicated, more
consistent and more convenient.

For example, in Lark, everything's an object, except `null` and `void` (which are the Lark spellings
for `null` and `undefined`). The distinction between primitive types (like `number` and `string`)
and OOP types (like `Number` and `String`) is an implementation detail. We think of primitive
types like specialized functions: It's all done under the hood to improve performance.

Lark replaces the `typeof` and `instanceof` operators with a single `is` operator that just works.
For example, the expression `x is Array` equates to `Array.isArray(x)`, while `x is String` equates
to `typeof x === "string" || x instanceof String`. You can even do stuff like `x is NaN`, which
compiles to `Number.isNaN(x)`.

Lark does have a `type` operator that returns the type of its only operand (like JavaScript's `typeof`),
`type` is a suffix operator that compliments `is` by returning the constructor of its only operand. For
example:

    let numbers = [1, 2, 3, 4, 5]
    put numbers is Array                        # true
    put numbers type == Array                   # true

There's also an `is not` operator that inverts the semantics of `is`, but doesn't introduce distinct
semantics. For example, `x is not Array`

The `put` operator just wraps `console.log` so it's easy to type.

While prefix operators are normally restricted to only ever operating on a single operand, Lark has a
form of Polish notation that allows you to supply any number of operands (as long as the number is
greater than one). For example:

    (put x, y, x)

When a parenthesized expression contains a single operand, it just groups the expression (like every
other formal language). However, when there's more than one operand, the compiler requires that the
first operand is a prefix operation (at the top level), while the other one or more operands can
be arbitrary expressions. All of the operands are passed to the operator, so the previous code
would compile to this:

    console.log(x, y, z);

Note: Prefix operators are generally defined to operate on a single operand. However, Lark has a small
set of prefix operators that are still able to operate on a single operand, but can also operate on more
than one. For example, the `min` and `max` operators each operate on a single collection, except when
they're used with Polish notation, where their operands form the collection they operate on:

    put min numbers                             # 1
    put max numbers                             # 5

Naturally, the operand can be any arbitrary expression:

    put min [2, 9, 3, 6, 8]                     # 2
    put max [2, 9, 3, 6, 8]                     # 9

Spreading the only operand also counts as more than one operand (no matter how many values the spread
provides, if any):

    put (min numbers...)                        # 1
    put (max numbers...)                        # 5

You cannot use the spread operator outside of a compound expression (so `put min numbers...` wouldn't
even parse).

Lark has two goals:

+ Design a nice, modern dialect of JavaScript that still looks, feels and acts like JavaScript.
+ Properly encapsulate the various components of the (self-hosted) implementation behind small,
  simple, declarative APIs, so anyone can easily create their own Lark dialect, without needing
  to dive into the guts of the code.

The ability to define your own dialect (potentially, for each project) exploits the fact that
Lark is not really a *language*. It's just a better grammar for JavaScript. Lark improves the
semantics here and there (improving some of the operators, for example), but retains seamless
interoperability JavaScript, as well as every other dialect (and version) of Lark, TypeScript,
CoffeeScript *et cetera*. It's all just JavaScript.

The Lark Grammar
================

Lark has a simple, statement grammar with curly braces. Newlines are significant. Indentation is
insignificant.

Lark replaces Automatic Semicolon Insertion (ASI) with Linewise Implicit Statement Termination
(LIST). The logic is similar to languages like Python and Swift: Newlines terminate the current
statement, unless the newline is nested within parens, brackets or braces.

Note: LIST only considers parens, brackets or braces that are part of the current statement. The
braces around control-flow blocks and function bodies do not effect the significance of newlines.

Note: Lark does not provide a continuation symbol for escaping newlines (like the `\` symbol in
Python). In practice, there are just always better options.

Note: Using 100-column source files is strongly recommended.

Lark does not use semicolons. However, it is still possible to have more than one statement on
the same line. You just use commas between the statements instead. This works especially well
with inline blocks:

    if valid(candidate) { count += 1, yield candidate }

Hoisting
--------

Lark does not hoist anything.

Declarations (using `let` and `var`) compile to `const` and `let` (respectively), which are not
hoisted (by JavaScript).

Function literals (including class literals) are *always* expressions, even when the statement
begins with the corresponding (operative) keyword. As a result, they are never hoisted either.

As a consequence, naming a function (or class) simply sets the `name` property (whether the name
is hardcoded or computed at runtime). It will never declare a variable of the same name. You can
only declare names using `let` or `var`.

I'm not sure what to do about temporal dead zones. We could detect them and swap out the name of
the hoisted declaration for a register (updating any references), so any references that appear
before a declaration, will (attempt to) reference the outer scope. I need to look into the
ratioanal for JavaScript's current design. I may be missing something.

Keywords
--------

Every proper statement (every statement aside from expression statements) starts with a keyword.
Block-statements use `do` (like `do { ... }`), and there's a `pass` statement that can be used
to create explicitly empty statements. This eliminates any ambiguity between statements and
expressions. The following statements are both valid expressions:

    {x, y, z} = object
    {}

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

Bodies & Blocks
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

Compound statements (those that contain blocks of other statements, like `if`, `while`, `for`
etc) are all proper statements (which are always formal). Therefore, when we have a compound
statement that only contains one other compound statement, we can inline the headers. For
example:

    for row in rows for column in columns { console.log(row, column) }

    for candidate in candidates if eligible(candidate) yield candidate

Note: This can recur to any degree (space permitting).

Declarations
------------

Declarations use  `let` and `var`, which compile to `const` and `let` respectively. There is no
way to access JavaScript's `var` semantics. Everything is block-scoped, and is either a constant,
declared with `let`, or a variable, declared with `var`.

    let pi = 3.141
    var xp = 0

The grammar for destructuring is copied directly from JS (`<assignees>` is pure JavaScript):

    let|var <assignees> = <expression>

For example:

    let {sin, cos} = Math
    var [x, y, z] = [0, 0, 0]

Assignment expressions use the exact same `<assignees>` grammar:

    <assignees> = <expression>

Note: As mentioned already, you do not need to wrap Lark assignments in parenthesis, even when
the expression would otherwise cause a statement to begin with an opening brace.

The `<assignees>` grammar is also used by for-loops, params in function definitions, and import
parameters in (static) import declarations (all described later in the document).


Implicitly Initialized Variables
--------------------------------

In JavaScript, variables can be declared without explicit initializers, and are implicitly void,
so the follow JavaScript statements are equivalent:

    let x = undefined;
    let x;

Lark declarations require initializers, so either of those statements would be written like this:

    var x = void

JavaScript users often forward-declare variables, so they can use more complex code to initialize
the variable. Below is a simple example:

    let result;
    for (const item of items) if (item.meets(requirements)) {
        result = item;
        break;
    }

That works, but it prevents `result` from being declared as a constant. To achieve that, something
like this is required:

    const result = function() {
        for (const item of item) if (item.meets(requirements)) return item;
    }();

Lark has a `do` qualifier that allows that code to be written like this:

    let result = do for item in items if item.meets(requirements) return item

Note: The `do` qualifier is described later, but it basically wraps the statement that follows
it in a function literal (if it's not a function literal already), then immediately invokes it.
With `do` statements, we can use arbitrarily complex initializers, so Lark declarations invariably
require explicit initialization.

Control Flow
------------

You have already seen `if` and `while`, so `for` loops are the obvious feature to look at next. As
mentioned, Lark has three (`for-in`, `for-of` and `for-from`).

Lark treats `for` as a declarator that declares a set of one or more constant assignees *for* each
iteration of the loop. This compliments the two principle declarators, `let` and `var`.

Below is an example for each of the three kinds of loop, (directly) followed by the JavaScript it
compiles to (however, the output will only make partial sense until you've read the next section):

    let scoreboard = {"Ali": 5000, "Bob": 2500, "Caz": 8000, "Dom": 6500}

    for player of scoreboard { console.log(scoreboard[player]) }

    -> for (const player of scoreboard.ƥkeys?.() ?? Object.keys(scoreboard)) {
           console.log(scoreboard[player]);
       }

    for [name, score] in enumerate scoreboard { console.log(name, score) }

    -> for (const [name, score] of window.ƥ.enumerate(scoreboard.ƥvalues?.() ?? Object.values(scoreboard))) {
           console.log(name, score);
       }

    for result from taskQueue.results() { console.log(result) }

    -> for await (const result of taskQueue.results()) { console.log(result) }

Note: In Lark, loop variables are always block-scoped, per-iteration constants.

Lark Methods, Lark Functions & Lark Registers
---------------------------------------------

The `scoreboard.ƥkeys` and `scoreboard.ƥvalues` methods in the JavaScript output for the previous
code examples both begin with the *Lark Character* (`ƥ`), which is really just a lowercase Latin-P
with a hook, but it looks a bit like a lark (with the right font).

Methods with names that begin with a Lark Character are called *Lark methods*, and the character
is pronounced *lark*, as in *scoreboard dot lark-keys*.

Lark methods are used to safely modify native types (with static methods) and native prototypes
(with instance methods). This allows the compiled JavaScript to easily select a specific operator
implementation, based on the types of the operands, without knowing the types (at comptime) or
relying on a typecheck (at runtime).

### Abstraction & Performance

In practice, the compiled JavaScript will often need to check that an expression evaluates to an
object (not `null` or `void`) and that the object has the required Lark method (typically using
some combination of the `?.` and `??` operators). In any case, none of the Lark operators are
substantially more expensive than the corresponding idomatic JavaScript.

For example, Lark methods are used to implement the `in` and `of` infix-operators, with matching
`for-in` and `for-of` loops. These operators work correctly with all of the builtin collection
types, without being much more expensive to use. Take this concrete example:

    for number in numbers { console.log(numbers) }

That would compile to something like the following JavaScript (which is pretty *noisey*, but not
very *expensive*):

    for (const number of numbers.ƥkeys?.() ?? Object.keys(numbers)) { console.log(number) }

Note: We assume the expression that expresses the iterable is not `null` or `void`, as that would
fail in any case.

Note: The extra code in `for-in` and `for-of` loops allows you to iterate over the keys or values
of any instance of any builtin collection (including `String`, `Array`, `Object`, `Map` and `Set`)
or any subclass, or the keys or values of any object with a `null` prototype (or with a prototype
chain that ends on `null`).

Note: The first condition (that `numbers` has a `ƥkeys` instance property) only fails if `numbers`
is an object with a `null` prototype (assuming that `numbers` is a container in the first place),
so we only fallback to the second case for objects with `null` prototypes (so they still work).
Furthermore, the extra checks only happen once (before the loop begins iterating).

Ultimately, the above approach allows Lark to be substantially higher-level than JavaScript, while
still providing similar performance in the worst case.

### Lark Functions

As seen above, Lark uses *lark-functions*, which are bound to an object named `window.ƥ`, and have
names like `enumerate`. These functions are used to implement operators and stuff like that.

Note: We put all our internal, top-level stuff into `window.ƥ` to avoid polluting the namespace.

### Lark Registers

As you will see soon, Lark also uses *lark-registers*, which are unqualified constants that begin
with a Lark Character, followed by one or more digits (`ƥ0`, `ƥ1` `ƥ2` *et cetera*). Each register
is used once and discarded. Whenever an expression in the Lark code is needed in multiple places
in the JavaScript output, it is assigned to a register (assuming the expression is not already
a name).

The Collection Operators: `in`, `of` & `on`
-------------------------------------------

IMPORTANT: Note how `for-of` uses `ƥkeys` and `for-in` uses `ƥvalues` in the previous examples.

Lark uses `in` and `of` for (more or less) the same stuff JavaScript uses them for, except that in
Lark, *the semantics are reversed*.

In JavaScript, `in` refers to the keys or indices *in* an object or array, while `of` refers to the
values *of* that container. Lark does the exact opposite.

Note: While JavaScript is technically correct, especially in a language where everything is passed
by reference (the keys are *in* the container, while the values are just floating around in memory),
that technicality is normally left implicit.

In practice, we generally speak about the *values in a collection*, and the *properties of an object*
or the *indices of an array* or the *keys of a hash* or the *keys of a map*.

In any case, Lark consistently uses `in` to refer to the values *in* a collection*, and uses `of` to
refer to the properties, indices or keys *of* a collection.

Lark further overloads `in` and `of` as Boolean infix operators, where `in` checks if the left operand
is in the right operand (as in `value in array` or `value in object`), and `of` checks if the left
operand is one of the right operand's *own* properties (as in `key of object` or `index of array`):

    x in y      -> const ƥ0 = x, ƥ1 = y; ƥ1.ƥin?.(ƥ0) ?? Object.values(ƥ1).includes(ƥ0)
    x of y      -> const ƥ0 = x, ƥ1 = y; ƥ1.ƥof?.(ƥ0) ?? Object.hasOwn(ƥ1, ƥ0)

Note: We invoke `ƥin` or `ƥof` when they exist on the instance (which is the case for anything that
inherits from a builtin collection type), else we fall back to `Object.values` and `includes` for
the `in` operator, and `Object.hasOwn` for the `of` operator. This ensures that these operators
still work for objects with `null` prototypes.

If you need the original semantics of JavaScript's `in` operator, it is available in Lark, but is
spelled `on` instead:

    x on y      -> x in y

The `in`, `of` and `on` operators can all be qualified by `not` to produce the `not in`, `not of`
and `not on` operators, which obviously invert the semantics.

### Unless & Until

Lark also supports unless-branches and until-loops, though unless-blocks do not support else-clauses
(or anything like that):

    unless <expression> <block>        -> if (!(<expression>)) <block>

    until <expression> <block>         -> while (!(<expression>)) <block>

Note: Only use `unless` or `until` with compounded predicates, and only if you would need to negate
the entire predicate otherwise:

    unless result.error || amount < 0 return

    until window.closed || game.over { animate() }

Note: The logical operators use words (`not`, `and` and `or`), while the bitwise operators have
been redesigned. They now use different spellings for the operators (for example, the prefix
operators `!` and `?` are used for bitwise-not and count-leading-zeros). Bitwise operations
also infer unsigned (32-bit integer) semantics (they're signed in JavaScript).

JSON Serialization: The `json` Operator
---------------------------------------

Lark provides a `json` prefix operator for serializing and deserializing JSON:

    json <expression>

When the operand is a string, the operator deserializes it to the encoded value (typically an
object). In all other cases, the operator serializes to operand to a JSON text string.

Note: The above description is overly optimistic. The `json` operator relies on `JSON.stringify`
and `JSON.parse`, which can both choke on invalid operands.

The `json` operator doesn't really require example code, but it's always nice to have some anyway:

    let o = {foo: 1}                -> const o = {__proto__: null, foo: 1};
    json json o                     -> JSON.parse(JSON.stringify(o))
    json x in json y                -> JSON.stringify(y).includes(JSON.stringify(x))

Note: The JavaScript after a skinny-arrow (throughout this doc) is only provided to express the
high-level *semantics* of a given block of Lark code (using language JavaScript programmers are
already highly familiar with). The compiled output will often differ in various (but generally
uninteresting) ways.

Note: The previous note is not a couched warning that our output may be inefficient. The compiler
does consider the context of a given operation to try to produce the most optimal result (often
eliminating any extra cost, while retaining all of the extra functionality).

Mutablity Operators
-------------------

Lark provides prefix operators for *packing*, *sealing* and *freezing* objects, with a
corresponding set for checking whether an object is *packed*, *sealed* or *frozen*.

While the terms *seal* and *freeze* should be familiar to any JS dev, the term *pack* was
introduced by Lark to refer to `Object.preventExtensions` and `Object.isExtensible`. This
addresses an inconsistency in JavaScript, where `isExtensible` returns `true` if we *can*
mutate the object, while `isSealed` and `isFrozen` return `true` if we *cannot* mutate
the object.

In Lark, `pack`, `seal` and `freeze` define three levels of progressively more immutable
state:

    pack o                         -> Object.preventExtensions(o)
    seal o                         -> Object.seal(o)
    freeze o                       -> Object.freeze(o)

Lark also provides a corresponding set of three prefix operators for checking whether an
object is packed, sealed or frozen, as well as the inverse set:

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

### Function & Generator Literals

The general (header-block) grammar for functions and generators is the same, except for the
respective keywords:

    function <params> <body>

    generator <params> <body>

There are no parens around parameters (the `<params>` construct can just be an empty string).
Otherwise, parameters use the same destructuring grammar as JavaScript.

The `<body>` must be wrapped in curly braces, and does not support any form of implicit-return
(see the skinny-arrow-operator below).

Note: Function literals are always expressions (even when a statement begins with `function`).

### Declaring Functions & Generators

As in JavaScript, we can declare functions and generators, using `let` (or `var`, technically).
Unlike JavaScript, Lark will also automatically copy the name to the function (or generator):

    let sum = function x, y { return x + y }

    -> const sum = function sum(x, y) { return x + y };

Function parameters are optionally prefixed by a declarator (either `let` or `var`), with `let`
being inferred when the declarator is omitted. For example, the `x` and `y` parameters in the
previous example are implicitly constant. The following line has the same meaning, but uses
explicit declarators:

    let sum = function let x, let y { return x + y }

To make the parameters variable, you would do something like this (although it would be entirely
pointless in this case):

    let sum = function var x, var y { return x + y }

Conversely, the following example would raise a (runtime) exception every time it's invoked:

    function x { x += 1 }           -> function(ƥ0) { const x = ƥ0; x += 1 }

The parameter must be explicitly variable:

    function var x { x += 1 }

As declaring functions is so commonplace, Lark has sugar that replaces both the `=` operator and
the `function` keyword with the much shorter `of` keyword (which is also optional if there are no
declared parameters):

    let|var <name> [of <args>] <body>

This allows you to shorten the previous examples to this:

    let sum of x, y { return x + y }

Or even just this (see the section on *contextuals* below):

    let sum { return @0 + @1 }

### Contextuals

Lark supports names and indices that begin with an `@` character, known as *contextuals*. These
values are qualified, based on the *functional context* (`this` and `arguments`):

    console.log(@)                     -> console.log(this);
    console.log(@foo)                  -> console.log(this.foo);
    console.log(@3)                    -> console.log(arguments[3]);

A function can also define a contextual parameter name, which causes the argument to be assigned
to `this` automatically. For example:

    let SpaceInvader = class {
        constructor @x, @y { pass }
    }

That Lark code would compile to this JavaScript:

    const SpaceInvader = class SpaceInvader {
        constructor(ƥ0, ƥ1) {
            this.x = ƥ0;
            this.y = ƥ1;
        }
    }

Contextuals actually begin with one or more `@` characters, though more than one is uncommon, and more
than two is rare.

The number of `@` characters is used to infer *which* context to refer to. One `@` character denotes the
the current scope, two (`@@`) denotes the containing scope, three (`@@@`) denotes the containing scope's
containing scope, and so on. The limit is the outermost function.

The compiler implements this feature by climbing the scopes, and copying `this` and `arguments` into
registers as required. For example:

    let outer = function {
        let mider = function {
            let inner = function {
                console.log(@, @@foo, @@@foo, @@@1)
            }
        }
    }

That code compiles to this JavaScript:

    const outer = function outer() {
        const ƥ0 = this, ƥ1 = arguments;
        const mider = function mider() {
            const ƥ2 = this;
            const inner = function inner() {
                console.log(this, ƥ2.foo, ƥ0.foo, ƥ1[1]);
            };
        };
    };

Note: In practice, it would be unusual to reference an implicit argument to a function that's that far
away (`@@@1`), but you can if you like.

Note: Contextuals are the main reason Lark doesn't expose JavaScript's arrow-functions. Lark originally
had lambdas that compiled to arrow-functions. However, having one type of function that got its own
context and another that didn't was just confusing in practice (especially as Lark programs tend
to use `arguments` extensively). Contextual values are far less confusing, while still being
more flexible than arrow-functions.

Note: In Lark, `this` and `arguments` are reserved words.

### Self & Self

In Lark, `self` is used to refer to the function (generator, method, etc) that contains the reference,
while `Self` refers to the containing class. Inside constructor functions `Self` and `self` both refer
to the containing class. For example:

    function { console.log(self) }

    let SpaceInvader = class {
        constructor { console.log(self, Self) }
        let descend { console.log(self, Self) }
    }

That example would compile (more or less) to this:

    function() { console.log(arguments.callee) }

    const SpaceInvader = class SpaceInvader {
        constructor() { console.log(SpaceInvader, SpaceInvader) }
        descend() { console.log(this.descend, SpaceInvader) }
    }

Note: It is a syntax error to reference `self` outside of anything functional, or to reference `Self`
outside of a class-definition.

Note: You can use `self` and `Self` like any other variables. For example, invoking `self` inside a
function to make a recursive call, or using `self.name` to access its name string *et cetera*.

### The `do` Qualifier

The do-qualifier can qualify the `function` or `generator` keywords to create an IIFE, which is an
expression that immediately invokes the function, and evaluates to its return value. For example:

    do function delta = 0 {
        requestAnimationFrame(self)
        renderer.render(delta)
    }

Note: The "dangling dogballs operator" is legal in Lark, but only implicitly, and it's always
redundant, so please, do not do this:

    function delta = 0 {
        requestAnimationFrame(self)
        renderer.render(delta)
    }()

### The `async` Qualifier

The async-qualifier can qualify the `function` or `generator` keywords to create an asynchronous
version of the respective function-type. For example:

    let read = async function url {
        let response = await fetch(url)
        return await response.text()
    }

### The `do async` Qualifier

The do-async-qualifier can qualify `function` or `generator` to do exactly what you'd expect
(immediately invoke the function and evaluate to a promise):

    let promise = do async function {
        let response = await fetch("/britney/hitme.mp3")
        return await response.arrayBuffer()
    }

### The Arrow Operators

The language defines two arrow operators (`->` and `=>`), which both have prefix and infix forms:

    -> <expression>                 -> function() { return <expression> }
    => <body>                       -> function() <body>

    (<params>) -> <expression>      -> function(<params>) { return <expression> }
    (<params>) => <body>            -> function(<params>) <body>

The params (when present) must be in parenthesis, but are otherwise the same as elsewhere.

When using the skinny-arrow, the expression must be an expression. It must evaluate to something,
which will be returned automatically.

When using the fat-arrow, the body must be properly braced, and must use explicit return-statements
to return anything.

For example:

    let sum = (x, y) -> x + y

    let sum = (x, y) => { return x + y }

    numbers.map((number) -> number * 2)

    numbers.map((number) => { return number * 2 })

    numbers.map(-> @0 * 2)

    numbers.map(=> { return @0 * 2 })

Note: When declaring a function using the arrow grammar, the output is still a full-fat function (using
JavaScript's `function` keyword). However, the name is not implicitly copied to the function.

Note: There is no shorthand for generator literals. They always use `generator <params> <block>`.

The `await` Operator and Asynchronous For-Loops
-----------------------------------------------

In Lark, `await` works exactly like it does JavaScript:

    await <asynchronous-expression>

When we want to iterate over the values yielded by an async-generator, Lark has a for-from loop
(instead of JavaScript's for-await-of loop):

    for <assignees> from <asynchronous-generator-expression>

<!-- Think of a short example. -->

Note: Lark does not use the asterisk for generators. It has a `generator` keyword, and copies the
`yield from <expression>` grammar (described below) from Python.

Yielding from Generators
------------------------

In Lark, `yield *` is spelled `yield from`:

    yield                           -> yield;
    yield item                      -> yield item;
    yield from stream               -> yield * stream;
    item = yield item               -> item = yield item;

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

    foo?()          -> foo?.()
    foo?[x]         -> foo?.[x]

Note: JavaScript's binary infix nullish operator (`??`) is supported (directly):

    a ?? b          -> a ?? b

You can combine the `!` operator with contextual names:

    @!foo           -> this.#foo

You can also do this to initialize parameters in functions (typically constructors):

    class {
        private foo
        constructor @!foo { pass }
    }

That example will desuger to this:

    class {
        #foo;
        constructor(ƥ0) { this.!foo = ƥ0 }
    }

You can also use the dot operator with an index to access sequences:

    foo.0                               -> foo[0]
    x.10.y.20                           -> x[10].y[20]

The `?` operator naturally works with indices too:

    foo?0                               -> foo?.[0]

Note: The `!` operator only works with a name on the righthand side (private indices make no
sense).

Equality Operators
------------------

Lark redid JavaScript's equality operations to use `==` and `!=`, but using the `Object.is`
function:

    a == b          -> Object.is(a, b)
    a != b          -> !Object.is(a, b)

Note: JavaScript's `==`, `===`, `!=` and `!==` operators are not available in Lark.

### The `is` & `is not` Operators

Lark uses the `is` operator instead of JavaScript's `instanceof`, and includes an `is not`
operator for checking the opposite:

    message is String
    message is not String

We use our own static `ƥis` method when available (which is the case for all builtin types). If
`ƥis` is not available on the given type, we fallback to `instanceof`. This allows us to do a
proper type check, based on the given type:

    "Hello, Sailor" is String                   <- true
    String("Hello, Sailor") is String           <- true
    new String("Hello, Sailor") is String       <- true

The righthand operand is just a normal expression (you don't need to specify the type directly):

    let types = [Number, String, Array, Object]
    "Hello, Sailor" is types[1]                 <- true

The statement `console.log(x is T)` compiles to this:

    const ƥ0 = x, ƥ1 = T;
    console.log(ƥ1?.ƥis?.(ƥ0) ?? ƥ0 instanceof ƥ1);

The runtime defines static methods on builtin types (`Number.ƥis`, `String.ƥis` *et cetera*),
which can properly check if the operand is a number or string or whatever, eliminating all the
usual edgecases. The `is` operator falls back to `instanceof`, so funky little object hierarchies
with prototype chains that end in `null` still work correctly.

Note: Everything is an object, except `null` and `void`, so `x is Object` would evaluate to `true` for
any value of `x`, except `null` or `void`. You have to be more specific (also, see the `is of` operator
in the next section).

Note: You cannot use the `is` operator to typecheck `null` or `void`. Instead, you use `x == null` or
`x == void` (without `null` and `void` being conflated):

    x == null                -> Object.is(x, null)
    x == void                -> Object.is(x, undefined)

    null == void             <- false


### The `is of` & `is not of` Operators

As well as providing an `is` operator (for checking the type hierarchy), Lark also has an `is of`
operator for checking if the left operand has the right operand for its constructor (if the right
operand is a function) or its prototype (otherwise). Below are some examples:

    let Bird = class { }            -> const Bird = class Bird {};
    let Duck = class of Bird { }    -> const Duck = class Duck extends Bird {};
    let duck = new Duck()           -> const duck = new Duck();

    duck is Duck                    <- true
    duck is Bird                    <- true
    duck is of Duck                 <- true
    duck is of Bird                 <- false
    Duck is of Function             <- true

    let o = {}                      -> const o = {__proto__: null};
    let oo = {as Object}            -> const oo = {__proto__: Object};

    o is null                       <- false
    o is of null                    <- true
    o is Object                     <- false
    o is of Object                  <- false
    o == null                       <- false

    oo is Object                    <- true
    oo is of null                   <- false
    oo is of Object                 <- true
    oo == Object                    <- false

    duck is Object                  <- true
    duck is of Object               <- false

    let f { return "foo" }          -> const f = function f() { return "foo" };

    f is Function                   -> true
    f is of Function                -> true
    f is Object                     -> true
    f is of Object                  -> false

The `is not` and `is not of` operators obviously have inverted sematics.

While it may seem odd that `of` has different semantics to `is of`, the Lark `of` operator specifically
checks the operand's *own* properties (while `on` checks the properties in the prototype chain). Lark's
`is` operator is used for typechecks. The `is of` operator combines the `is` operator's typechecking
semantics with the `of` operator's has-own-property semantics to express a typecheck that is based
exclusively on the operand's *own* constructor or prototype.

This works by comparing the left operand's prototype to the right operand's `prototype` property when
the right operand is a `Function`, and directly comparing the left operand's prototype to the right
operand otherwise, which compiles to this:

    x is of T                       -> Object.getPrototypeOf(x) === (T?.ƥisOf ?? T)

The runtime binds `Function.prototype.ƥisOf` to a computed (instance) property that returns the value
of the function's `prototype` property. So, when `T` in the example is a `Function` instance, we check
its prototype, but in any other case, we just compare to `T` directly (so you can also check if `x`
has some arbitray object as its prototype or just a `null` prototype).

The `put` Operator
------------------

Note: The `put` operator is a low-precedence, prefix operator that logs its only operand, prefixed
by the name of the containing function and the location of the `put` operator (its line and column
number in brackets):

    let wellNamed = function {
        put "Hello, Sailor"
    }

    const wellNamed = function wellNamed() {
        console.log(`wellNamed [2:5]`, `Hello, Sailor`)
    }

String & Text Literals
----------------------

Lark string literals use quotes (`"`) exclusively (you cannot delimit strings with apostrophes or
grave accents):

    let name = "Lark"

Lark also supports *text literals*, which have extra features that simplify expressing text that
spans multiple lines. Text literals begin with three or more (contiguous) quotes, and end on the
exact same number of quotes that opened that particular literal:

    let doc = """
    The Lark Programming Language
    =============================

    Lark implements a better grammar for JavaScript that you can edit and
    extend to create your own dialects which are fully interoperable with
    Vanilla JS, TypeScript, other Lark dialects *et cetera*.
    """

Text literals exclude the lines that include the quotes, so the expressed string begins on the
first line after the opening quotes, and ends on the last line before the closing quotes. The
indentation level of the closing quotes indicates how much indentation to remove from each of
the lines within the literal. For example, the following statements are equivalent:

    let string = "this line is onside"

    let string = """"""""
        this line is onside
        """"""""

Note: Having eight quotes is redudant in that example (as it doesn't contain a string of seven
quotes), but it demonstrates how the text literal grammar applies when there are at least three,
and how the indentation of the group that closes the literal controls how much indentation to
remove from each line within the literal.

Note: Naturally, two quotes expresses an empty string literal, so anything that follows it would
be interpreted as regular code. Hence, the requirement of at least three.

Indenting the literal in the previous example was only done to demonstrate the gramar. I would
personally recommend keeping everything at the same level (for what it's worth):

    let string = """
    this line is onside
      this line is indented by two spaces
    this line is onside again
    """

The ability to indicate where the lines begin is especially useful when the literal is nested
inside indented code. Otherwise, you get this kind of thing:

    do {
        let string = "this line is onside
      this line is indented by two spaces
    this line is onside again"
    }

Note: Ignoring whitespace, it is a syntax error for a text literal to have any characters after
the opening quotes (on the same line, obviously), before the closing quotes, or inside the
indentation that gets removed from the start of each line (when there is any).

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

Commentary
----------

Comments start on a hash character (`#`), then run to end of the line.

Note: Lark uses `//` for floor-division (`x // y` -> `Math.floor(x / y)`).

By contrived-convention, docstrings use a string literal instead. For example, this is the function
that implements the main loop of the writer stage:

    let walk = function of statements { # internal helper

        "Take a statement iterator, traverse it, and convert each statement to its corresponding
        JavaScript code, adding preambles and semicolons as required, before yielding each top-
        level statement, one at a time. Returns `undefined`."

        for statement of statements if statement.compile {

            let terminated = statement is Header or statement is Label
            let source = indentation + statement.js(api) + (empty when terminated else semicolon)

            yield from preambles
            yield source

            preambles.length = 0
        }
    }

Note: Naturally, you can use text literals as docstrings too. They're just expressions that the
compiler will throw away (as they are not used for anything).

Repeat-While
------------

Lark does not support JavaScript's do-while grammar (`do <block> while <predicate>`). In fact, Lark
does not (and will never) support any kind of block-header grammar. Compound statements always have
the (entire) header at the start, and the (entire) compound part at the end.

Nonetheless, a loop that executes once unconditionally, then zero or more times conditionally, can
be useful. To that end, Lark provides a `repeat` qualifier that qualifies `while` statements, to
form the `repeat while <predicate> <block>` grammar. For example:

    with var x = 8 repeat while x > 0 { put(x -= 1) }           # 7, 6, 5, 4, 3, 2, 1, 0

That code would compile to this JavaScript:

    {
        let x = 8;
        do { console.log(x -= 1) } while (x > 0);
    }

Object Literals
---------------

Object literals infer `null` prototypes, unless a prototype is explicitly set using the magic
`__proto__` key (from JavaScript), or Lark's equivalent `as` prefix-operator:

    let hash = {x: 1}                           -> const hash = {__proto__: null, x: 1}
    let object = {__proto__: Object, x: 1}      -> const object = {__proto__: Object, x: 1}
    let object = {as Object, x: 1}              -> const object = {__proto__: Object, x: 1}

Note: You can use the `as` prefix-operator anywhere within the object literal, but it's a
syntax error to specify more than one prototype (whether you use the `__proto__` key, the
`as` operator).

Dynamic keys use square brackets (just like JavaScript):

    {[foo]: 1, [bar]: 2}                        -> {__proto__: null, [foo]: 1, [bar]: 2}

You can also use names as keys and values, so these two lines are equivalent:

    {x, y, z}
    {x: x, y: y, z: z}

Set Literals
------------

    let numbers = {[1, 2, 3]}                   -> const numbers = Object.freeze(new Set([1, 2, 3]));

Map Literals
------------

    let scores = {{true: 1000, false: -2500}}   -> const scores = Object.freeze(new Map([[true, 1000], [false, -2500]]));


ObjectMutability
----------------

            create  delete  write   read
pack        x       o       o       o
seal        x       x       o       o
freeze      x       x       x       o

Math Operators
--------------

The `Math` object is fully expanded in Lark, making it generally redundant.

The `Math.pow` function is available as the `**` infix-operator, like JavaScript.

The `Math.clz32` (count leading zeros) function is available as the `?` bitwise prefix-operator.

All of the remaining `Math` functions that take a single `Number` argument are available as
Lark prefix-operators:

+ `abs x`: Returns the absolute value of `x`.
+ `acos x`: Returns the arccosine of `x`, in radians.
+ `acosh x`: Returns the hyperbolic arccosine of `x`.
+ `asin x`: Returns the arcsine of `x`, in radians.
+ `asinh x`: Returns the hyperbolic arcsine of `x`.
+ `atan x`: Returns the arctangent of `x` (as a value between `-PI / 2` and `PI / 2` radians).
+ `atanh x`: Returns the hyperbolic arctangent of `x`.
+ `cbrt x`: Returns the cubic root of `x`.
+ `ceil x`: Returns `x`, rounded upwards to the nearest integer.
+ `cos x`: Returns the cosine of `x` (`x` is in radians).
+ `cosh x`: Returns the hyperbolic cosine of `x`.
+ `exp x`: Returns the value of `E ** x`.
+ `expm1 x`: Returns the value of `E ** x - 1` (more accurate than `(exp 1) - 1`).
+ `floor x`: Returns `x`, rounded downwards to the nearest integer.
+ `log x`: Returns the natural logarithm (base-E) of `x`.
+ `log2 x`: Returns the base-2 logarithm of `x`.
+ `log10 x`: Returns the base-10 logarithm of `x`.
+ `log1p x`: Returns the natural logarithm of `1 + x`.
+ `round x`: Rounds `x` to the nearest integer.
+ `fround x`: Returns the nearest 32-bit, single precision, IEEE floating-point representation to `x`.
+ `sign x`: Returns whether `x` is negative, null or positive (-1, 0, 1)
+ `sin x`: Returns the sine of `x`.
+ `sinh x`: Returns the hyperbolic sine of `x`.
+ `sqrt x`: Returns the square root of `x`.
+ `tan x`: Returns the tangent of the angle `x`.
+ `tanh x`: Returns the hyperbolic tangent of `x`.
+ `trunc x`: Returns the integer part of `x`.

For consistancy, the remaining `Math` functions (the ones that take more than one argument) have been
converted to prefix-operators that require a single compound operand instead:

+ `atan2 p`: Returns the arctangent of the quotient of point `p` (compiles to `Math.atan2(p.y, p.x)`).
+ `max c`: Returns the highest value in some collection `c` (compiles to `Math.max(...c)`).
+ `min c`: Returns the lowest value in some collection `c` (compiles to `Math.min(...c)`).

All of JavaScript's `Math` constants (as well as 𝜏, and a randomized number named `R`) are also
available without qualification. However, only four names are used: `E`, `PI` and `TAU` and `R`.
The other constants are expressed by combining existing constants with the corresponding operators
as follows:

+ `R` -> `Math.random()`: A new random number every time.
+ `E` -> `Math.E`: Euler's number.
+ `PI` -> `Math.PI`: The value of π.
+ `TAU` -> `Math.TAU` The value of 𝜏 (2π).
+ `sqrt 2` -> `Math.SQRT2`: The square root of 2.
+ `sqrt .5` -> `Math.SQRT1_2`: The square root of 1/2.
+ `log 2` -> `Math.LN2`: The natural logarithm of 2.
+ `log 10` -> `Math.LN10`: The natural logarithm of 10.
+ `log2 E` -> `Math.LOG2E`: The base-2 logarithm of Euler's number.
+ `log10 E` -> `Math.LOG10E`: The base-10 logarithm of Euler's number.

Note: The Lark runtime includes `Math.TAU = 6.283185307179586`. This is the only name (that's not
prefixed by a Lark Character) that we introduce to any JavaScript namespace. It seems safe to
assume that any future standard that defines `Math.TAU` at all would only ever bind it to
`Math.PI * 2`.

Note: The number `6.283185307179586` is equal to `Math.PI * 2` in JavaScript (and any language that
uses 64-bit IEEE-754 arithmetic), and the exact value of Python's `math.tau` constant as well, so
Lark reuses the same approximation for `TAU`.

Note: Consistant with the `Math` operators, the names `E`, `PI`, `TAU` and `R` will not exist at
runtime. The compiler replaces them with the `Math` constants they map to.

Note: The compiler detects when the constants listed above are passed to the corresponding operators,
replacing the invocations with the corresponding constants (so `log 10` compiles to `Math.LN10`). As
long as the expression is a literal that evaluates to the appropriate value, the compiler optimizes
it away. For example, `log 2`, `log 2.0`, `log 2.000000`, `log 0x0002` and `log .0002e4` would all
be optmized to the `Math.LN2` constant.

Keywords and Named Prefix Operators
-----------------------------------

Lark has four types of word token: Reserved words, keywords, (named) operators and variable names
(symbols). The first three types are defined by membership in the arrays of spellings exported
from `./user/spellings.js` (`reserved`, `keywords` and `operators`). You can declare any word
that's not in those arrays as a name.

Naturally, reserved words never appear in valid code, and there's not much to say about variable
names. Keywords and operators generally do what you expect as well. However, Lark does handle the
distinction between its keywords (that naturally begin statements), and its named prefix-operators
(that naturally begin expressions, which can also incidentally start statements) differently.

JavaScript generally avoids the need for a distinction at all by classifying words like `throw`,
`delete` and `yield` as prefix-operators: If `delete x` is a valid expression, then it's also a
valid statement, so you can just use `delete x` anywhere, and never need to worry about it. It's
a distinction without a difference.

While JavaScript's approach seems ideal, it has edgecases. If a JavaScript statement begins with
the `function` keyword, it becomes a function-statement (which is not a valid expression), so you
can not invoke the function (to form a IIFE) in this one specific case. This requires workarounds,
like wrapping the function in parens, or adding a noop prefix operation (like `void` or `!`).

Lark never trades consistancy for simplicity, so we had to address this issue, accepting that some
words will just be keywords (like `for` and `while` - words that only ever begin statements), other
words will just be prefix operators (like `sin` and `floor` - words that act like any other prefix
operators), and there will also be words (like `function`) that are both keywords and operators
(forming formal statements and valid expressions).

Once a language adds (proper) support for keyword-operators (so they just work), there's no reason
to avoid them. In Lark, words like `throw`, `delete` and `yield` (as well as `function`) all form
formal statements (that are also valid expressions) when used to begin a statement, and all form
plain expressions in any other context.

For consistancy, Lark treats all named prefix-operators (including `pack`, `seal`, `freeze` etc)
as keyword-operators, except the core ALU operators (arithmetic, comparison, logical and bitwise
operators, including `not`, `cos`, `tan` etc). They are plain operators, and cannot introduce
formal statements.

Note: This distinction has no relevance to named infix-operators (like `and` and `or`). They are
just plain operators in any context. Likewise for suffix operators (like `is packed`).

Note: The distinction between formal and informal statements is purely lexical (only determining when
curly braces are optional around control-flow blocks). Beyond that sugar, keywords and operators just
do the correct thing implicitly.

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
--- Decrement (++)
++ delete operator
++ Destructuring assignment
++ Division (/)
++ Floor Division
++ Division assignment (/=)
--- Equality (==)
++ Exponentiation (**)
++ Exponentiation assignment (**=)
++ Greater than (>)
++ Greater than or equal (>=)
++ Grouping operator ( )
++ import.meta
+ import()
++ in operator
++ of operator
--- Increment (++)
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
++ for-of
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

++ function
++ generator
++ async function
++ async generator
++ do function
++ do generator
++ do async function
++ do async generator
+ class

## Misc

++ debug
+ import
+ export
++ labels

Promotions
==========

+ `sin <expression>`            -> Math.sin(<expression>)
+ `cos <expression>`            -> Math.cos(<expression>)
+ `tan <expression>`            -> Math.tan(<expression>)
+ *et cetera*

Inline WebAssembly
==================

Once Lark is up and runnnig, I might integrate PHANTASM into Lark, as a form of inline assembly.

I like both languages, but the PHANTASM implementation needs redoing. They have enough similarities
with each other that it'd be relative simple to reimplement something similar to PHANTASM directly
inside Lark.

WASM is useless without at least *some* JS, so you always need some Lark to use PHANTASM now. In
the future, a Lark source file would always equate to exactly one JS module, and optionaly one
WASM module that would be automatically loaded and integrated into the JS module, so Lark
would be able to invoke PHANTASM functions, read and write to the memory *et cetera*.

You could prefix imports and exports with a new `wasm` qualifier, and they'll then require a type,
and be added to the WASM module instead:

    wasm import { sum } from "./lib/helpers.wasm" as $sum of i32, i32 -> i32

We could make `subroutine` a keyword that works like `function` and `generator`, and require types
on the parameters (using labels) *et cetera*, and add support for PHANTASM type-expressions:

    subroutine $sum of x: i32, y: i32 -> i32 {
        get 0, get 1, add i32
    }

    subroutine $sum of type $binop { get 0, get 1, add i32 }

Note: Lark and PHANTASM both use a comma or a newline to terminate a statement/instruction/command.
PHANTASM also uses significant indentation, but that would need to be replaced with curly brackets,
if it was integrated into Lark (which ignores indentation outside of text literals).

Note: The type (`$binop`) would be referencing the PHANTASM indexspace, while the function name
(`$sum`) would exist in the PHANTASM and Lark namespaces, so you could then do this:

    console.log($sum(1, 2))

Functions could be prefixed with `private` to make them internal to the WASM module, or we could
make things internal by default, and then have a `public` keyword (or something). I only thought
of doing this twenty minutes ago...

Should we reserve dollar names for this (as we can use `js.$name` as an escape hatch)??

The Hierarchy
=============

All of the abstract base classes in the token hierarchy:

// Token, Terminal, Terminator, Delimiter, Opener, Closer,
// Caller, Word, Constant, Keyword, Declaration, ClassQualifier,
// BranchStatement, CommandStatement, Header, PredicatedBlock, Functional,
// Operator, PrefixOperator, InfixOperator, GeneralOperator,
// DotOperator, GeneralDotOperator, ArrowOperator, AssignmentOperator,

All of the concrete classes in the token hierarchy:

// NumberLiteral,
// StringLiteral,
// TextLiteral
// FullFunction,
// AllConstant,
// ArgumentsConstant,
// And,
// AND,
// ARSHIFT,
// As,
// Ask,
// Assert,
// Assign,
// AssignAND,
// AssignARSHIFT
// AssignFloor,
// AssignPlus,
// AssignLSHIFT,
// AssignMinus,
// AssignModulo,
// AssignOR,
// AssignRaise,
// AssignRSHIFT,
// AssignStar,
// AssignXOR,
// Async,
// Await,
// Bang,
// Break,
// Class,
// CloseBrace,
// CloseBracket,
// CloseParen,
// Label,
// Comma,
// Continue,
// Debug,
// DefaultConstant,
// Dev,
// Do,
// Dot,
// Else,
// Equal,
// EOF,
// Export,
// FalseConstant,
// FatArrow,
// Floor,
// For,
// Freeze,
// From,
// Frozen,
// Generator,
// GlobalConstant,
// Greater,
// If,
// Import,
// In,
// InfinityConstant,
// Is,
// Lesser,
// Let,
// LineFeed,
// Local,
// LSHIFT,
// Minus,
// Modulo,
// NaNConstant,
// New,
// Not,
// NotEqual,
// NotGreater,
// NotLesser,
// NullConstant,
// Nullish,
// Of,
// Or,
// OR,
// OpenBrace,
// OpenBracket,
// OpenParen,
// Pack,
// Packed,
// Pass,
// Plus,
// Private,
// Raise,
// RandomConstant,
// Reserved,
// Return
// RSHIFT,
// Seal,
// Sealed,
// SkinnyArrow,
// Slash,
// Spread,
// Star,
// Static,
// Subclass,
// SuperConstant,
// ThisConstant,
// Throw,
// TrueConstant,
// Unless,
// Until,
// Var,
// Variable,
// VoidConstant,
// When,
// While,
// XOR,
// Yield,

Import Directives
-----------------

Static imports use *import directives*. Directives are similar to statements, but must occur
before any statements, and must end on a newline or a comment (you cannot use commas between
directives).

The abstract grammar of a import directive looks like this:

    from <url> [with <assertions>] import <imports>

The `<url>` is a URL string that is implicitly delimited by the whitespace at each end.

When present, the `<assertions>` can either be a single, labelled value, or a comma-separated
list of one or more labelled values, wrapped in braces. However, the latter option is only
provided for future-proofing (as the only valid assertions, given the current spec, are
all limited to a single entry).

The `<imports>` construct is a comma-separated list of `<import>` constructs, which is analogous
to a `<params>` construct being a comma-separated list of `<param>` constructs. The two are very
similar (grammatically), but not identical. Firstly, `<imports>` do not support splats (there is
no way to gather a bunch of imports into an array). However, you can unpack an imported array or
object (exactly like an `<assignees>` construct). Secondly, the list of `<imports>` runs to the
end of the line, unless it's wrapped in curly braces (where it can span any number of lines).

Note: As with function parameters, in Lark, any names declared by an `<import>` are implicitly
constant, unless prefixed by `let` (making them explicitly constant) or `var` (making them
explicitly variable).

Below is a list of the various parameter-forms (omitting their optional declarators):

+ `<name>` Import the named value and bind it to the same name.
+ `<name>: <assignees>` Import the named value, and unpack it into the `<assignees>` as normal.
+ `default: <name>` Import the default value and bind it to the name.
+ `module: <name>` Bind the module to the name.

Note: As ever, the colon implies the word *as*.

Note: The `<name>: <assignees>` form includes the `<name>: <name>` form, which imports the left
name, and binds it to the right name (`import foo: bar` means *import `foo` as `bar`*).

Note: The `<import>` declarators (whether implied or explicit) apply to the things being declared,
so an `<import>` like `color: [r, g, b]` makes `r`, `g` and `b` (implicitly) constant. You would
use something like `var color: [r, g, b]` to make `r`, `g` and `b` variable.

Note: While you can import an export named `module`, you can not import it by any other name. This
is a limitation that arises from an ambiguity in the grammar (and the fact that `module` is not a
reserved word in JavaScript, and Lark supports importing modules written in JavaScript). We could
introduce a special case (for example, adding a `module <name>` instruction for `<imports>`, like
the `splat <expression>` instruction for params and arguments, or `as <expression>` for object
literals). However, in the exceptional case that you need to import something named `module`
by a different name, you can just import `module`, then assign it to another name. So, at
least for now, we opted to simply live with the limitation.

Examples:

    from ./source/lexer.lark import lex

    from ./source/parser.lark import default: parser

    from ./helpers.js import send, fetch, load, store

    from ../data/jan2024.json with type: "json" import jan2024

    from ../data/current.json with {type: "json"} import var currentData

    from ../../vendor/lib.js import color: [r, g, b], controller: {b: a, a: b, y: x, x: y}

    from https://cdn.com/framework/main.js import {
        Form, Button, Textfield, ColorPicker,
        Model, View, Controller,
    }

You can also use `import <url>` to import a module, without importing anything *from* the
module:

    import ./runtime.js

asm multiply of x: i32, y: i32 returns i32 {
    return x * y
}

put multiply(1, 1)

let load = function of filepath {
    let file = open(filepath)
    defer { file.close() }
    return file.toString()
}

const load = function(filepath) {
    try {
        const file = open(filepath);
        return file.toString();
    } finally { file.close() }
};


let SpaceInvader = class of Sprite {

    this.spritesheet = load("../sprites/invader.png")

    prototype.foo = function of x {

    }
}

Observations Regarding Primitives
=================================

Packing, sealing or freezing `null` or `void` is a noop (the result is the same value).

Packing, sealing and freezing primitives has no effect when the value is a proper primitive, though
*primitive objects* can be packed, sealed or frozen (as objects).

Symbols and BigInts are *strict primitives* (they cannot be called with `new`).

Regexs are objects.

Operators that Coerce Operands to Primitves
-------------------------------------------

JavaScript Operators that coerce to a primitive: Unary arithmetic (`-`, `+`, `--` and `++`), binary
arithmetic (`+`, `-`, `*`, `/`, `%` and `**`), bitwise-not (`~`) and the bitwise infix operators
(`<<`, `>>`, `>>>`, `&`, `|` and `^`), the four comparison operators (`<`, `>`, `<=` and `>=`),
as well as `in`, `delete` and bracket notation (when using primitive keys). For example:

    const o = {foo: 1, bar: 2};
    new String("foo") in o                  // true
    o[new String("foo")]                    // 1
    delete o[new String("foo")]
    o                                       // {bar: 2}

JavaScript operators that do not, but probably should, coerce to a primitive: Equality (`==`, `!=`, `===`
and `!==`), logic (`&&`, `||` and `!`) and the ternary operator (the predicate is not coerced). For example:

    new String("foo") === "foo"             // false
    new Boolean(false) || true              // Boolean {false}
    new Boolean(false) ? "true" : "false"   // "true"

JavaScript operators where it would make no sense to coerce to a primitive: Breadcrumbs, invocations (including
with `new` and `import`), `void`,  `await`, `typeof` and `instanceof`, the nullish operator (`??`), the entire
set of assignment operators, splats, `yield` and `yield *`, the arrow operator and commas.

JavaScript Operator Precedence
==============================

18) Grouping: `(x + y) * z`
17) Dot Operators: `foo.bar`, `foo?.bar`
    Bracket Notation: `o[i]`,
    Invocations: `f(x)`, `new Foo()`
    Dynamic Imports: `import(item)`
16) New	without argument list: `new Foo`
15) Suffix Operators: `i++`, `i--`
14) Prefix Operators: `++i`, `--i`, `!x`, `~x`, `+x` `-x`, `typeof primitive`, `void x`, `delete foo`, `await call`
13) Exponentiation(right-associative): `x ** y`
12) Multiplicative Operators: `x * y`, `x / y`, `x % y`
11) Additive Operators: `x + y`, `x - y`
10) Bitwise Shift Operators: `x << y`, `x >> y`, `x >>> y`
09) Comparison Operators: `x < y`, `x <= y`, `x > y`, `x >= y`, `x in o`, `x instanceof T`
08) Equality Operators: `x == y`, `x != y`, `x === y`, `x !== y`
07) Bitwise AND: `x & y`
06) Bitwise XOR: `x ^ y`
05) Bitwise OR: `x | y`
04) Logical AND: `x && y`
03) Logical OR: `x || y`, `x ?? y`
02) Assignment: `name = x`, `name += x`, `name ??= x` etc
    Ternary Operator(right-associative): `x ? y : z`
    Arrow Operator(right-associative): `x => 2 ** x`
    Thread Operators: `yield tokens`, `yield * tokens`
    Rest Operator: `...args`
01) Comma: `x, y, z`

Lark Operator Precedence
========================

18) Grouping: `(x + y) * z`
17) Accessors: `foo.bar`, `foo?bar`, `foo!bar`, `o[i]`, `f(x)`, `new Foo()`, `import(item)`
16) New	without argument list: `new Foo`
15) Suffix Operators: `i++`, `i--`
14) Prefix Operators: `++i`, `--i`, `!x`, `~x`, `+x` `-x`, `typeof primitive`, `void x`, `delete foo`, `await call`
13) Exponentiation(right-associative): `x ** y`
12) Multiplicative Operators: `x * y`, `x / y`, `x % y`
11) Additive Operators: `x + y`, `x - y`
10) Bitwise Shift Operators: `x << y`, `x >> y`, `x >>> y`
09) Comparison Operators: `x < y`, `x <= y`, `x > y`, `x >= y`, `x in o`, `x instanceof T`
08) Equality Operators: `x == y`, `x != y`, `x === y`, `x !== y`
07) Bitwise AND: `x & y`
06) Bitwise XOR: `x ^ y`
05) Bitwise OR: `x | y`
04) Logical AND: `x && y`
03) Logical OR: `x || y`, `x ?? y`
02) Assignment: `name = x`, `name += x`, `name ??= x` etc
    Ternary Operator(right-associative): `x ? y : z`
    Arrow Operator(right-associative): `x => 2 ** x`
    Thread Operators: `yield tokens`, `yield * tokens`
    Rest Operator: `...args`
01) Comma: `x, y, z`