---
layout: post
title: "Math-Haskell Rosetta Stone - Part 1"
date: 2024-09-24
permalink: /blog/math-haskell-rosetta-stone/
comments: true
tags:
  - haskell
  - math
  - set theory
  - category theory
---

This post begins a short series meant to serve as an informal guide to reading Haskell code and translating back and forth with mathematics.
It's meant to help members of [r/CategoryTheory](https://reddit.com/r/categorytheory) understand posts that use Haskell code to convey ideas.
My hope is that this series should also find use among Haskell programmers, as exposure to some of the basic methods and terminology used in modern math.

<!--break-->

- [Part 1 (this post)](#)
<!-- Generated TOC below this line -->
  - [Basic types and functions](#basic-types-and-functions)
    - [Pattern matching and variables](#pattern-matching-and-variables)
    - [A note on multi-variable functions](#a-note-on-multi-variable-functions)
  - [Simple sum types and branching](#simple-sum-types-and-branching)
    - [Branching logic](#branching-logic)
    - [If-then-else and guards](#if-then-else-and-guards)
  - [Algebraic data types](#algebraic-data-types)
    - [Product types and universal projection maps](#product-types-and-universal-projection-maps)
    - [Sum types and universal inclusion maps](#sum-types-and-universal-inclusion-maps)
  - [Closing](#closing)
<!-- Generated TOC above this line -->
- Part 2 (pending)
  - Type variables
  - Type classes
  - Constructor classes
- Part 3 (pending)
  - Kinds
  - Type-level programming


## Basic types and functions

Math is concerned with defining _sets_ and _functions._
Analogously, Haskell code is concerned with defining _types_ and _functions._

{% highlight haskell %}
data RealVec2 = RV2 RealNumber RealNumber

rv2Norm :: RealVec2 -> RealNumber
rv2Norm (RV2 u v) = sqrt (u**2 + v**2)

rv2Scale :: RealNumber -> RealVec2 -> RealVec2
rv2Scale r (RV2 x y) = RV2 (r*x) (r*y)

rv2Add :: RealVec2 -> RealVec2 -> RealVec2
rv2Add (RV2 a1 a2) (RV2 b1 b2) = RV2 (a1+b1) (a2+b2)
{% endhighlight %}

The above code roughly translate to the following definitions:

- A set $$\mathrm{RealVec2}$$,

$$
  \mathrm{RealVec2} = \{ (\mathrm{RV2}, x, y) \mid x, y \in \mathbb{R} \}
$$

- A single-variable function $$\mathrm{rv2Norm}$$,

$$
  \mathrm{rv2Norm} : \mathrm{RealVec2} \to \mathbb{R} \\
  \mathrm{rv2Norm}\big((\mathrm{RV2}, u, v)\big) = \sqrt{u^2 + v^2}
$$

- A two-variable function $$\mathrm{rv2Scale}$$,

$$
  \mathrm{rv2Scale} : \mathbb{R} \times \mathrm{RealVec2} \to \mathrm{RealVec2} \\
  \mathrm{rv2Scale}\big( r, (\mathrm{RV2}, x, y) \big) = ( \mathrm{RV2}, rx, ry )
$$

- A two-variable function $$\mathrm{rv2Add}$$, and

$$
  \mathrm{rv2Add} : \mathrm{RealVec2} \times \mathrm{RealVec2} \to \mathrm{RealVec2} \\
  \mathrm{rv2Add}\big( (\mathrm{RV2}, a_1, a_2), (\mathrm{RV2}, b_1, b_2) \big) = ( \mathrm{RV2}, a_1 + b_1, a_2 + b_2 )
$$

- A two-variable function $$\mathrm{RV2}$$.

$$
  \mathrm{RV2} : \mathbb{R} \times \mathbb{R} \to \mathrm{RealVec2} \\
  \mathrm{RV2}(r_1, r_2) = ( \mathrm{RV2}, r_1, r_2 )
$$

Straightforward enough (except maybe for the $$\mathrm{RV2}$$ bit).
Let's dig into the syntax of the Haskell code to understand each part.

{% highlight haskell %}
data RealVec2 = RV2 RealNumber RealNumber
-- ^ `data`, keyword that begins a type definition
--   ^^^^^^^^ `RealVec2`, the name we give this function
--              ^^^ `RV2`, a "data constructor", or "tag", we define
--                  ^^^^^^^^^^ ^^^^^^^^^^ `RealNumber`, a reference to an already-defined type
--                                        we define `RV2` as having two "fields," or "arguments"
--                                        each field contains a data object of type `RealNumber`
{% endhighlight %}


### Pattern matching and variables

The tag `RV2` does a number of things.

- It ensures that the type `RealVec2` has no members in common with any other type (this is one reason why it's called a _tag_).
- It acts as a function that allows the programer to create data objects of type `RealVec2`.
  This is why it's called a _data constructor,_ and we use `RV2` in this capacity in the formulae of `rv2Scale` and `rv2Add`.
- Third, notice that `RV2` is an injective function, allowing us to use `RV2` to refer to arbitrary members of `RealVec2`.
  This is the main sense in which `RV2` is a _tag:_ every member of `RealVec2` has a unique form `RV2 x y` for some particular `x` and `y` in `RealNumber`.
  We take advantage of this fact in the definitions of `rv2Norm`, `rv2Scale`, and `rv2Add`.

{% highlight haskell %}
rv2Norm :: RealVec2 -> RealNumber
-- ^^^^ define a function named `rv2Norm`
--      ^^ `rv2Norm` has type `RealVec2 -> RealNumber`
--         ^^^^^^^^ `rv2Norm` has domain type `RealVec2`
--                     ^^^^^^^^^^ `rv2Norm` has codomain type `RealNumber`

rv2Norm (RV2 u v) = sqrt (u**2 + v**2)
--                ^ equation defining `rv2Norm`
--      ^^^^^^^^^ An arbitrary member of `RealVec2` must match this pattern
--                  ^^^^^^^^^^^^^^^^^^ formula that computes the value of `rv2Norm (RV2 u v)`
{% endhighlight %}

Expressing a data object in terms of a data constructor is called _pattern matching._
Pattern matching is ubiquitous in Haskell.
To define `rv2Norm`, we match its argument against the pattern that the `RV2` data constructor follows: namely, the symbol `RV2` followed by two variables representing the data objects contained in the fields of `RV2`.
We do likewise in the definitions of `rv2Scale` and `rv2Add`.

{% highlight haskell %}
rv2Scale :: RealNumber -> RealVec2 -> RealVec2
rv2Scale r (RV2 x y) = RV2 (r*x) (r*y)
--       ^ `r`, a variable representing the first argument of `rv2Scale`
--         ^^^^^^^^^ `(RV2 x y)`, pattern matching the second argument of `rv2Scale`
--              ^ `x`, a variable representing the contents of the first field of `RV2`
--                ^ `y`, a variable representing the contents of the second field of `RV2`
--                     ^^^ `RV2`, used to construct a data object
--                         ^^^^^ `(r*x)`, the contents we intend for the first field
--                               ^^^^^ `(r*y)` the contents we intend for the second field

rv2Add :: RealVec2 -> RealVec2 -> RealVec2
rv2Add (RV2 a1 a2) (RV2 b1 b2) = RV2 (a1+b1) (a2+b2)
--     ^^^^^^^^^^^ pattern match the first argument of `rv2Add`
--                 ^^^^^^^^^^^ pattern match the second argument of `rv2Add`
--          ^^ `a1`, name we choose for the first field of the first argument
--             ^^ `a2`, name we choose for the second field of the first argument
--                      ^^ `b1` we name we choose for the first field of the second argument
--                         ^^ `b2`, name we choose for the second field of the second argument
{% endhighlight %}

Again, the names we choose for variables are arbitrary.

Before we move on, I should take a moment to be clear about what I mean by the word _variable_ in this context.
You might already know that programmers often use the word _variable_ to refer to a region of program memory that can be referenced, read from, and written to by the program at various times during execution.
That is not what I mean by _variable_ in this sense.
In this post, I use _variable_ to mean an abstract symbol used as a placeholder for a functions eventual argument, that is, the usual meaning of the word, as it's used in math.


### A note on multi-variable functions

Next, it's time to have a little talk about the number of variables a Haskell function has.

{% highlight haskell %}
rv2Scale :: RealNumber -> RealVec2 -> RealVec2
-- ^^^^^ defining a function named `rv2Scale`
--          ^^^^^^^^^^ domain type is `RealNumber`
--                        ^^^^^^^^^^^^^^^^^^^^ codomain type is `RealVec2 -> RealVec2`
rv2Scale r (RV2 x y) = RV2 (r*x) (r*y)
{% endhighlight %}

When reading type signatures, the right-most function arrow (the `->`) takes precedence.
The above code would be equivalent to the following (valid) code.

{% highlight haskell %}
rv2Scale :: RealNumber -> (RealVec2 -> RealVec2)
rv2Scale r = f
  where f (RV2 x y) = RV2 (r*x) (r*y)
{% endhighlight %}

Earlier, I conceived of the math counterpart to `rv2Scale` as mapping $$\mathbb{R} \times \mathrm{RealVec2}$$ to $$\mathrm{RealVec2}$$.
Were I to maintain fuller fidelity to the meaning of the Haskell code, though, I would have used the following instead.

$$
  \mathrm{rv2Scale} : \mathbb{R} \to \mathop{\mathrm{Hom}}\big(\mathrm{RealVec2},\mathrm{RealVec2}\big)
$$

A Haskell function may take one, and only one, argument.
When we speak of a _multi-variable_ function in Haskell, we really mean a function of one variable that returns a function.
This might seem odd at first, but please consider that Haskell actually agrees with our notion of function arguments in math.
When we speak colloquially of a two-variable (math) function, what we truly speak of is a function of a single variable---the domain merely happens to be a cartesian product.

$$
\mathrm{rv2Scale} : \underbrace{\mathbb{R} \times \mathrm{RealVec2}}_{\text{this is still just one set!}} \to \mathrm{RealVec2} \\
\mathrm{rv2Scale} \underbrace{\big( r, (\mathrm{RV2}, x, y) \big)}_{\text{this is still just one argument!}} = ( \mathrm{RV2}, rx, ry )
$$

As it is in math, so it is in Haskell: all functions are single-variable functions.

But still, instead of returning a function, I could have defined `rv2Scale` as taking a pair as argument.
Haskell has built-in syntax for ordered pairs, even.
The type `(RealNumber, RealVec2)` is the Haskell analog of the set $$ \mathbb{R} \times \mathrm{RealVec2} $$.
We could have used that to define `rv2Scale`.

{% highlight haskell %}
rv2Scale :: (RealNumber, RealVec2) -> RealVec2
rv2Scale (r, RV2 x y) = RV2 (r*x) (r*y)
{% endhighlight %}

The reason haskellers favor using the formulation `RealNumber -> (RealVec2 -> RealVec2)` (which is call the _curried_ form) over the equivalent formulation `(RealNumber, RealVec2) -> RealVec2` (which is call the _uncurried_ form) is simply due to Haskell's syntax.
You must have noticed by now that Haskell's syntax rules allow you to omit the parentheses normally used to indicate function application.
Function application parentheses may be omitted whenever the argument consists of a single symbol.

{% highlight haskell %}
rv2Reflect :: RealVec2 -> RealVec2
rv2Reflect (RV2 x y) = RV2 y x
--                         ^ first argument is a single-symbol, no parentheses needed
--                           ^ second argument is a single-symbol, no parentheses needed
--         ^^^^^^^^^ argument is a compound expression, requires parentheses
{% endhighlight %}

Because Haskell allows you to omit function-application parentheses, the curried formulation ends up being far more convenient to use and (arguably) easier to read than the uncurried formulation.
There isn't a deeper reason than that.

In summary, the code we've seen so far defines:

- a type, `RealVec2`;
- a function `RV2`, with domain type `RealNumber` and codomain type `RealNumber -> RealVec2`;
- a function `rv2Norm`, with domain type `RealVec2` and codomain type `RealNumber`;
- a function `rv2Scale`, with domain type `RealNumber` and codomain type `RealVec2 -> RealVec2`;
- a function `rv2Add` with domain type `RealVec2` and codomain type `RealVec2 -> RealVec2`; and
- a function `rv2Reflect` with domain type `RealVec2` and codomain type `RealVec2`.

Since their codomains are function types, `rv2Scale`, `rv2Add`, and `RV2` all yield functions.
Even though it's a little bit of a lie, it's often convenient to think of such function-yielding-functions as having multiple variables.

- Think of `rv2Scale` as having two inputs, with types `RealNumber` and `RealVec2`, and an output of type `RealVec2`.
- Think of `rv2Add` as having two inputs, both with type `RealVec2`, and an output of type `RealVec2`.
- Think of `RV2` as having two inputs, both with type `RealNumber`, and an output of type `RealVec2`. `RV2` is injective when thought of this way, a fact we use when we pattern match.

Readers may choose to skip this next aside, unless they feel like _really_ nerding out.

{% aside %}
Often when used in programming, the word _function_ means something more like _procedure_ or _subroutine._
In most programming languages, functions tend to "do" things; they're state transitions, reading from and operating on state variables defined in the running program.
Such functions might have no explicit arguments, as they can read state variables defined in the program.
Similarly, they might not have a explicit return values, as such functions can communicate their data by modifying the values of state variables defined in the program.
In this manner, program state variables act as implicit arguments and return values, accessible to all function in that program.
Programmers seem to like this state of affairs, at it makes it trivially easy to propagate data throughout the program.

From a mathematical point of view, modelling such functions as sets of ordered pairs can prove challenging.
Suppose $$S$$ is the set of all possible states of a program $$P$$, and suppose `f` is a function in $$P$$ with set of permissible inputs $$A$$ and set of potential outputs $$B$$.
We model `f` mathematically via a counterpart function $$f : A×S → S×B$$.
By including an $$S$$ component in the domain and codomain, $$f$$ makes explicit the dependencies and consequences `f` has on program state.
This approach usually works well enough for modelling single-threaded programs.
It is utterly inadequate for modelling multi-threaded programs, as the presence of simultaneous threads of computation can cause the $$S$$ argument to change unpredictably midway through computing `f`.

Functions in Haskell can't do any of that.
The language syntax simply makes it impossible to describe state variables, let alone reading or writing to them.
So, a function in Haskell is a genuine function, in the sense that it yields a computed value that is completely determined by its arguments, conceptually equivalent to a set of ordered pairs.
As a result, data propagation in Haskell is always limited entirely to functions' arguments and its computed results.
{% endaside %}


## Simple sum types and branching

Haskell ships with a standard library called _base._
_base_ comes with several useful data types predefined.
Among them are `Bool` and `Ordering`.

{% highlight haskell %}
data Bool = False | True
data Ordering = LT | EQ | GT
{% endhighlight %}

In math, they would look something like this.

$$
  \mathrm{Bool} = \{ \mathrm{False}, \mathrm{True} \} \\
  \mathrm{Ordering} = \{ \mathrm{LT}, \mathrm{EQ}, \mathrm{GT} \}
$$

Each of `False`, `True`, `LT`, `EQ`, and `GT` is a data constructor with no fields.
Conceptually they are constants, uniquely identified and referenced by their respective names.


### Branching logic

The _base_ library defines various operations on these data types, such as the familiar boolean operations.

{% highlight haskell %}
not :: Bool -> Bool
not True = False
not False = True

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(&&) :: Bool -> Bool -> Bool
p && q =
  case (p, q) of
    (True, True) -> True
    (True, False) -> False
    (False, True) -> False
    (False, False) -> False
{% endhighlight %}

Here we see our first examples of branching logic in a Haskell function.
Fundamentally, all branching in Haskell is done by pattern matching.

- The branching in `not` and `(||)` is accomplished by defining several different equations.
  Each such equation matches a different pattern of the arguments, in other words, a different combination of data constructors.

- `(&&)` branches using a `case` expression.
  Each case of the `case` expression matches a different pattern.

I could have defined `(&&)` with multiple equations, like `(||)`, but I wanted to have an example that used a case expression.

We also see our first example of operators.
By giving them symbolic names, Haskell knows that we intend the functions `(&&)` and `(||)` to be binary operators.
This is reflected in the equations defining these functions.

Final note on these before moving on, each of `(&&)` and `(||)` really just needs two cases.
In fact, defining these functions this way may cause the program to do more computations than necessary.
Accordingly, Haskell syntax allows us to use an underscore (the character `_`) to indicate a match-anything pattern.

{% highlight haskell %}
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False
{% endhighlight %}

{% aside %}
You may have noticed that we use parentheses to refer to a symbolic operator and we omit parentheses when we invoke a symbolic operator.
For example, we write `(||)` to refer to the boolean disjunction operator.
We write `||` to invoke the boolean disjunction operator.
{% endaside %}


### If-then-else and guards

_base_ provides a type `Double` that implements [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) double-precision floating point arithmetic.
Let's define a function that compares `Double`s and yields an `Ordering`.

{% highlight haskell %}
compareDoubles :: Double -> Double -> Ordering
compareDoubles x y =
  case x < y of
    True -> LT
    False -> case x > y of
      True -> GT
      False -> EQ
{% endhighlight %}

`x < y` is a member of `Bool`, which we then pattern match.
Pattern matching a member of `Bool` is such common idiom that Haskell has a syntactic shortcut for it.

{% highlight haskell %}
compareDoubles :: Double -> Double -> Ordering
compareDoubles x y =
  if x < y
    then LT
    else if x > y
      then GT
      else EQ
{% endhighlight %}

We can style that in various different ways, to our preference.

{% highlight haskell %}
compareDoublesAlt1 :: Double -> Double -> Ordering
compareDoublesAlt1 x y =
  if x < y then
    LT
  else if x > y then
    GT
  else
    EQ

compareDoublesAlt2 :: Double -> Double -> Ordering
compareDoublesAlt2 x y =
  if x < y then LT else
  if x > y then GT else
  EQ

compareDoublesAlt3 :: Double -> Double -> Ordering
compareDoublesAlt3 x y = if x < y then LT else if x > y then GT else EQ
{% endhighlight %}

All of those are still a bit clunky, so Haskell syntax has a shorthand for checking a series of conditions.
The syntax is reminiscent of defining a piecewise function.

{% highlight haskell %}
compareDoubles :: Double -> Double -> Ordering
compareDoubles x y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ
{% endhighlight %}

The translation to math is straightforward.

$$
  \mathrm{compareDoubles} : \mathrm{Double} \times \mathrm{Double} \to \mathrm{Ordering} \\
  \mathrm{compareDoubles}(x, y) =
    \begin{cases}
      \mathrm{LT} & \text{if $x < y$} \\
      \mathrm{GT} & \text{if $x > y$} \\
      \mathrm{EQ} & \text{otherwise}
    \end{cases}
$$

## Algebraic data types

Recall the types we've seen so far.

{% highlight haskell %}
data RealVec2 = RV2 RealNumber RealNumber

data Bool = False | True

data Ordering = LT | EQ | GT
{% endhighlight %}

`RealVec2` builds a type by packaging data objects from several other types into a single conceptual unit.
Virtually all programming languages have this feature, known under various different names.
Some call such types _structs,_ some languages call them _objects,_ other languages call them _records,_ and so on.

`Bool` and `Ordering` are defined by enumerating their members.
Many programming languages support defining _enumeration types,_ or _enums,_ similarly.

In Haskell lingo, `RealVec2` is an example of a _product type._
`Bool` and `Ordering` as special cases of _sum types,_ though we will see sum types that are much more general.
We will now see that the terminology, "product" and "sum," is deserved.


### Product types and universal projection maps

If you're very clever, you may already have recognize `RealVec2` as a categorical product of `RealNumber` with itself.
We need to define the universal projection maps and verify the universal product property to make sure we're right.

{% block proposition Claim 1 %}
`RealVec2` is a product of `RealNumber` and `RealNumber`, with projection maps `p1 :: RealVec2 -> RealNumber` and `p2 :: RealVec2 -> RealNumber` defined by `p1 (RV2 x _) = x` and `p2 (RV2 _ y) = y`.
{% endblock %}

{% block proof Proof %}
Let `T` be a type, and suppose we have `f1 :: T -> RealNumber` and `f2 :: T -> ReadNumber`.
Define `f :: T -> RealVec2` by `f t = RV2 (f1 t) (f2 t)`.
We need to show that `f1` and `f2` factor through `f`.
To this end, take any `t` in `T`.
We have
```
(p1 . f) t === p1 (f t) === p1 (RV2 (f1 t) (f2 t)) === f1 t
```
and
```
(p2 . f) t === p2 (f t) === p2 (RV2 (f1 t) (f2 t)) === f2 t
```
so `f1 === p1 . f` and `f2 === p2 . f`, as required.

Next, we need to show that `f` is the only function through which `f1` and `f2` factor in this way.
Suppose that `g :: T -> RealVec2` is any function satisfying `f1 === p1 . g` and `f2 === p2 . g`.
Take any `t` in `T`, and let `g t === RV2 u v` for some `u` and `v` in `RealNumber`.
We have
```
f1 t === (p1 . g) t === p1 (g t) === p1 (RV2 u v) === u
```
and
```
f2 t === (p2 . g) t === p2 (g t) === p2 (RV2 u v) === v
```
so `f1 t === u` and `f2 t === v`.
Then
```
f t === RV2 (f1 t) (f2 t) === RV2 u v === g t
```
so `g === f`, which completes the proof.
{% endblock %}

So, Haskell admits pairwise products of types.
In fact, Haskell can express the product of any number of types (up to machine limitations).
If `A1`, `A2`, ..., `AN` are types, we define their product as so.

{% highlight haskell %}
data P = PCons A1 A2 ... AN

p1 :: P -> A1
p1 (PCons x _ ... _) = x

p2 :: P -> A1
p2 (PCons _ x ... _) = x

...

pN :: P -> AN
pN (PCons _ _ ... x) = x
{% endhighlight %}

The names `P`, `PCons`, `p1`, `p2`, ..., `pN` are arbitrary.
The `...` is pseudo-code for however many types you have/underscores you need.

Defining the functions `p1`, `p2`, ..., `pN` can be rather tedious.
Haskell gives us a shorthand way to define both the product type and the projection maps at once, using _named fields._

{% highlight haskell %}
data P = PCons {p1 :: A1, p2 :: A2, ..., pN :: AN}
{% endhighlight %}

This has all the effects of the first code block.
It creates the function `p1`, `p2`, ..., `pN` and the function `PCons :: A1 -> A2 -> ... -> AN -> P`.
It also has the additional effect that we may choose to supply arguments to `PCons` either positionally or by name, to our preference.
Here's what I mean.

{% highlight haskell %}
x :: P
x = PCons x1 x2 ... xN -- `PCons` with arguments supplied positionally.

y :: P
y = PCons {p1 = y1, p2 = y2, ..., pN = yN} -- `PCons` with arguments supplied by name.
{% endhighlight %}

When supplying arguments by name, we don't need to remember the order in which the various fields were originally declared.
Any permutation of the fields is valid.
The below example is perfectly valid.

{% highlight haskell %}
data Person = Person {name :: String, age :: Integer}

daniel :: Person
daniel = Person {age = 21, name = "Daniel"}
{% endhighlight %}

I slipped in a new concept there that Haskellers like to call "name punning."
I used "Person" as the name of the type _and_ as the name of the data constructor.
This is legal and valid haskell, and it's pretty common, especially when a data type has only one data constructor.

### Sum types and universal inclusion maps

`Bool` and `Ordering` are each defined by explicitly listing their members.
Most languages support this, and it's typically called _enumerated types,_ or _enums._
In Haskell, a types that have more than one data constructor are called _sum types._

One of Haskell's innovative features is that it allows you to freely combine both product type and sum types.

{% highlight haskell %}
data Payment
  = Cash {amount :: Dollars}
  | Card {amount :: Dollars, cardNo :: CardNumber, zip :: ZipCode}
  | Complimentary {amount :: Dollars} -- still need to keep track of how much we're giving away!
{% endhighlight %}

Here we have a data type with three data constructors.
The novel feature of Haskell is that it allows a programmer to give different fields to each constructor in an enumeration.
(I know, it's crazy that this is a _novel feature_ in programming languages!)

Notice that the constructors `Cash` and `Complimentary` have the same fields.
Because of their tags, they will create different copies of `Dollar` in `Payment`.
Thus, we have defined a tagged union.

$$
\begin{align*}
  \mathrm{Payment} &= \{ (\mathrm{Cash}, x) | x \in \mathrm{Dollars} \} \\
    &\cup \{ (\mathrm{Card}, x, y, z) | x \in \mathrm{Dollars}, y \in \mathrm{CardNumber}, z \in \mathrm{ZipCode} \} \\
    &\cup \{ (\mathrm{Complimentary}, x) | x \in \mathrm{Dollars} \}
\end{align*}
$$

Tagged unions are coproducts of sets.
Similarly, sum types in Haskell are coproducts of types.

{% block proposition Claim 2 %}
`Payment` is the coproduct of `Dollars`, `(Dollars, CardNumber, ZipCode)` and `Dollars`, where the inclusion maps are defined as `i1 x = Cash x`, `i2 (x, y, z) = Card x y z`, and `i3 x = Complimentary x`.
{% endblock %}

{% block proof Proof %}
Let `T` be a type, and suppose we have `f1 :: Dollars -> T`, `f2 :: (Dollars, CardNumber, ZipCode) -> T`, and `f3 :: Dollars -> T`.
Define `f :: Payment -> T` as follows.
```
f (Cash x) = f1 x
f (Card x y z) = f2 (x, y, z)
f (Complimentary x) = f3 x
```

We need to show that `f1`, `f2` and `f3` factor through `f`.
For `f1`, take any `x` in `Dollars`.
Then
```
(f . i1) x === f (i1 x) === f (Cash x) === f1 x
```
so `f1 === f . i1`.

For `f2`, take any `(x, y, z)` in `(Dollars, CardNumber, ZipCode)`.
```
(f . i2) (x, y, z) === f (i2 (x, y, z)) === f (Card x y z) === f2 (x, y, z)
```
so `f2 === f . i2`.

For `f3`, take any `x` in `Dollars`.
```
(f . i3) x === f (i3 x) === f (Complimentary x) === f3 x
```
so `f3 === f . i3`.
Thus, `f1`, `f2`, and `f3` factor through `f`, as required.

Next, we need to show that `f` is the only function through which `f1`, `f2`, and `f3` factor in this way.
Suppose that `g :: Payment -> T` is any function satisfying `f1 === g . i1`, `f2 === g . i2`, and `f3 === g . i3`.
Take any `p` in `Payment`.
We have three cases to consider.

If `p === Cash x` for some `x` in `Dollars`, then
```
f (Cash x) === f1 x === (g . i1) x === g (i1 x) === g (Cash x)
```
so `g p === f p` in case `p === Cash x`.

If `p === Credit x y z` for some `x` in `Dollars`, `y` in `CardNumber`, and `z` in `ZipCode`, then
```
f (Credit x y z) === f2 (x, y, z) === (g . i2) (x, y, z) === g (i2 (x, y, z)) === g (Credit x y z)
```
so `g p === f p` in case `p === Credit x y z`.

If `p === Complimentary x` for some `x` in `Dollars`, then
```
f (Complimentary x) === f3 x === (g . i3) x === g (i3 x) === g (Complimentary x)
```
so `g p === f p` in case `p === Complimentary x`.

In all cases, `g p === f p`, so `g === f`, completing the proof.
{% endblock %}

A programming language supports definition of product type and coproduct types, it is said to support _algebraic data types._

## Closing

In this post, we say how arbitrary products and coproducts of Haskell types can be expressed as Haskell types.
Roughly, we have a category similar to $$\mathrm{SET}$$, where Haskell types are the objects and Haskell functions are the maps.
That's more-or-less the case, although there are a few caveats that we might have time for at the end.
In the mean time, it's useful to limit our discussion to the fragment of Haskell were it is true.
This observation, though, is far from the total extant to which Category theory bares its teeth in Haskell.

As a parting note, notice that in the proofs of claims 1 and 2, we defined the induced map in a very ad hoc, manual way.
In the next post, we'll see how to write a Haskell program that generates these induced maps, naturally.
Moreover, slipping the word _naturally_ in there was intentional.
We will rely on a concept called _type variables._
With type variables, we will be able to turn the machinery of Haskell in on itself.
Type variables will allow us to embed---and leverage, to great utility---functors and natural transformation within Haskell programs.
