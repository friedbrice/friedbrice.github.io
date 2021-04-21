---
layout: post
title: "Of function instances and abstract syntax"
date: 2021-04-20
permalink: /blog/of-function-instances-and-abstract-syntax/
redirect-from:
  - /blog/2021-04-20/
comments: true
tags:
  - haskell
  - commutative ring theory
  - polynomials
  - universal algebra
  - tagless final
---

Some Haskell classes `class Myclass a` admit an instance for functions
`instance Myclass a => Myclass (x -> a)` based on the instance for `a`.
All of these instances have a few things in common: (1) they implement
the class methods in a straightforward way as `mymethod f = \x -> mymethod (f x)`,
and (2) they are polarizing among Haskell practitioners. The sequel is a
case study of why I find such instances compelling and useful.

<!-- break -->

{% highlight haskell %}
{-# LANGUAGE NoImplicitPrelude,
             RankNTypes,
             RebindableSyntax,
             ViewPatterns #-}

module Polynomial where

import Data.Void (Void)
import Prelude (Either, Integer, String, foldl, (==), (<>))
import qualified Prelude
{% endhighlight %}

The dictionary of functions that comprise a `Num` instance is roughly
(if you omit `abs` and `signum`) the signature of what math folk call a
_commutative ring_.

{% highlight haskell %}
class Ring a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  fromInteger :: Integer -> a
{% endhighlight %}

A commutative ring \\(R\\) is a set (or type) on which associative,
commutative operations \\(+\\) and \\(\cdot\\) are defined, a unary operation \\(-\\) is
defined that takes a ring element to its \\(+\\) inverse, \\(\cdot\\) distributes
over \\(+\\), there is an element called \\(0\\) that is an identity for \\(+\\),
there is an element called \\(1\\) that is an identity for \\(\cdot\\), and \\(0\\) and
\\(1\\) are distinct. This gives us our `+`, `*`, and `negate` functions in
Haskell's `Num`.

For any commutative ring \\(R\\), it is possible to define one (and only
one) function \\(i\\) from the integers to \\(R\\) with the following special
properties:

1.  \\(i\\) distributes over the usual \\(+\\) and \\(\cdot\\) of the integers, yielding
    the \\(+\\) and \\(\cdot\\) of \\(R\\) when it does, i.e.

    $$
    \begin{align*}
    i(x + y) &= i(x) + i(y) \\
    i(x \cdot y) &= i(x) \cdot i(y)
    \end{align*}
    $$

    where \\(+\\) and \\(\cdot\\) on the left hand side refer to the usual
    operations defined for integers, but \\(+\\) and \\(\cdot\\) on the right hand
    side refer to the ring operations defined for \\(R\\).

2.  \\(i(0)\\) is the \\(0\\) of \\(R\\), and \\(i(1)\\) is the \\(1\\) of \\(R\\).

Facts 1 and 2 tell us that every commutative ring (abbreviated to just
*ring* for the remainder of this discussion) has an image of the
integers inside of it. This function \\(i\\) is analogous to our
`fromInteger` function in Haskell's `Num`. At the same time, implementing
`fromInteger` for a `Num` instances implicitly defines the \\(0\\) and \\(1\\)
of our ring vis a vis `fromInteger 0` and `fromInteger 1`.

{% highlight haskell %}
instance Ring Integer where
  (+) = (Prelude.+)
  (*) = (Prelude.*)
  negate = Prelude.negate
  fromInteger = Prelude.fromInteger
{% endhighlight %}

Obviously \\(\mathbb{Z}\\), the set of all integers, is a ring. In a sense, \\(\mathbb{Z}\\) is the
most prototypical ring. But there are many other rings, some exotic. For
example, Take any set \\(X\\), and form the set \\(\mathop{\mathrm{Pow}}(X)\\) consisting of all
subsets of \\(X\\). Recall \\(\cap\\) is the usual intersection of sets, \\(\cup\\) is the
usual union of sets, and \\(\setminus\\) is the usual difference of sets (i.e.,
\\(E_1 \setminus E_2 = \left\\{ x \mid x \in E_1 \text{ and } x \notin E_2 \right\\}\\)). Define \\(\mathop{\Delta}\\) by
\\(E_1 \mathop{\Delta} E_2 = (E_1 \cup E_2) \setminus (E_1 \cap E_2)\\). Then, perhaps surprisingly, \\(\mathop{\mathrm{Pow}}(X)\\)
is a ring using \\(\mathop{\Delta}\\) as \\(+\\), \\(\cap\\) as \\(\ast\\), and the identity function as \\(-\\)
(i.e. every set is its own \\(\Delta\\) inverse).

If \\(R_1\\) and \\(R_2\\) are rings, then \\(R_1 \times R_2\\) (the set of ordered pairs) is
a ring using \\((x_1, x_2) + (y_1, y_2) = (x_1 + y_1, x_2 + y_2)\\) and
\\((x_1, x_2) \cdot (y_1, y_2) = (x_1 \cdot y_1, x_2 \cdot y_2)\\). Towards our main goal, if
\\(R\\) is a ring and if \\(X\\) is any arbitrary set, then the set of all
functions from \\(X\\) to \\(R\\) is a ring using \\(f + g = x \mapsto f(x) + g(x)\\) and
\\(f \cdot g = x \mapsto f(x) \cdot g(x)\\).

{% highlight haskell %}
instance (Ring a, Ring b) => Ring (a, b) where
  (x1, x2) + (y1, y2) = (x1 + y1, x2 + y2)
  (x1, x2) * (y1, y2) = (x1 * y1, x2 * y2)
  negate (x1, x2) = (negate x1, negate x2)
  fromInteger n = (fromInteger n, fromInteger n)
{% endhighlight %}

{% highlight haskell %}
instance Ring a => Ring (x -> a) where
  f + g = \x -> f x + g x
  f * g = \x -> f x * g x
  negate f = \x -> negate (f x)
  fromInteger n = \_ -> fromInteger n
{% endhighlight %}

Finally, the polynomials: if \\(X\\) is any set, define the _polynomials in
\\(X\\)_, which we'll write as \\(P(X)\\), as the set of all formal expressions of the form

$$
n_1 x_{1,1} x_{1,2} ... x_{1,k_1}
  + n_2 x_{2,1} x_ {2,2} ... x_ {2,k_ 2}
  + ...
  + n_m x_{m,1} x_{m,2} ... x_{m,k_m}
$$

where the \\(n_i\\) are integers, the \\(k_i\\) and \\(m\\) are non-negative integers,
and the \\(x_{i,j}\\) are elements of \\(X\\).

<aside>
<strong>Note:</strong> when math folk say <em>formal expression</em> they mean <em>abstract
syntax</em>.
</aside>

Put more constructively:

1.  each individual integer is a polynomial in \\(X\\),

2.  a string of element of \\(X\\) is a polynomial in \\(X\\),

3.  if \\(p_1\\) is a polynomial in \\(X\\) of Form 1 and \\(p_2\\) is a polynomial in \\(X\\)
    of Form 2, then the juxtaposition \\(p_1 p_2\\) is a polynomial in \\(X\\), and

4.  if \\(p_1\\) and \\(p_2\\) are a polynomials in \\(X\\), then the formal expression \\(p_1 + p_2\\) is a polynomial in
    \\(X\\).

We could directly encode this as a Haskell datatype as follows.

{% highlight plain %}
data PExpr x
  = Form1 Integer
  | Form2 [x]
  | Form3 Integer [x]
  | Form4 (PExpr x) (PExpr x)
{% endhighlight %}

Doing so, we notice that `Form3` subsumes `Form1` and `Form2`, so we
will work with a simplified definition.

{% highlight haskell %}
data PExpr x
  = Form3 Integer [x]
  | Form4 (PExpr x) (PExpr x)
{% endhighlight %}

Traditionally elements of \\(X\\) are called the _variables_, _generators_,
or _indeterminates_ of \\(P(X)\\), so you will often see \\(P(X)\\) referred to
as _the polynomials generated by \\(X\\)_ or as _the polynomials with
indeterminates in \\(X\\)_. Elements of \\(X\\) may be lifted into \\(P(X)\\) (i.e.,
thought of as elements of \\(P(X)\\)) in light of Form 2.

{% highlight haskell %}
liftVar :: x -> PExpr x
liftVar x = Form3 1 [x]
{% endhighlight %}

\\(P(X)\\) forms a ring, independent of (e.g. ignoring) any existing ring
structure on \\(X\\) itself.

{% highlight haskell %}
instance Ring (PExpr x) where
  p1 + p2 = Form4 p1 p2

  Form4 p1 p2 * p3 = Form4 (p1 * p3) (p2 * p3)
  p1 * Form4 p2 p3 = Form4 (p1 * p2) (p1 * p3)
  Form3 n1 xs1 * Form3 n2 xs2 = Form3 (n1 * n2) (xs1 <> xs2)

  negate (Form3 n ss) = Form3 (negate n) ss
  negate (Form4 p1 p2) = Form4 (negate p1) (negate p2)

  fromInteger n = Form3 n []
{% endhighlight %}

Reasonable sets to use for \\(X\\) include any singleton set, from which
arise polynomials in one variable, any pair set, from which arise
polynomials in two variables, etc. You may derive the set of polynomials
in infinitely-many variables by using \\(\mathbb{Z}\\) for \\(X\\),
but you must take care when you do: polynomials of Form 1 inherit the
ring structure of the integers, but polynomials of Form 2 are merely
abstract symbols with no ring structure other than that of \\(P(\mathbb{Z})\\)
defined above. Thus, \\(P(\mathbb{Z})\\) contains two distinct copies of \\(\mathbb{Z}\\) with
different algebraic properties, and it's up to you to always be
cognizant whether an integer was lifted into \\(P(\mathbb{Z})\\) using Form 1 or
using Form 2. Because of this possibility for confusion, it is often
more convenient to use the set of character strings as \\(X\\) to give rise
to the ring of polynomials in infinitely-many variables, and you get the
added benefit of being able to name variables. Using the empty set for
\\(X\\) gives a ring that is indistinguishable (from the point of view of
ring theory) from \\(\mathbb{Z}\\).

{% highlight haskell %}
type P_1 = PExpr () -- polynomials in one variable
type P_2 = PExpr (Either () ()) -- polynomials in two variables
type P_inf = PExpr Integer -- polynomials in infinitely-many unfortunately-named variables
type P_named = PExpr String -- polynomials in infinitely-many nicely-named variables
type Z = PExpr Void -- polynomials, but with no variables (a.k.a, the integers)
{% endhighlight %}

In computery terms, the polynomials in \\(X\\) are the "ring expressions",
with variables denoted by elements of \\(X\\). They are the abstract syntax
trees of (commutative) ring theory. That is, the polynomials are the
expressions that "make sense" and can be interpreted in any ring \\(A\\),
once you throw in a dictionary that assigns an element of \\(A\\) to each
element of \\(X\\). This framing suggests a deep fact that we'll discuss next.

Given a set \\(X\\) and a ring \\(A\\), we may define the _evaluation map of \\(X\\)
to \\(A\\)_ that takes a dictionary from \\(X\\) to \\(A\\) and a polynomial in \\(X\\)
and determines a value of \\(A\\) by first substituting values of \\(A\\) for elements
of \\(X\\) as they appear in the polynomial and then reducing the polynomial
according to the ring operations of \\(A\\) (treating juxtaposition as \\(\cdot\\)
and treating the formal \\(+\\) exactly as the notation suggests).

{% highlight haskell %}
evalPExpr :: Ring a => (x -> a) -> PExpr x -> a
evalPExpr dict (Form3 n xs) = foldl (\a x -> a * dict x) (fromInteger n) xs
evalPExpr dict (Form4 p1 p2) = evalPExpr dict p1 + evalPExpr dict p2
{% endhighlight %}

Now, remember that at its core, a function from \\(X\\) to \\(A\\) is simply a
selection of \\(X\\)-many elements of \\(A\\). For example, defining a function
from a one-element set to the set of integers is equivalent to merely
picking an integer. Defining a function from a two-element set to the
set of character strings is equivalent to selecting two particular character
strings (in a particular order). In this sense, we can think of a function \\(X\\) to \\(A\\)
as a substitution assignment of values in \\(A\\) to the indeterminates of
\\(P(X)\\). Thus, an element of \\(P(X)\\) is interpreted as a **function**
on \\(A\\) with \\(X\\)-many
variables (recovering fully the classical notion of a polynomial as
being one of those familiar functions from high-school Math). Thus, the
polynomials are the functions that can be defined wholly in terms of the
rings operations, and thus may be applied in any ring. This fact,
encoded above as `evalPExpr`, is the essential property of polynomials
from which all of their other properties derive.

It took us quite a lot of work to get here, between the definition of
the datatype `PExpr` and its `Ring` instance, not to mention the
definition of `evalPExpr`. It would be nice if there were an easier way
to reach the light at the end of the tunnel. Notice what happens when we
rearrange the arguments and constraint on `evalPExpr`, though.

{% highlight plain %}
evalPExpr :: PExpr x -> (forall a. Ring a => (x -> a) -> a)
{% endhighlight %}

My hope is that this reminds you of a comment I made above. To
paraphrase and summarize the above discussion:

_The polynomials with indeterminates in \\(X\\) are exactly the functions of
\\(X\\)-many variables that make sense in any ring._

Now look at the signature. The `(x -> a) -> a` is a function on `a`
with `x`-many variables, where the `x -> a` argument is thought of as a
dictionary selecting `x`-many elements of `a` to substitute into the
function. The `forall a. Ring a =>` indicates that this function makes
sense in any `Ring`. The type `forall a. Ring a => (x -> a) -> a)` is in
fact the ring of polynomials generated by `x` in disguise.

{% highlight haskell %}
-- | The polynomials in @x@.
newtype Polynomial x = P (forall a. Ring a => (x -> a) -> a)
{% endhighlight %}

Lifting \\(X\\) values into \\(P(X)\\) is merely function application.

{% highlight haskell %}
var :: x -> Polynomial x
var x = P (\dict -> dict x)
{% endhighlight %}

The evaluation map, the map that promotes an arbitrary \\(X\\) to \\(A\\) function into a
\\(P(X)\\) to \\(A\\) function, is also just application.

{% highlight haskell %}
evalMap :: Ring a => (x -> a) -> Polynomial x -> a
evalMap dict (P f) = f dict
{% endhighlight %}

And the ring structure on `Polynomial x` (i.e., the `Ring` instance) is
the familiar `Ring` instance for functions.

{% highlight haskell %}
instance Ring (Polynomial x) where
  P f + P g = P (f + g)
  P f * P g = P (f * g)
  negate (P f) = P (negate f)
  fromInteger n = P (fromInteger n)
{% endhighlight %}

My claim is that the ring `Polynomial x` is indistinguishable (from the
point of view or ring theory) from the ring `PExpr x`. To convince you
of this, I need to define a function from `Polynomial x` to `PExpr x`
that (1) is invertible and that (2) distributes over the ring
operations. These two facts together would mean that any ring-theoretic
statement of fact you could make about `PExpr x` would have a
corresponding statement of fact about `Polynomial x`, and vice versa.

Here's that function.

{% highlight haskell %}
toExpr :: Polynomial x -> PExpr x
toExpr = evalMap liftVar
{% endhighlight %}

Doing the easy job first, we'll establish that `toExpr` distributes over
the ring operations.

{% highlight plain %}
toExpr (P f + P g)
  = toExpr (P (f + g)) -- def. of `+` for `Polynomial x`
  = (f + g) liftVar -- def. of `toExpr` and `evalMap`
  = f liftVar + g liftVar -- def. of `+` for functions
  = toExpr (P f) + toExpr (P g) -- def. of `evalMap` and `toExpr`

toExpr (P f * P g)
  = toExpr (P (f * g)) -- def. of `*` for `Polynomial x`
  = (f * g) liftVar -- def. of `toExpr` and `evalMap`
  = f liftVar * g liftVar -- def. of `*` for functions
  = toExpr (P f) * toExpr (P g) -- def. of `evalMap` and `toExpr`

toExpr (negate (P f))
  = toExpr (P (negate f)) -- def. of negate for `Polynomial x`
  = (negate f) liftVar -- def. of `toExpr` and `evalMap`
  = (\dict -> negate (f dict)) liftVar -- def. of `negate` for functions
  = negate (f liftVar) -- beta reduction
  = negate (toExpr (P f)) -- def. of `evalMap` and `toExpr`

toExpr (fromInteger n)
  = toExpr (P (fromInteger n)) -- def. of `fromInteger` for `Polynomial x`
  = (fromInteger n) liftVar -- def. of `toExpr` and `evalMap`
  = (\dict -> fromInteger n) liftVar -- def. of `fromInteger` for functions
  = fromInteger n -- beta reduction
{% endhighlight %}

Now, we need to see that `toExpr` is invertible. To do that, we'll
define a function going the other way, and then show that they cancel
each other.

{% highlight haskell %}
fromExpr :: PExpr x -> Polynomial x
fromExpr = evalPExpr var
{% endhighlight %}

Using the definitions of the `Ring` operations defined in the instance
for `PExpr x`, we'll see that `toExpr . fromExpr = id`.

{% highlight plain %}
toExpr (fromExpr (Form3 n [x_1, ..., x_k]))
  = toExpr (evalPExpr var (Form3 n [x_1, ..., x_k]))
  = toExpr (foldl (\a x -> a * var x) (fromInteger n) [x_1, ..., x_k])
  = toExpr (fromInteger n * var x_1 * ... * var x_k
  = toExpr (
      P (\dict -> fromInteger n)
    * P (\dict -> dict x_1)
    * ...
    * P (\dict -> dict x_k))
  = toExpr (P (\dict -> fromInteger n * dict x_1 * ... * dict x_k))
  = (\dict -> fromInteger n * dict x_1 * ... * dict x_k) liftVar
  = fromInteger n * liftVar x_1 * ... * liftVar x_k
  = Form3 n [] * Form3 1 [x_1] * ... * Form3 1 [x_k]
  = Form3 n [x_1, ..., x_k]
{% endhighlight %}

So `toExpr . fromExpr` fixes `Form3` expressions. Likewise for `Form4`
expressions. For this, we use induction. Suppose that `expr1` and
`expr2` are in `PExpr x` and that `(toExpr . fromExpr) expr1 = expr1`
and `(toExpr . fromExpr) expr2 = expr2`.

{% highlight plain %}
toExpr (fromExpr (Form4 expr1 expr2))
  = toExpr (evalPExpr var (Form4 expr1 expr2))
  = toExpr (evalPExpr var expr1 + evalPExpr var expr2)
  = toExpr (fromExpr expr1 + fromExpr expr2)
  = toExpr (fromExpr expr1) + toExpr (fromExpr expr2)
  = expr1 + expr2
  = Form4 expr1 expr2
{% endhighlight %}

We've left to show that `fromExpr . toExpr = id`. This is highly
technical and only true if we assume termination. We would proceed using
induction on the set of all functions of the form
`Ring a => (x -> a) -> a`. We know that any (terminating) function of
that form can only apply the dictionary to inhabitants of `x` and use
the `Ring` methods, so we'd have base cases `\dict -> fromInteger n`
and `\dict -> dict x`, and we have
induction cases `\dict -> f dict + g dict`, `\dict -> f dict * g dict`,
and `\dict -> negate (f dict)`. We'd show the base cases all satisfy
`(fromExpr . toExpr) p = p`, and we'd show that each induction case
satisfies `(fromExpr . toExpr) p = p` when we assume the hypotheses
`(fromExpr . toExpr) f = f` and `(fromExpr . toExpr) g = g`. We'll spare
the details, but assuming this work is done, we will have accomplished
our goal of showing that `Polynomial x` and `PExpr x` are
indistinguishable from the point of view of ring theory.

{% highlight haskell %}
x :: Polynomial String
x = var "x"
{% endhighlight %}

{% highlight haskell %}
y :: Polynomial String
y = var "y"
{% endhighlight %}

{% highlight haskell %}
constant :: Integer -> Polynomial x
constant = fromInteger
{% endhighlight %}

{% highlight haskell %}
ellipticCurve :: Integer -> Integer -> Polynomial String
ellipticCurve (constant -> a) (constant -> b) = y^2 - x^3 - a*x - b
{% endhighlight %}

Now some philosophy. The `Ring` instance for the function type `X -> A`
defines the ring operations for functions in terms of the operations on
the type `A`. In a sense, `X -> A` inherits its ring structure (its
`Ring` instance) from the ring structure of `A`. An element of
`Polynomial x`, that is, a function `Ring a => (x -> a) -> a`, is
polymorphic in `a`. Any ring may be slotted in for `a`, and the
polynomial can be evaluated at that type. Yet `Ring a => (x -> a) -> a` is
a function type; it inherits its ring structure from the target ring
`a`, which--again--can be any ring. In a sense, `Polynomial x` inherits
its ring structure from *every* ring.

Recall that we earlier said that the ring of integers is the most-
prototypical ring. Every other ring contains a projection of the ring of
integers, witnessed by its `fromInteger` implementation. The polynomial
rings, inheriting their ring structure from every ring, are the
counterpoint to the integers' maximal prototypicality. By inheriting the
ring structure of every ring, the polynomial rings are the most general
rings.

There is a way to make this notion of maximal generality mathematically
precise. Direct your attention to the signature of `evalMap`.

{% highlight plain %}
evalMap :: Ring a => (x -> a) -> Polynomial x -> a
{% endhighlight %}

I'm very deliberate about the choice of name here. It should remind you
of a common Haskell function you're hopefully familiar with.

{% highlight plain %}
foldMap :: Monoid a => (x -> a) -> [x] -> a
{% endhighlight %}

Suspending legitimate concerns about termination and infinite lists,
Haskell's `[]` functor is commonly thought of as the free monoid
construction. What does that mean? It means that `[]` is a factory that
produces free monoids. Explicitly, it means that the type `[()]` is the
free monoid on one generator (a.k.a. "in one variable"), the type
`[Either () ()]` is the free monoid on two generators (a.k.a. "in two variables"). The type `[x]` is the free monoid generated by `x` (i.e., monoid expressions with indeterminates in `x`). Elements of the free monoid generated by `x` are abstract monoid-syntax trees, using the elements of `x` as variables. `foldMap` is the function that allows us to interpret the abstract syntax, promoting a dictionary of assignments `x
-> a` to a function`[x] -> a`. This explanation of what it means to be a free monoid should sound familiar to you, because it's analogous to the discussion we had earlier about polynomials, whereby our intuitive notion that the polynomials `Polynomial
x` are the `x`-variable functions that make sense in any ring is made precise. The polynomial ring `Polynomial
x` is the free (commutative) ring generated by `x`, in the same sense that the list type `[x]` is the free monoid generated by `x`.

Summary:

1.  Types that are instances of Haskell's `Num` class are analogous to
    (commutative) rings.

2.  The ring of integers is the most-prototypical ring. Every ring
    contains an image of the integers.

3.  If \\(A\\) is a ring and if \\(X\\) is any set, then the set of functions
    \\(X \to A\\) is a ring in a non-arbitrary way, in that the ring
    operations on functions are defined in terms of the ring operations
    on \\(A\\). \\(X \to A\\) inherits its ring structure from \\(A\\).

4.  A polynomial with indeterminates in \\(X\\) is an \\(X\\)-variable function
    that makes sense in any ring. In Haskell terms, that is precisely a
    polymorphic function with a `Ring` (or `Num`) constraint.
    Equivalently, we can think of polynomials as ring expressions,
    abstract syntax built from the ring operations.

5.  The polynomials in \\(X\\) is a ring in a non-arbitrary way, because the
    polynomials are functions into a ring (really, functions into any
    ring), and functions inherit the ring structure of their target
    ring.

6.  Counterpoint to the role the integers play as the most-prototypical
    ring, The polynomial rings are the most-general rings. The
    polynomials are functions into any target ring, so the ring of
    polynomials inherit the ring structure of every ring. Made precise,
    this is the statement that the polynomial rings are the free
    (commutative) rings.

{% highlight haskell %}
(-) :: Ring a => a -> a -> a
x - y = x + negate y
{% endhighlight %}

{% highlight haskell %}
(^) :: Ring a => a -> Integer -> a
x ^ 0 = 1
x ^ n = x * (x ^ (n - 1))
{% endhighlight %}

{% highlight haskell %}
infixr 8 ^
infixl 7 *
infixl 6 +
infixl 6 -
{% endhighlight %}
