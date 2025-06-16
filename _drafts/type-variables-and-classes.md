---
layout: post
title: "Math-Haskell Rosetta Stone - Part 2"
date: 2024-09-25
permalink: /blog/math-haskell-rosetta-stone-2/
comments: true
tags:
  - haskell
  - math
  - set theory
  - category theory
---

<!--break-->

- [Part 1 - Algebraic data types](/blog/math-haskell-rosetta-stone/)
  - [Basic types and functions](/blog/math-haskell-rosetta-stone/#basic-types-and-functions)
  - [Simple sum types and branching](/blog/math-haskell-rosetta-stone/#simple-sum-types-and-branching)
  - [Algebraic data types](/blog/math-haskell-rosetta-stone/#algebraic-data-types)
  - [Closing](/blog/math-haskell-rosetta-stone/#closing)
- [Part 2 (this post)](#)
<!-- Generated TOC below this line -->
  - [Type variables](#type-variables)
    - [Generalizing over functions](#generalizing-over-functions)
    - [Generalizing over types](#generalizing-over-types)
    - [Parametrized types](#parametrized-types)
    - [Type constructors](#type-constructors)
  - [Type classes](#type-classes)
  - [Constructor classes](#constructor-classes)
<!-- Generated TOC above this line -->
- Part 3 (pending)
  - Kinds
  - Type-level programming


## Type variables

In the last post, we saw how to encode products and coproducts of types in Haskell.
We also say how to define the induced maps associated with their universal products.

Suppose `A` and `B` are types.
We define their product and coproduct like so.

{% highlight haskell %}
data Prod_A_B = Prod_A_B {pi1_A_B :: A, pi2_A_B :: B}
data Coprod_A_B = Iota1_A_B A | Iota2_A_B B
{% endhighlight %}

The fields `pi1_A_B` and `pi2_A_B` each generate a function.
Those functions serve as the universal projection maps for `Prod_A_B`.
Analogously, the data constructors `Iota1_A_B` and `Iota2_A_B` serve as the universal inclusion maps for `Coprod_A_B`.

{% highlight haskell %}
pi1_A_B :: Prod_A_B -> A
pi2_A_B :: Prod_A_B -> B

Iota1_A_B :: A -> Coprod_A_B
Iota2_A_B :: B -> Coprod_A_B
{% endhighlight %}

Given appropriate functions, we can defined the induced maps.
Specifically, given a type `W` and functions `f1 :: W -> A` and `f2 :: W -> B`, we can define a suitable induced map `f`.
```
f w = Prod_A_B (f1 w) (f2 w)
```
Similarly, with functions `g1 :: A -> W` and `g2 :: B -> W`, we can define a suitable induced map `g`.
```
g (Iota1_A_B a) = g1 a
g (Iota2_A_B b) = g2 b
```

This state of affairs is somewhat less than ideal.
First, for every pair of types, it seems it's up to us to define a bespoke type for their product or coproduct.
Second, for every pair of functions, it's up to use to define a bespoke function to serve as the induced map.
And, we have to be careful to carry this procedure out correctly each time.
Shouldn't the induced maps be, you know, _induced?_


### Generalizing over functions

Let's first tackle that second problem.
Supposing that each of `A`, `B`, ad `W` is some type, it's not difficult to write a function that yields an appropriate induced map when supplied two appropriate functions.

{% highlight haskell %}
inducedProductMap_A_B_W :: (W -> A) -> (W -> B) -> W -> Prod_A_B
inducedProductMap_A_B_W f1 f2 w = Prod_A_B (f1 w) (f2 w)

inducedCoproductMap_A_B_W :: (A -> W) -> (B -> W) -> Coprod_A_B -> W
inducedCoproductMap_A_B_W g1 g2 ab = case ab of
  Iota1_A_B a -> g1 a
  Iota2_A_B b -> g2 b
{% endhighlight %}

This gives us a degree of generality above our previous efforts.
The prior state of affairs had us defining a bespoke induced map for _every_ pair of functions.
Now, we merely need to feed those two function into the appropriate one of these functions, and it produces the induced map for us.
The variable `f1` generalizes over all functions of type `W -> A`, so that we're no longer considering one specific function.
Similar remarks apply for the variables `f2`, `g1`, and `g2`.

This is still not a satisfying level of generality, though, because the type `W` is hard-coded into these definitions.
We couldn't use `inducedProductMap_A_B_W` on a pair of functions `h :: Integer -> A`, `k :: Integer -> B`, for example.
If you've ever heard a pythonista complain about static types making programming unnecessarily cumbersome, now you know why.
What we need is a way to generalize over types.
We need type variables.


### Generalizing over types

Haskell gives us a way to write functions whose domain and codomain are expressions involving one or more type variables.
This claim is slightly incorrect, but we'll make it rigorous in a moment.

{% highlight haskell %}
inducedProductMap_A_B :: forall w. (w -> A) -> (w -> B) -> w -> Prod_A_B
inducedProductMap_A_B f1 f2 w = Prod_A_B (f1 w) (f2 w)

inducedCoproductMap_A_B :: forall w. (A -> w) -> (B -> w) -> Coprod_A_B -> w
inducedCoproductMap_A_B g1 g2 ab = case ab of
  Iota1_A_B a -> g1 a
  Iota2_A_B b -> g2 b
{% endhighlight %}

First thing you should notice is that the formulas defining `inducedProductMap_A_B` and `inducedCoproductMap_A_B` are _exactly the same_ as the formulas defining `inducedProductMap_A_B_W` and `inducedCoproductMap_A_B_W`.
The only thing that changed for each of them was their signature (_ie._ the function's type declaration, above its formula).

Second thing to notice is a bit more subtle.
This is the part where I make this section's opening remark rigorous.
I want to convey my point with an analogy.

Consider the function defined below.

$$
f : \mathbb{R} \to \mathbb{R} \\
f(x) = x^2 - 4
$$

In $$x^2 - 4$$, what is $$x$$?

If you said, "$$x$$ is a real number," that's great!
It's only slightly wrong.

See, if you say that $$x$$ is a real number, then what real number is $$x$$, exactly?
$$x$$ doesn't look like any real number I've ever seen.
So far as I've seen, real numbers look like $$5$$.
Or they look like $$e^\sqrt{2}$$.
Or they look like $$\mathrm{Sup} \{r \in \mathbb{Q} | r^3 < 5 \}$$, if you want to get technical.
You know what they _don't_ look like?
They don't look like $$x$$.
In fairness, rational numbers don't look like $$r$$, but at least we can agree that the exact arrangement of symbols $$\mathrm{Sup} \{r \in \mathbb{Q} | r^3 < 5 \}$$ does indeed specify a real number.
The symbol $$x$$ does not denote a real number.
So what, the hell, _is_ $$x$$?

$$x$$ is an abstraction.
$$x$$ is abstract syntax that shows us what to do with a real number, if we had one.
$$x$$ is a concession that allows us to unambiguously specify which function we mean when we say $$f$$.
In reality, though, we don't have a number.

This is subtle, but important.
In the signature `(w -> A) -> (w -> B) -> w -> Prod_A_B`, the symbol `w` does not refer to a type.
Consequently, `(w -> A) -> (w -> B) -> w -> Prod_A_B` is not a type.
Consequently, `inducedProductMap_A_B` is not a function.
(Disagree? Then what's its domain? What's its codomain?)
`inducedProductMap_A_B` is a _family_ of functions: one function for every possible type that we may put in place of `w`.
`inducedProductMap_A_B` is a family of functions, indexed by the objects in our category.
Put a pin in that.

{% aside %}
It's worth mentioning now that the names of Haskell types are required to start with upper-case letters.
This allows us to easily identify which names in a type signature refer to specific types and which names refer to type variable: a type variables always begins with a lower-case letter.

Furthermore, since there's never a chance of ambiguity, declaring type variables using the `forall` keyword is optional, except in certain specific circumstances.
In this series, I'll always declare type variables with `forall`, but readers should know how to identify type variables without an explicit declaration.
{% endaside %}

### Parametrized types

We'd like to generalize away `A` and `B` with type variables in `inducedProductMap_A_B` and `inducedCoproductMap_A_B`, like we generalized away `W`.
The bespoke nature of `Prod_A_B` and `Coprod_A_B` prevents us: they are both defined in terms of the (hypothetical) specific types `A` and `B`.
That's not a problem, though, because Haskell allows us to use type variables in type definitions.

{% aside %}
It's understandable if, at this point, you're left wondering what the heck I'm going on about.
Why am I making such a big deal out of replacing things with variables?
This is, perhaps, the biggest distinction between doing math and doing programming.
When doing math, you can take pretty much any fragment of "math stuff" and replace it with a variable.
All you have to do is explain that to your reader.

You can't always do that in programming.
You can't necessarily abstract arbitrary fragments of a program.
Every programming language has syntactic forms that have no way to be abstracted within the language.
You can always _talk_ about abstracting-away some snippet of syntax with a variable, but that abstraction will be limited to the discussion happening around the chalkboard.
{% endaside %}

Here are products and coproducts in their most-general form.

{% highlight haskell %}
data Prod a b = Prod {pi1 :: a, pi2 :: b}
data Coprod a b = Iota1 a | Iota2 b
{% endhighlight %}

The lower-case `a` and `b` are type variables.
We introduce them by writing them on the left-hand side, and we reference them on the right-hand side.
We can now define the induced maps in full generality.

{% highlight haskell %}
inducedProductMap :: forall a b w. (w -> a) -> (w -> b) -> w -> Prod a b
inducedProductMap f1 f2 w = Prod (f1 w) (f2 w)

inducedCoproductMap :: forall a b w. (a -> w) -> (b -> w) -> Coprod a b -> w
inducedCoproductMap g1 g2 ab = case ab of
  Iota1 a -> g1 a
  Iota2 b -> g2 b
{% endhighlight %}

Now we can dispense with all the bespoke types and functions, such as `Prod_A_B` and `inducedProductMap_A_B`.
These (families of) functions can be used in any place where we otherwise would have needed a bespoke version, disarming all lingering complaints the pythonistas might levy.

Notice that the formulas remain exactly the same as before.
Only the signatures have changed.
This confirms that these are genuine generalizations of the bespoke versions.


### Type constructors

It's important to understand that neither `Prod` nor `Prod a b` are types.
`Prod Integer Double` is a type.
`Prod Bool String` is a type.
`Prod Person Dog` is a type.
But `Prod a b` is not a type, simply because `a` and `b` are not types.
They're type variables, abstractions.
So, `Prod a b` is abstract: it has no members.

On the other hand, `Prod` is not abstract.
`Prod` is very concrete, in the same sense that $$\mathrm{sin}$$ is concrete even thought $$\mathrm{sin}(x)$$ is abstract.
That's because `Prod` refers to something specific and tangible.
`Prod` is a function on types.
`Prod` expects to receive two types (for example, `Bool` and `Dog`), and it yields a type (in the same example, `Prod Bool Dog`).
`Prod` is a type-level function, the first example we've seen.

Similar remarks apply to `Coprod`.

Recall that `inducedProductMap` is not a function, but rather a family of functions, parametried by type variables `a`, `b`, and `w`.

{% highlight haskell %}
data Prod a b = Prod {pi1 :: A, pi2 :: B}
inducedProductMap :: forall a b w. (w -> a) -> (w -> b) -> w -> Prod a b
inducedProductMap f1 f2 w = Prod (f1 w) (f2 w)
{% endhighlight %}

Imagine that `w` and `a` are fixed, while we allow `b` to vary.


## Type classes


## Constructor classes
