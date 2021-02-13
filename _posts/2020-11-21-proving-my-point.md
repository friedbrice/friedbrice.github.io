---
layout: post
title: "Proving My Point"
date: 2020-11-21
permalink: /blog/proving-my-point/
redirect-from:
  - /blog/24/
  - /blog/2020-11-21/
comments: true
tags:
  - haskell
  - functional programming
  - theorem proving
---

A coworker of mine has been kind enough to spend his scant spare time guiding a small group of future Haskellers through Hutton's [Programming in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pih.html), assigning weekly problems and judging the solutions submitted along the way. What follows here today[^1] is my smart-ass solution to my colleague's _Problem 1_.

<!--break-->

[^1]: I know I owe you a retro on the [linear regression post](/blog/simple-linear-regression-in-one-pass/), including a clever observations made by a user on Twitter.com and a constant-space solution. (My solution uses memory proportional to the size of the input---yuck!) I haven't forgotten.


## Executing Programs on Paper

Week 1 of the reading group was to read Chapter 1 in _Hutton_ and complete a few simple problems based on the reading. Chapter 1 is mostly an inspirational overview, but it does show how one can write simple, recursive functions on lists and how one can evaluate such functions on paper. For example, Hutton defines `sum` as follows.

{% highlight haskell %}
sum [] = 0
sum (x : xs) = x + sum xs
{% endhighlight %}

He then shows how one may evaluate `sum [1, 2, 3]` on paper.

$$
\begin{align*}
\texttt{sum}\,[1, 2, 3] &= \texttt{sum}\,(1 : 2 : 3 : []) \\
&= 1 + \texttt{sum}\,(2 : 3 : []) \\
&= 1 + (2 + \texttt{sum}\,(3 : [])) \\
&= 1 + (2 + (3 + \texttt{sum}\,[])) \\
&= 1 + (2 + (3 + 0)) \\
&= 1 + (2 + 3) \\
&= 1 + 5 \\
&= 6
\end{align*}
$$

## Week 1 Problem 1

Using the above example as a guide, said group leader assigned what would be the first problem for week 1.

**Problem 1:** Prove that for all `x`, `sum [x]` is `x`.

This is a great problem, and I'm sure the intended solution was to evaluate the program on paper for a general value \\(x\\). By that, I mean we assume that \\(x\\) is some fixed value and we proceed to evaluate \\(\texttt{sum}\,[x]\\) on paper as though we knew what value \\(x\\) were.

$$
\begin{align*}
\texttt{sum}\,[x] &= \texttt{sum}\,(x : []) \\
&= x + \texttt{sum}\,[] \\
&= x + 0 \\
&= x
\end{align*}
$$

Since our answer did not depend on any specific characteristics of \\(x\\), the reasoning is valid no matter what value \\(x\\) were, and so we know that this same conclusion can be drawn about any such value. That is, we have shown that for all `x`, `sum [x]` is `x`. (Notice that the above evaluation could not have been carried out to conclusion had the formula for `sum` directed us to pattern match \\(x\\). Also, we must assume a lawful `Num` instance, as we tacitly use \\(x + 0 = x\\).)

## Proofs that Compile

The above is certainly a fine proof, and---I stress again---surely the intended solution to Problem 1.

But I was bored. And I wanted to be extra, super-duper sure of the veracity of the claim.

To this very end, I wrote a little program in Haskell. And as we know about Haskell, _if it compiles, it works._

{% highlight haskell %}
module Chapter1 where

import Prelude hiding (sum)

data Nil
data Cons x xs
{% endhighlight %}

We're going to prove Problem 1, but for type-level lists, using classes as predicates on types. Our type-level list is implemented using `Cons` "cells" terminating with a `Nil`. For example, the type level list consisting of `Int`, `Double`, and `String` (in that order) is quite simply `Cons Int (Cons Double (Cons String Nill))`. This type has no inhabitants, but that's fine---we won't be needing any.

{% highlight haskell %}
{-# LANGUAGE FunctionalDependencies #-}

...

import Data.Proxy (Proxy(..))

...

class Sum list x | list -> x where
  sum :: Proxy list -> Proxy x
  sum _ = Proxy
{% endhighlight %}

We've added a class to represent the predicate _The type `x` is the sum of the type `list`._ That is, `x` is the sum of `list` if and only if the compiler can find an instance `Sum list x`. The syntax `| list -> x` means that attempting to create two instances `Sum list x1` and `Sum list x2` is a compiler error. We want this, because the sum of `list` (if a sum exists) should be uniquely and unambiguously determined by `list`. The `FunctionalDependencies` language extension enables this syntax and the associated check. The extension also aids type inference, since---in light of the restriction imposed by `| list -> x`---the type `x` can now be inferred from the type `list`.

The class method `sum` is notable for both its signature and its implementation. Its signature `Proxy list -> Proxy x` tells us that `sum` gives us a value-level way to find the type `x` that is the sum of the type `list`.

To see how, we need to talk about `Proxy`. `Proxy` is defined as `data Proxy a = Proxy`. So, `Proxy Int` has the one inhabitant `Proxy :: Proxy Int`. And `Proxy Bool` has the one inhabitant `Proxy :: Proxy Bool`. And `Proxy [IO String]` has the one inhabitant `Proxy :: Proxy [IO String]`. In fact, no matter what type is used for the parameter, we have just one inhabitant, `Proxy`, that carries no data. What gives?

Well, the interesting thing about `Proxy` isn't the value, it's the type parameter. In code, the data constructor `Proxy` is often given an explicit type annotation as a way to _pass types as arguments_ into Haskell functions. This is exactly how we're going to use it with our `sum` function.

If we have some type `list` in mind and we want to know its sum, all we have to do is  ask the compiler to inferred type of `sum (Proxy :: Proxy list)`. The compiler will infer a type `Proxy x` if it can find an instance `Sum list x`, from which we see that type `x` is the sum of type `list`, or else it will reject the program if it fails to find an instance. This is how we get the compiler to perform type-level computations for us (such as summing lists) at compile time.

The implementation `sum _ = Proxy` works because the compiler doesn't need to inspect the argument (it has no data anyway) as it already knows the type of the argument, and from that it can infer the type of the result `Proxy` value.

Moving on, we still have to create instances of class `Sum`. To see this viscerally, let's try to get the compiler to infer the type of `sum (Proxy :: Proxy (Cons 1 (Cons 2 (Cons 3 Nil))))`.

{% highlight haskell %}
{-# LANGUAGE PolyKinds #-} -- gives us, among other things, type-level numbers

...


sum123 :: _ -- the underscore here asks GHC to infer this type
sum123 = sum (Proxy :: Proxy (Cons 1 (Cons 2 (Cons 3 Nil))))
{% endhighlight %}

We get a compiler error:

```
No instance for (Sum (Cons 1 (Cons 2 (Cons 3 Nil))) x0)
  arising from a use of ‘sum’
```

Although we humans know the type of `sum123` should be `Proxy 6`, the compiler error is actually quite reasonable: we haven't written any instances of class `Sum` yet!

The instances are where we teach the compiler how to compute sums. In other words, instances are the actual _programming_ in this flavor of type-level programming.

{% highlight haskell %}
{-# LANGUAGE FlexibleInstances #-} -- probably should be enabled by default
{-# LANGUAGE TypeFamilies, TypeOperators #-} -- lets us add type-level numbers
{-# LANGUAGE UndecidableInstances #-} -- lets the compiler solve hard class constraints

...

instance Sum Nil 0
instance (Sum rest n2, n ~ (n1 + n2)) => Sum (Cons n1 rest) n

-- sum123 :: _
-- sum123 = sum (Proxy :: Proxy (Cons 1 (Cons 2 (Cons 3 Nil))))
{% endhighlight %}

Commenting out `sum123` for suspense, we've ported to the type system the standard recursive definition of `sum` that we encountered at the start of this post. We have two instances, a base case and a recursive case. Our base case `instance Sum Nil 0` tells the compiler that the sum of an empty list is `0`. Our recursive case `instance (Sum rest n2, n ~ (n1 + n2)) => Sum (Cons n1 rest) n` tells the compiler that the sum of `Cons n1 rest` is `n`, provided that the sum of `rest` is `n2` and that `n` equals `n1 + n2`.

{% highlight haskell %}
...

sum123 :: _
sum123 = sum (Proxy :: Proxy (Cons 1 (Cons 2 (Cons 3 Nil))))
{% endhighlight %}

```
Found type wildcard ‘_’ standing for ‘Proxy 6’
```

Uncommenting `sum123`, we see the compiler now infers the correct type, namely `Proxy 6`. That is, the sum of the type `Cons 1 (Cons 2 (Cons 3 Nil))` is the type `6`.

Let's fill the underscore with `Proxy 6` in our code and press on to the main event.

{% highlight haskell %}
{-# LANGUAGE ExplicitForAll #-}

...

sum123 :: Proxy 6
sum123 = sum (Proxy :: Proxy (Cons 1 (Cons 2 (Cons 3 Nil))))

problem1 :: forall (n :: Nat). Proxy (Cons n Nil) -> Proxy n
problem1 = sum
{% endhighlight %}

And, would you look at that! `problem1` says that `sum` has the type `forall (n :: Nat). Proxy (Cons n Nil) -> Proxy n`, which is what we wanted to show. It compiles, so it works ;-)


## Pulling Back on the Reins

If you found this post confusing and maybe even a little scary, don't worry. Programming in Haskell is really nothing like this. For that reason, I won't go out of my way to share the above "solution" with the reading group. I did it simply because I, personally, found it to be an enjoyable puzzle on which to pass an hour.

If, on the other hand, you did enjoy this post, you might want to take a look at this lovely story called [Typing the Technical Interview](https://aphyr.com/posts/342-typing-the-technical-interview), by Aphyr, from which I learned this style of programming. And it's a riot, to boot. You might also enjoy the excellent talk [4 Programming Paradigms in 45 Minutes](https://www.youtube.com/watch?v=3TBq__oKUzk) by Aja _[Thagomizer](https://www.thagomizer.com)_ Hammerly. Finally, we provide the above program (and some bonus features) conveniently as a [single file](https://gist.github.com/friedbrice/6ab87d1d36b6181ce03cae96dbc7508c).

Thanks for reading!
