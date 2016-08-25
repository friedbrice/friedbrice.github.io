---
layout: post
title: "Automatic Differentiation Revisited"
date: 2016-08-24
permalink: /blog/15/
comments: true
tags:
- math
- calculus
- automatic
- symbolic
- differentiation
- haskell
---

[Branium][2] and the [Santa Monica Haskell Users Group][1] were kind enough to give me a chance to present about automatic differentiation for an evening, a topic we've [visited before][3]. This gave me a chance to improve my existing implementation and even add symbolic differentiation capabilities.

<!--break-->

You can find the [source code][4] on GitHub, but I'll inline the interesting bits.

{% highlight haskell linenos %}
data Dual a = Dual a a
  deriving (Eq, Read, Show)

constDual x = Dual x 0
seedDual x x' = Dual x x'
evalDual (Dual x _) = x
diffDual (Dual _ x') = x'
{% endhighlight %}

`Dual` is now a type constructor, rather than a concrete type. This lets us accomplish a neat trick, which I will point out below.

{% highlight haskell %}
instance (Eq a, Floating a) => Floating (Dual a) where
  ...
  (Dual u u') ** (Dual n 0)
    = Dual (u ** n) (u' * n * u ** (n - 1))
  (Dual a 0) ** (Dual v v')
    = Dual (a ** v) (v' * log a * a ** v)
  (Dual u u') ** (Dual v v')
    = Dual (u ** v) ((u ** v) * (v' * (log u) + (v * u' / u)))
  ...
{% endhighlight %}

Implementation of `(**)` is split into three cases in order to fix the bug mentioned in my earlier post. The three cases correspond to the power rule (variable raised to a constant), the exponential function rule (constant raised to a variable), and a third unnamed rule for finding the derivative of a variable raised to a variable. We can derive this rule using logarithmic differentiation:

$$
\begin{align*}
  y &= u^v \\
  \ln y &= \ln u^v \\
  \ln y &= v \ln u \\
  \frac{d}{dx} \ln y &= \frac{d}{dx} v \ln u \\
  \frac{y'}{y} &= v' \ln u + v \frac{u'}{u} \\
  y' &= y \left( v' \ln u + v \frac{u'}{u} \right) \\
  y' &= u^v \left( v' \ln u + v \frac{u'}{u} \right) \\
\end{align*}
$$

We also found it necessary to add an `Ord (Dual a)` instance (when `Ord a` exists, of course) for some bookkeeping later on (I forget exactly where).

{% highlight haskell %}
instance Ord a => Ord (Dual a) where
  (Dual x _) <= (Dual y _) = x <= y
{% endhighlight %}

Now for the neat trick. Normally, if we have a function \\(f\\) in hand, then you can think of differentiation as two different processes. In the first process, you can choose a specific input \\(c\\) and ask for \\(f'(c)\\), presumably some kind of concrete type. We carry out this process when we ask Haskell to evaluate `f $ Dual 5 1`, for example.

A more general process would be to _pretend_ we chose a specific number (using, say, \\(x\\) as a placeholder) and use the formalism to arrive at a formula for \\(f'\\) (in terms of \\(x\\)). In Haskell, we pretend that we chose a number like this: `\x -> f $ Dual x 1`. This lambda is the derivative \\(f'\\) of the original function \\(f\\). Let's write down this lambda expression so that we don't forget it.

{% highlight haskell %}
d :: Num a => (Dual a -> Dual c) -> a -> c
d f x = diffDual . f $ Dual x 1
{% endhighlight %}

So now, if `f` is a Haskell function with type `Num a -> Num c` (res. `Fractional`/`Floating`), then `d f` is the derivative of `f`, also of type `Num a -> Num c` (res. `Fractional`/`Floating`).

And now we see why `Dual` was made a type constructor this time around: `f` and `d f` would both be stuck being boring `Double -> Double`s if `Dual` were simply defined as `data Dual = Dual Double Double`.

There are some [illustrative examples][5] in the repo that you might want to check out, but those aside, it's time for symbolic differentiation.

For symbolic differentiation, I literally copied portions of code from Benjamin Kovach's "Abstract Nonsense" blog post, [Symbolic Calculus in Haskell][6]. Kovach defines a type `Expr a` for algebraic expressions that accept and produce values of type `a` ("accept" and "produce" in paper-pencil-land, not in Haskell).

{% highlight haskell %}
infixl 4 :+:
infixl 5 :*:

data Expr a
  = Var Char
  | Const a
  | (Expr a) :+: (Expr a)
  | (Expr a) :*: (Expr a)
  deriving (Eq, Read, Show)
{% endhighlight %}

(In practice, you'd want more than just `:+:` and `:*:`, but we're only going to implement `Num (Expr a)` today.)

Where Kovach implements a function `derivative :: (Num a) => Expr a -> Expr a` which returns an `Expr a` for the derivative of the passed `Expr a`, we take a different approach: we will write a `Num (Expr a)` instance, which will allow us to leverage our existing automatic differentiation machinery.

{% highlight haskell %}
instance Num a => Num (Expr a) where
  u + v         = u :+: v
  u * v         = u :*: v
  u - v         = u :+: Const (-1) :*: v
  fromInteger n = Const $ fromInteger n
  abs u         = undefined -- pay no attention to the
  signum u      = undefined -- unimplemented methods <.<;
{% endhighlight %}

Automatic differentiation doesn't do anything to `Expr a`s. It requires a _Haskell_ function--something like `Num a => a -> a`. So, the last ingredient we need is a way to think of an `Expr a` as a Haskell function. We get this by writing a function that evaluates an `Expr a` at a given `a`. Kovach implements a nice one. I'm going to just take one on credit for now:

{% highlight haskell %}
applyExpr :: Expr a -> a -> a
applyExpr expr x = undefined -- exercise left to the reader ;-)
{% endhighlight %}

And that's it, we now have symbolic differentiation! For example, consider the following `Expr a`s.

{% highlight haskell %}
f = 3 * Var 'x' + 4 -- represents `\x -> 3 * x + 4`
g = 4 - Var 'x' * Var 'x' -- represents `\x -> 4 - x * x`
{% endhighlight %}

Find their derivatives by:

{% highlight haskell %}
d (\x -> applyExpr f x) (Var 'x')
d (\x -> applyExpr g x) (Var 'x')
{% endhighlight %}

Or, for lack of an implementation of `applyExpr`, inline the lambdas:

{% highlight haskell %}
d (\x -> 3 * x + 4) (Var 'x')
d (\x -> 4 - x * x) (Var 'x')
{% endhighlight %}

You'll find that you get the expected results.

  [1]: https://www.meetup.com/santa-monica-haskell/
  [2]: http://www.brainiumstudios.com/site/index.html
  [3]: /blog/10/
  [4]: https://github.com/friedbrice/AutoDiff
  [5]: https://github.com/friedbrice/AutoDiff/blob/master/src/Examples.hs
  [6]: http://5outh.blogspot.in/2013/05/symbolic-calculus-in-haskell.html
