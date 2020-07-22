---
layout: post
title: "Purely Random"
date: 2020-06-15
permalink: /blog/purely-random/
redirect-from:
  - /blog/24/
  - /blog/2020-06-15/
comments: true
tags:
  - haskell
  - functional programming
  - math
  - probability
  - opaque type
  - firewall pattern
  - reusable instances pattern
---

The question, "How do you model randomness with pure functions," comes up frequently in functional programming forums and chat rooms. Confusing matters further is the serviceable but somewhat unmotivated API to psuedorandom number generation provided by Haskell's `System.Random` module. In this post, we present the method Mathematicians have traditionally used to model randomness (with pure functions) and port this method to Haskell, building upon the foundation of `System.Random` and exposing a robust and intuitive API.

<!--break-->


## Modeling Randomness

Suppose we need to do some probabilistic computations, such as rolling dice or drawing cards. The correct abstraction for this is what Mathematicians call a [random variable](https://en.wikipedia.org/wiki/Random_variable). In vague terms, a _random variable_ on the set \\(X\\) is a value of \\(X\\) that is unknown until it is observed and potentially different each time it is observed, with such observations being contingent on a random process.

Of course in Math there's no such thing as randomness and there's no such thing as a variable. Flowery descriptions such as the one above are merely illustrative, and the name _random variable_ itself is mostly a case of loaded terminology. That said, we still need to pick a representation. We have two main options.

  1. Define a random variable \\(v\\) on a set \\(X\\) to be a function \\(f : X \to [0, \infty)\\) with the property that the sum \\(\sum_{x \in X} f(x)\\) (or integral, if \\(X\\) is infinite) equals 1. Each value \\(f(x)\\) is then thought of as the probability of observing the element \\(x\\). Such an \\(f\\) is called the _probability density function_, \\(\mathrm{pdf}_v\\) , of the random variable \\(v\\).

  2. Define a random variable \\(v\\) on a set \\(X\\) to be a function \\(g : [0,1] \to X\\). If the set \\(X\\) is finite, then any such function is suitable. (If \\(X\\) is infinite, we need \\(g\\) to satisfy some differentiability conditions that I'm not worried about right now.) Such a function \\(g\\) is called the _cumulative distribution function_, \\(\mathrm{cdf}_v\\), of the random variable \\(v\\).

Given \\(\mathrm{pdf}_v\\) of a random variable \\(v\\), we can compute \\(\mathrm{cdf}_v\\). Vice versa, given \\(\mathrm{cdf}_v\\) we can compute \\(\mathrm{pdf}_v\\). Thus, we are free to choose either definition without loss. Notice that either definition models a random variable as a pure function, and so makes a good candidate for a functional programming implementation.

Given a set \\(X\\), write \\(\mathrm{RVar}(X)\\) to denote the set of all random variables on \\(X\\). For example, a fair coin is an element of \\(\mathrm{RVar}(\mathrm{Bool})\\) with probability density function

\\[
f(x) = \begin{cases}
    0.5 & \text{if } x = \mathrm{false} \\\\\\\\
    0.5 & \text{if } x = \mathrm{true}
\end{cases}
\\]

and cumulative distribution function

\\[
g(r) = \begin{cases}
    \mathrm{false} & \text{if } r \leq 0.5 \\\\\\\\
    \mathrm{true} & \text{if } r \leq 1
\end{cases} \text{.}
\\]

This conception of random variables has a few consequences that are important for our public API.

  * If the set \\(X\\) has an algebraic structure (such as a monoid structure or a ring structure), then the set \\(\mathrm{RVar(X)}\\) inherits that structure. In other words, random variables can be added, subtracted, multiplied, and divided as you would do so with numbers.

  * The mapping \\(X \mapsto \mathrm{RVar}(X)\\) is not only a functor but also a monad on the category of sets and functions. Indeed, given \\(v \in \mathrm{RVar}(A)\\) and \\(f : A \to B\\), we may define \\(\mathrm{RVar}(f) : \mathrm{RVar}(A) \to \mathrm{RVar}(B) \\) by noting that \\(\mathrm{cdf}_{\mathrm{RVar}(f)(v)}(r) = f(\mathrm{cdf}_v(r))\\). This is the familiar reader monad, as it applies to the \\(\mathrm{cdf}\\) representation of random variables.

  * Given a random variable in \\(\mathrm{RVar}(X)\\) and a source of entropy (such as a list of the digits of \\(\pi\\), or a clock with picosecond resolution, or a CPU with attached thermometer), we can _sample_ the random variable to produce an element of \\(X\\). The sample is deterministic based on the input, so any non-determinism is punted (as is tradition in Math, so is tradition in functional programming) to the nature of the source of entropy.

We want our API to expose the three points above while hiding particulars of our chosen representation.


## Representing Randomness

That said, our choice of representation can have surprising consequences. For example, if we choose to represent an `RVar a` as its \\(\mathrm{pdf}\\), `a -> Rational`, then we lose the ability to inspect and serialize random variables. We might, then, choose to represent the \\(\mathrm{pdf}\\) as a map `Map a Rational`, which is easily serialized and compared, but then users would only be able to create random variables on types that are instances of `Ord`.

The representation for `RVar a` we'll use places no assumptions on the nature of `a`, lends itself to serialization and comparison, and makes the implementation of sampling the random variable trivial. The cost of these conveniences is that we will have to maintain certain coherence conditions when constructing values of this type that will not be structurally enforced.

{% highlight haskell %}
data RVar a
    = Constant a
    | Step [(Rational, RVar a)]
    deriving (Eq, Read, Show)
{% endhighlight %}

The `Step` constructor gets its name by virtue of being a step function from \\([0, 1]\\) to `RVar a`. In this sense, the `Step` constructor is a slight variation of the \\(\mathrm{cdf}\\) representation, with the addition that `Step` allows nesting of `RVar`s.

Using this representation, we'd model a fair coin as follows.

{% highlight haskell %}
fairCoin :: RVar Bool
fairCoin = Step [(0.5, Constant False), (1.0 Constant True)]
{% endhighlight %}

A random variable `trueFalseOrTurkey`, defined as

{% highlight haskell %}
trueFalseOrTurkey :: RVar String
trueFalseOrTurkey =
    Step [
        (0.3, Constant "true"),
        (0.9, Constant "false"),
        (1.0, Constant "turkey")
    ]
{% endhighlight %}

represents a \\(30\%\\) chance of `"true"`, a \\(0.9 - 0.3 = 60\%\\) chance of `"false"`, and a \\(1.0 - 0.9 = 10\%\\) chance of `"turkey"`.

In order for an `RVar a` to be considered _well-formed_, the `Rational` coordinates of the list supplied to the `Step` constructor must satisfy the following conditions:

  * They must all be positive,
  * They must be in increasing order, and
  * The last one must be 1.

Notice that `fairCoin` and `trueFalseOrTurkey` are both well-formed.

Assuming our `RVar a` is well-formed, this choice of representation makes the implementation of the sampling function trivial. (In accordance with Haskell tradition, we name the sampling function `runRVar`.)

{% highlight haskell %}
runRVar :: RVar a -> IO a
runRVar (Constant x) = return x
runRVar (Step xs) = do
    r <- randomIO :: IO Double -- This is our source of entropy.
    let Just (_, x) = find (\(w, _) -> r <= fromRational w) xs
    runRVar x
{% endhighlight %}

The `Functor`, `Applicative`, and `Monad` instances for `RVar` and the `Semigroup`, `Monoid`, `Num`, `Fractional`, and `Floating` instances for `RVar a` are completely unsurprising and are included in the listing at the end of this post.


## Encapsulating Randomness

While being very convenient for our purposes, our choice of representation of `RVar a` has a few pitfalls that we'd like to spare users from falling into.

  * The `Step` data constructor places coherence conditions on its argument. This makes it easy to accidentally construct invalid data.

  * The `Read` instance on `RVar a` gives users a back door that allows accidental construction of invalid data.

  * The `Eq` instance on `RVar a` is not semantic equality. Two semantically-equivalent random variables could compare as not equal.

  * `show` on `RVar a` is not a well-defined function, in the sense that `show` can produce different results when applied to two semantically-equivalent `RVar a`s.

  * While `runRVar` is linear in the number of monadic binds used to construct an `RVar a`, `show` consumes time and space exponential in the number of monadic binds.

We've derived these unsafe `Eq`, `Read`, and `Show` instances because they make it possible for us to test our implementation, but we'd like to hide them from the public API. Haskell doesn't directly give us a way to hide specific instances, but we can accomplish this indirectly by wrapping our implementation in a `newtype` and exporting the `newtype` instead.

{% highlight haskell %}
data RVar' a
  = Constant a
  | Step [(Rational, RVar' a)]
  deriving (Eq, Read, Show)

{- implement instances for `RVar'`:
    `Functor`, `Applicative`, ...,
    `Semigroup`, `Monoid`, ..., etc. -}

newtype RVar a = Wrap (RVar' a)
  deriving (Functor, Applicative, Monad) via RVar'
  deriving (Semigroup, Monoid, Num, Fractional, Floating) via RVar' a
{% endhighlight %}

We then export the type constructor `RVar` but hide the type constructor `RVar'` and the data constructor `Wrap`.

This pattern allows us to select specific instances for export while hiding other instances. Notice how we do this by deriving the instances we'd like to export for `RVar` by delegating to the instances for the underlying `RVar'`. This way, we can export the `Funtor`-line and numeric instances without exporting the unsafe `Eq`, `Read`, and `Show` instances.

Having hidden all the data constructors, we need to give our users at least one safe way to construct an `RVar a`. We'll expose a function `density` that allows users to supply a \\(\mathrm{pdf}\\), which we'll convert into a \\(\mathrm{cdf}\\).

{% highlight haskell %}
-- Create a random variable with the given density function.
-- If your weights don't sum to 1, we'll normalize them for you.
-- `Nothing` if and only if one or more of the weights is negative.
density :: [(a, Rational)] -> Maybe (RVar a)
density pdf =
  let
    step (xs, v) (x, r) =
      let v' = v + r
      in ((<> [(v', Constant x)]) . xs, v')
    (steps, vol) = foldl' step (id, 0) pdf
    norm (r, x) = (r / vol, x)
  in
    if all (\(_, x) -> x >= 0) pdf then
      Just . Wrap . Step . fmap norm $ steps []
    else
      Nothing
{% endhighlight %}

I like to document the exact conditions under which a function into `Maybe` will produce `Nothing` so that cognizant users can decide when to use `fromJust`.

This one safe constructor and the exported instances is all we need in order to start writing random programs: we never again need to revisit the unsafe underlying implementation.


## Programing Randomness

Let's start by defining a few more constructors. Again, nothing of what follows will require access to the underlying implementation, so it could all exist in client code rather than in library code if need be.

{% highlight haskell %}
-- Create a Bernoulli random variable with given probability of `True`.
-- `Nothing` if and only if `pTrue` is negative or greater than 1.
bernoulli :: Rational -> Maybe (RVar Bool)
bernoulli pTrue = density [(True, pTrue), (False, 1 - pTrue)]

-- Create a uniform random variable.
uniform :: [a] -> RVar a
uniform =
    (\(Just v) -> v) -- I think of myself as fairly cognizant.
        . density
        . fmap (\x -> (x, 1))
{% endhighlight %}

Let's roll some dice.

{% highlight haskell %}
-- Duplicate a random variable `n` times.
-- The result is `n` independent,
-- identically-distributed random variables.
dup :: Int -> RVar a -> RVar [a]
dup n = sequence . replicate n

-- The sum of rolling `n` `s`-sided dice.
dee :: Int -> Int -> RVar Int
dee n s = fmap sum . dup n $ uniform [1..s]
{% endhighlight %}

Wow, that was easy!

Many games involve rolling several dice and summing a portion of them. None of our encapsulation measures prevent clients from writing such random programs for themselves, but it's such a common idiom that there's no reason for us not to include it.

{% highlight haskell %}
-- Drop the lowest `n` from a list and sum the rest.
dropLow :: (Num a, Ord a) => Int -> RVar [a] -> RVar a
dropLow n = fmap (sum . drop n . sort)

-- Sum the lowest `n` from a list.
takeLow :: (Num a, Ord a) => Int -> RVar [a] -> RVar a
takeLow n = fmap (sum . take n . sort)

-- Drop the highest `n` from a list and sum the rest.
dropHigh :: (Num a, Ord a) => Int -> RVar [a] -> RVar a
dropHigh n = fmap (sum . drop n . reverse . sort)

-- Sum the highest `n` from a list.
takeHigh :: (Num a, Ord a) => Int -> RVar [a] -> RVar a
takeHigh n = fmap (sum . take n . reverse . sort)
{% endhighlight %}

Dice rolls of various quantities and sizes are so common in _Dungeons and Dragons_ and similar games that they use a special notation. For example, \\(2d6 + 3\\) denotes a random variable that is sampled by rolling two six-sided dice, summing their faces, and adding three to the result.

{% highlight console %}
ghci> :type 2 `dee` 6 + 3
2 `dee` 6 + 3 :: RVar Int
ghci> runRVar $ 2 `dee` 6 + 3
9
{% endhighlight %}

Speaking of _Dungeons and Dragons_, a player character's six _ability scores_ are generated by rolling four six-sided dice, dropping the lowest die, summing the rest, and repeating this process five more times. Let's implement it.

{% highlight haskell %}
rollAttrs :: RVar (Int, Int, Int, Int, Int, Int)
rollAttrs = do
  xs <- dup 6 . dropLow 1 . dup 4 $ 1 `dee` 6
  let [x1,x2,x3,x4,x5,x6] = xs
  return (x1,x2,x3,x4,x5,x6)
{% endhighlight %}

{% highlight console %}
ghci> runRVar rollAttrs
(14,14,16,15,16,15)
{% endhighlight %}

Nice stats! I'll have to keep this character.


## Listings

Full code examples illustrating the above ideas.

### Our Random Variable Library

{% highlight haskell linenos %}
{-# LANGUAGE DerivingVia #-}

module RVars
  ( RVar

  -- Eliminators
  , runRVar

  -- Constructors
  , uniform, bernoulli, density, dee

  -- Combinators
  , dup, dropLow, takeLow, dropHigh, takeHigh
  ) where

import Data.List
import Deriving
import System.Random

newtype RVar a = Wrap { unwrap :: RVar' a }
  deriving (Functor, Applicative, Monad)
    via RVar'
  deriving (Semigroup, Monoid, Num, Fractional, Floating)
    via RVar' a

data RVar' a
  = Constant a
  | Step [(Rational, RVar' a)]
  deriving (Eq, Read, Show)
  deriving (Functor, Applicative)
    via ViaMonad RVar'
  deriving (Semigroup, Monoid, Num, Fractional, Floating)
    via (ViaApplicative RVar' a)

instance Monad RVar' where
  return = Constant
  Constant x >>= f = f x
  Step xs >>= f = Step ((fmap . fmap) (>>= f) xs)

runRVar :: RVar a -> IO a
runRVar = runRVar' . unwrap
  where
  runRVar' (Constant x) = return x
  runRVar' (Step xs) = do
    r <- randomIO :: IO Double
    let Just (_, x) = find (\(w, _) -> r <= fromRational w) xs
    runRVar' x

density :: [(a, Rational)] -> Maybe (RVar a)
density pdf =
  let
    step (xs, v) (x, r) =
      let v' = v + r
      in ((<> [(v', return x)]) . xs, v')
    (steps, vol) = foldl' step (id, 0) pdf
    norm (r, x) = (r / vol, x)
  in
    if all (\(_, x) -> x >= 0) pdf then
      Just . Wrap . Step . fmap norm $ steps []
    else
      Nothing

bernoulli :: Rational -> Maybe (RVar Bool)
bernoulli pTrue = density [(True, pTrue), (False, 1 - pTrue)]

uniform :: [a] -> RVar a
uniform = (\(Just v) -> v) . density . fmap (\x -> (x, 1))

dee :: Int -> Int -> RVar Int
dee n s = fmap sum . dup n $ uniform [1..s]

dup :: Int -> RVar a -> RVar [a]
dup n = sequence . replicate n

dropLow :: (Num a, Ord a) => Int -> RVar [a] -> RVar a
dropLow n = fmap (sum . drop n . sort)

takeLow :: (Num a, Ord a) => Int -> RVar [a] -> RVar a
takeLow n = fmap (sum . take n . sort)

dropHigh :: (Num a, Ord a) => Int -> RVar [a] -> RVar a
dropHigh n = fmap (sum . drop n . reverse . sort)

takeHigh :: (Num a, Ord a) => Int -> RVar [a] -> RVar a
takeHigh n = fmap (sum . take n . reverse . sort)
{% endhighlight %}

### Example Client Code

{% highlight haskell linenos %}
module Main where

import RVars

rollAttrs :: RVar (Int, Int, Int, Int, Int, Int)
rollAttrs = do
  xs <- dup 6 . dropLow 1 . dup 4 $ 1 `dee` 6
  let [x1,x2,x3,x4,x5,x6] = xs
  return (x1,x2,x3,x4,x5,x6)

main :: IO ()
main = print =<< runRVar (3 `dee` 6 + 2)
{% endhighlight %}

### Boilerplate

Ideally, this module would be provided in the `base` library.

{% highlight haskell linenos %}
{-# LANGUAGE DerivingVia #-}

module Deriving where

import Control.Applicative
import Control.Monad

newtype ViaMonad f a = ViaMonad (f a)
  deriving Monad via f

instance Monad f => Functor (ViaMonad f) where
  fmap = liftM

instance Monad f => Applicative (ViaMonad f) where
  pure = return
  (<*>) = ap

newtype ViaApplicative f a = ViaApplicative (f a)
  deriving (Functor, Applicative) via f

instance
    (Applicative f, Semigroup a) =>
    Semigroup (ViaApplicative f a)
  where
    (<>) = liftA2 (<>)

instance
    (Applicative f, Monoid a) =>
    Monoid (ViaApplicative f a)
  where
    mempty = pure mempty

instance
    (Applicative f, Num a) =>
    Num (ViaApplicative f a)
  where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance
    (Applicative f, Fractional a) =>
    Fractional (ViaApplicative f a)
  where
    recip = fmap recip
    fromRational = pure . fromRational

instance
    (Applicative f, Floating a) =>
    Floating (ViaApplicative f a)
  where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh
    (**) = liftA2 (**)
{% endhighlight %}
