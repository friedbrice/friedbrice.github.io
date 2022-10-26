---
layout: post
title: "Looking for a Needle that Might Not Be in an Infinite Haystack"
date: 2022-10-26
permalink: /blog/looking-for-a-needle-that-might-not-be-in-an-infinite-haystack/
redirect-from:
  - /blog/2022-10-26/
comments: true
tags:
  - haskell
  - programming
  - math
  - topology
  - computability
---

Hand-wavy explanations of my lightning talk of the same title at Mercury's October 2022 PDX Haskell meetup.
Based on "Infinite sets that admit fast exhaustive search" by Martín Escardó.

<!-- break -->

{% highlight haskell %}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE DerivingVia, FlexibleInstances #-}

module Escardo where

import Prelude hiding (Real)

import Data.List (find)
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
{% endhighlight %}

## Searchable types

We start by defining a class, `Searchable a` for types `a` that admit exhaustive search.
Given `phi :: a -> Bool` and `a0 :: a`, we say `a0` _satisfies_ `phi` if `phi a0 == True`.
We say `phi` is _satisfiable_ if at least one non-bottom member of `a` satisfies `phi`.

{% highlight haskell %}
type Search a = (a -> Bool) -> a

class Searchable a where
  -- Law: if `phi` is satisfiable then `search phi` satisfies `phi`.
  search :: Search a
{% endhighlight %}

A type `a` is _searchable_ if there is a total function `search :: Search a` where `search phi` is non-bottom and satisfies `phi` for every satisfiable total predicate `phi :: a -> Bool`.

$$
  \mathtt{Searchable} \, a
  \iff
  \left(
    \forall_{\phi : a \to \mathtt{Bool}} \,
      \left( \exists_{x : a} \, \phi \, x = \mathtt{True} \right)
      \Rightarrow
      \phi \left( \mathtt{search} \, \phi \right) = \mathtt{True}
  \right)
$$

In other words, `search phi` will find a member of `a` that satisfies `phi`, if such a member exists.

If `phi` is unsatisfiable, `search` still has to return some member of `a`, so it's the caller's responsibility to check to see whether or not the search result satisfies `phi`.
Let's automate that check.

{% highlight haskell %}
query :: Searchable a => (a -> Bool) -> Maybe a
query phi =
  case search phi of
    a0 | phi a0    -> Just a0
       | otherwise -> Nothing
{% endhighlight %}

In typical Haskell fashion, we use our API to eliminate ambiguity.
If `query phi` is `Just a0`, then `phi` is satisfiable _and_ `a0` satisfies `phi`.
If `query phi` is `Nothing`, then no member of `a` will satisfy `phi`: `phi` is just not satisfiable.
Let's automate that reasoning, too!

{% highlight haskell %}
exists :: Searchable a => (a -> Bool) -> Bool
exists = not . null . query

forAll :: Searchable a => (a -> Bool) -> Bool
forAll phi = (not . exists) (not . phi)
{% endhighlight %}

Given this framework, Our basic problem is to discover which types are `Searchable` and to implement `search` for such types.

## Finite types are searchable

Duh...

A finite type `a` is searchable, because we can implement `search` as follows.
When given a predicate `phi :: a -> Bool` we can iterating over the members of `a`, applying `phi` to each.
If we find a member of `a` that satisfies `phi`, we stop and return it.
If we exhaust `a`, we return the last member we checked.

{% highlight haskell %}
newtype Finite a = Finite a
  deriving (Bounded, Enum) via a

instance (Bounded a, Enum a) => Searchable (Finite a) where
  search phi = fromMaybe maxBound (find phi [minBound .. maxBound])
{% endhighlight %}

This is a brute-force approach, and many searchable types will have a more-elegant and more-efficient implementation of `search`.
Here's the implementation for `Bool`

{% highlight haskell %}
instance Searchable Bool where
  search phi = phi True
{% endhighlight %}

To verify that our instance is lawful, we consider three cases.

_Case 1:_ Suppose $\phi \, \mathtt{True}$ is $\mathtt{True}$.
Then $\mathtt{search} \, \phi$ satisfies $\phi$, as required.

$$
\phi \left( \mathtt{search} \, \phi \right)
  = \phi \left( \phi \, \mathtt{True} \right)
  = \phi \, \mathtt{True}
  = \mathtt{True}
$$

_Case 2:_ Supposed $\phi \, \mathtt{True} = \mathtt{False}$ and $\phi \, \mathtt{False} = \mathrm{True}$.
Then $\mathtt{search} \, \phi$ satisfies $\phi$, as required.

$$
\phi \left( \mathtt{search} \, \phi \right)
  = \phi \left( \phi \, \mathtt{True} \right)
  = \phi \, \mathtt{False}
  = \mathtt{True}
$$

_Case 3:_ Supposed $\phi \, \mathtt{True} = \phi \, \mathtt{False} = \mathrm{False}$.
Then $\mathtt{search} \, \phi$ is not required to satisfy $\phi$, so we are done.

The cases exhaust all possibilities, thus the claim is verified.

This is effectively the solution to the _Knight/Knave Riddle._
Two guards stand in front of two doors; one door leads to certain death
One guard, the knave, always lies, and the other, the knight, always tells the truth.
You have no way of telling which is which.
You may ask a single question to one of the guards.
You need to craft your question so that it's guaranteed to reveal which door is ~~safe~~ less lethal, no matter which guard you ask.

## Building searchable types inductively

If each of `a` and `b` is searchable, then their product is searchable.

{% highlight haskell %}
instance (Searchable a, Searchable b) => Searchable (a, b) where
    search phi = (a0, b0)
      where
        a0 = search phi1
        phi1 a = exists (\b -> phi (a, b))

        b0 = search phi2
        phi2 b = phi (a0, b)
{% endhighlight %}

We claim that the above instance is lawful.
To that end, suppose `phi` is satisfiable.
We need to show that `(a0, b0)` satisfies `phi`.
Since `phi` is satisfiable, `phi1` is satisfiable, because `fst :: (a,b) -> a` maps solutions of `phi` to solutions of `phi1`.
Since `phi1` is satisfiable, `a0 = search phi1` satisfies `phi1`.
Since `a0` satisfies `phi1`, `exists (\b -> phi (a0,b))` is `True`.
But notice `exists (\b -> phi (a0,b))` is identically `exists phi2`, thus `exists phi2` is `True`.
Since `exists phi2` is `True`, `phi2` is satisfiable.
Since `phi2` is satisfiable, `b0 = search phi2` satisfies `phi2`.
That is to say `phi (a0, b0)` is `True`, completing the proof.
(This proof would be _way_ simpler if I didn't have to do it constructively. Constructive logic is _way_ harder than the logic I'm used to doing.)

So we now know that the product of any two searchable types is searchable.
This generalizes in a straight-forward manner, allowing us to conclude that the product of finitely-many searchable types is searchable.
What about the product of infinitely-many searchable types?
In particular, let's consider infinite-length tuples of a searchable type `a`.
Such infinite tuples are, of course, more appropriately thought of as infinite sequences of members of `a`.
If `a` is searchable, is the type comprised of infinite sequences of members of `a` searchable?
Surprisingly, it is!

{% highlight haskell %}
type Sequence a = Natural -> a

instance Searchable a => Searchable (Sequence a) where
  search = tychonoff (const search)
{% endhighlight %}

`Sequence a` has infinitely-many non-bottom members as long as `a` has at least two non-bottom members, so we now how examples of _infinite_ types that are still searchable.
`tychonoff` is doing all the heavy lifting here.
`tychonoff` is a function that takes a sequence of searchers and uses that to build a searcher of sequences.
We thus define `search :: Search (Sequence a)` by applying `tychonoff` to the constant sequence that identically returns `search :: Search a`.

{% highlight haskell %}
tychonoff :: Sequence (Search a) -> Search (Sequence a)
tychonoff searchers phi = result
  where
    result = decode . encode $ \i ->
      searchers i $ \a ->
        psi i a $
          tychonoff
            (\i' -> searchers (i' + i + 1))
            (psi i a)

    psi i a as =
      phi $ \i' -> case compare i' i of
        LT -> result i'
        EQ -> a
        GT -> as (i' - i - 1)
{% endhighlight %}

I've read Escardó's paper four times now, and I still have no idea how `tychonoff` works.
Maybe on my fifth reading I'll see the light.
I will attempt to convey the gist of why this is even plausible in the first place, though.
I defined _searchable_ using the word _total_ to describe predicates.
The crucial fact that this gives us is that a total predicate $\phi : a \to \mathtt{Bool}$ must be non-bottom for all non-bottom members of a $a$.
Now, consider the case of a predicate defined on infinite sequences.
In order for a predicate $\phi : \mathtt{Sequence} \, a \to \mathtt{Bool}$ to be non-bottom on all non-bottom members of $\mathtt{Sequence} \, a$, there must necessarily be a largest natural number $n$ beyond which $\phi \, x$ does not use any information from $x$ for each non-bottom sequence $x$.
This is because the expression $\langle\phi \, x\rangle$ reduces to an expression $\langle\phi_1 \, [x \, 1] \, x\rangle$ which reduces to an expression $\langle\phi_2 \, [x \, 1] \, [x \, 2] \, x\rangle$ and so on.

$$
\langle\phi \, x\rangle \Rightarrow
\langle\phi_1 \, [x \, 1] \, x\rangle \Rightarrow
\langle\phi_2 \, [x \, 1] \, [x \, 2] \, x\rangle \Rightarrow
\dots \Rightarrow
\langle\phi_i \, [x \, 1] \, ... \, [x \, i] \, x\rangle \Rightarrow
\dots
$$

(Where $\langle\cdot\rangle$ is $\mathtt{apply}$ and $[\cdot]$ is $\mathtt{eval}$.)

This reduction process generates an infinite sequence of expressions.
Because we require $\phi \, x$ be non-bottom, the tail of the sequence eventually stabilizes at either the expression $\mathtt{True}$ or the expression $\mathtt{False}$.
So given any non-bottom sequence $x$, we know that $\phi \, x$ will terminate.

This explains how `tychonoff (const search) phi` terminates in the case where `phi` is satisfiable.
If we assume for the moment that we are clairvoyant and we know that `phi` is satisfiable before the fact, then we further know that iterating over all non-bottom sequences will eventually yield a sequence that satisfies `phi`.

What's still not clear to me is how `tychonoff (const search) phi` terminates in the case where `phi` is not satisfiable.
Here's my best guess, though.
Suppose a predicate $\phi$ is not satisfiable.
For each non-bottom sequence $x$, let $N_\phi(x)$ be the smallest natural number $n$ where

$$
\langle\phi_n \, [x \, 1] \, ... \, [x \, n] \, x\rangle
\in \{\langle\mathtt{True}\rangle, \langle\mathtt{False}\rangle\}
\text{.}
$$

(I.e., $N_\phi(x)$ is the point at which $\phi \, x$ terminates.)
We know that $N_\phi(x) < \infty$ for each non-bottom $x$.
What we'd like is for $N_\phi$ to have a maximum, and we'd like to be able to compute that maximum.
If $N_\phi$ had a computable maximum, then we'd know when to make $\mathtt{tychonoff} \, (\mathtt{const} \, \mathtt{search}) \, \phi$ stop looking.
Now, this is just one approach, one _very hopeful_ approach.
It's totally conceivable that there's some predicate $\phi$ out there where $N_\phi$ doesn't have a maximum.
So maybe the stopping condition for $\mathtt{tychonoff} \, (\mathtt{const} \, \mathtt{search}) \, \phi$ is found some other way.

Tangentially, we want our search to be fast, so we memoize our sequence.
A common memoization strategy in Haskell is passing a function through a concrete data structure; here we use a binary tree.
This step is optional: the search algorithm takes longer but still works without it.

{% highlight haskell %}
data Tree a = Branch a (Tree a) (Tree a)

encode :: Sequence a -> Tree a
encode f =
  Branch (f 0) (encode (\n -> f (2 * n + 1)))
               (encode (\n -> f (2 * n + 2)))

decode :: Tree a -> Sequence a
decode (Branch x l r) n =
  case n of
    0             -> x
    n | odd n     -> decode l ((n - 1) `div` 2)
      | otherwise -> decode r ((n - 2) `div` 2)
{% endhighlight %}

Here's where we stand.
Pick your favorite searchable type `a`.
Thanks to `tychonoff`, we know that the type comprised of _infinite sequences_ of members of `a`—clearly an infinite type—is also searchable.
Exhaustively.
In finite time.
Mind blowing.

## Equality of functions

Functions from `a` to `b` may be tested for equality—extensional equality, not reference equality—deterministically and in finite time if `a` is searchable and `b` instances `Eq`.

{% highlight haskell %}
instance (Searchable a, Eq b) => Eq (a -> b) where
  f == g = forAll (\a -> f a == g a)
{% endhighlight %}

Real numbers between zero and one (inclusive) may be represented by infinite sequences of bits.
This is the binary encoding of the number's decimal expansion, as decoded by `approx`.
The `Show` instance for `Real` makes use of only eight bits of information, but this will prove to be fairly expensive.
In our demo, showing `Real` numbers will end up taking longer than testing `Real`-variable functions for equality.

{% highlight haskell %}
type Real = Sequence Bool

approx :: Natural -> Real -> Double
approx bits x =
  sum [recip $ 2 ^ (i + 1) | i <- take (fromIntegral bits) [0 ..], x i]

instance Show Real where
  show x = show (approx 8 x) <> "..."

ints :: Enum a => Sequence a -> Integer -> Integer
ints x = toInteger . fromEnum . x . fromIntegral
{% endhighlight %}

## Demonstration

Here's the code I used to verify the searchability of `Real`, a file called `demo.hs`.

{% highlight haskell %}
import Prelude hiding (Real, (^))
import qualified Prelude

import Escardo
import Numeric.Natural (Natural)
import System.CPUTime (getCPUTime)

(^) :: Num a => a -> Natural -> a
(^) = (Prelude.^)
{% endhighlight %}

We define several `Integer`-valued functions of a `Real` variable to test on.
We avoid returning a `Real` or a `Double` because `Eq (a -> b)` requires `Eq b`, and while `Double` has an `Eq` instance, equality for `Double` values is somewhat flaky.
I didn't want risk having that flakiness change the outcome of any of the tests.

{% highlight haskell %}
f :: Real -> Integer
f x =
  x'
    $   10 * x' (3 ^ 80)
    +  100 * x' (4 ^ 80)
    + 1000 * x' (5 ^ 80)
  where
    x' = ints x

g :: Real -> Integer
g x =
  x'
    $   10 * x' (3 ^ 80)
    +  100 * x' (4 ^ 80)
    + 1000 * x' (6 ^ 80)
  where
    x' = ints x

h :: Real -> Integer
h x =
  if x' (4 ^ 80) == 0
    then x'        j
    else x' (100 + j)
  where
    i = if x' (5 ^ 80) == 0 then 0 else 1000
    j = if x' (3 ^ 80) == 1 then 10 + i else i
    x' = ints x
{% endhighlight %}

Our demo will test these functions for equality, finding inputs on which they disagree if applicable.
We'll annotate our test cases with various stringy labels and also the time they take to run.
In order to accurately measure this time, we force evaluation by branching on `length . show` of the output before grabbing the end time.
Otherwise, laziness could cause misleading timings.

{% highlight haskell %}
type Lbl = (,) String
type Mil = (,) Integer

diff :: (Searchable a, Eq b) => (a -> b) -> (a -> b) -> Maybe (Lbl b, Lbl b)
diff f g =
  fmap
    (\x -> (("f x", f x), ("g x", g x)))
    (query (\x -> f x /= g x))

diffShow :: (Searchable a, Eq b) => (a -> b) -> (a -> b) -> Maybe (Lbl b, Lbl b, Lbl a)
diffShow f g =
  fmap
    (\x -> (("f x", f x), ("g x", g x), ("x", x)))
    (query (\x -> f x /= g x))

timed :: IO a -> IO (Mil a)
timed x = do
  t0 <- getCPUTime
  y <- x
  t1 <- getCPUTime
  pure (div (t1 - t0) (1000 * 1000 * 1000), y)

told :: Show a => String -> a -> IO ()
told ann x = do
  print (ann, x)

did :: Show a => a -> IO a
did x = do
  case length (show x) of
    0 -> pure x
    _ -> pure x

main :: IO ()
main = do
  told "f == g" =<< timed (did $ f == g)
  told "g == h" =<< timed (did $ g == h)
  told "h == f" =<< timed (did $ h == f)

  told "f x /= g x" =<< timed (did $ diff f g)
  told "g x /= h x" =<< timed (did $ diff g h)

  told "f x /= g x" =<< timed (did $ diffShow f g)
  told "g x /= h x" =<< timed (did $ diffShow g h)
{% endhighlight %}

### Interpreted

Ran the `main` action of `demo.hs` in GHCi, which precludes any kind of optimizations and is also slower than native binaries generally.
It took about 2.5 seconds and 2.6 seconds to determine that `f` and `g` disagree and that `g` and `h` disagree, respectively.
Interestingly, it only took about 1.1 seconds to determine that `f` and `h` agree at every input.
It took about 2.6 seconds each to find inputs where `f` and `g` disagree and where `g` and `h` disagree, unsurprisingly.
To find and also print the input on which they disagree took a whopping 10.4 and 10.7 seconds, respectively.
Printing the input takes longer than finding it.

Times in the output are in milliseconds.

{% highlight console %}
daniel@newport:~/Desktop
[11:32:12] $ ghci demo.hs
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/daniel/.ghci
[1 of 2] Compiling Escardo          ( Escardo.hs, interpreted )
[2 of 2] Compiling Main             ( demo.hs, interpreted )
Ok, two modules loaded.
ghci> main
("f == g",(2534,False))
("g == h",(2684,False))
("h == f",(1135,True))
("f x /= g x",(2694,Just (("f x",0),("g x",1))))
("g x /= h x",(2667,Just (("f x",1),("g x",0))))
("f x /= g x",(10438,Just (("f x",0),("g x",1),("x",0.99609375...))))
("g x /= h x",(10769,Just (("f x",1),("g x",0),("x",0.99609375...))))
{% endhighlight %}

### Compiled

Native code generated without optimizations fared much better.
Finding inputs now takes less than half a second in all cases.
Finding and printing takes nearly two seconds.

{% highlight console %}
daniel@newport:~/Desktop
[11:33:21] $ ghc -O0 demo.hs
[1 of 2] Compiling Escardo          ( Escardo.hs, Escardo.o )
[2 of 2] Compiling Main             ( demo.hs, demo.o )
Linking demo ...
daniel@newport:~/Desktop
[11:34:56] $ ./demo
("f == g",(435,False))
("g == h",(403,False))
("h == f",(202,True))
("f x /= g x",(437,Just (("f x",0),("g x",1))))
("g x /= h x",(427,Just (("f x",1),("g x",0))))
("f x /= g x",(1909,Just (("f x",0),("g x",1),("x",0.99609375...))))
("g x /= h x",(1766,Just (("f x",1),("g x",0),("x",0.99609375...))))
{% endhighlight %}

### Compiled with optimizations

Optimizations further bring these numbers down, to less than a quarter of a second to find inputs and less than a half of a second extra if you want to print them.

{% highlight console %}
daniel@newport:~/Desktop
[11:35:52] $ ghc -O2 demo.hs
[1 of 2] Compiling Escardo          ( Escardo.hs, Escardo.o )
[2 of 2] Compiling Main             ( demo.hs, demo.o )
Linking demo ...
daniel@newport:~/Desktop
[11:35:59] $ ./demo
("f == g",(193,False))
("g == h",(216,False))
("h == f",(115,True))
("f x /= g x",(205,Just (("f x",0),("g x",1))))
("g x /= h x",(209,Just (("f x",1),("g x",0))))
("f x /= g x",(645,Just (("f x",0),("g x",1),("x",0.99609375...))))
("g x /= h x",(702,Just (("f x",1),("g x",0),("x",0.99609375...))))
{% endhighlight %}
