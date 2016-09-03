---
layout: post
title: "A Flaw in AutoDiff"
date: 1999-12-31
permalink: /blog/1999-12-31-a-flaw-in-autodiff/
comments: true
tags:
- math
- calculus
- automatic
- symbolic
- differentiation
- haskell
---

_So, it turns out that the example that we thought broke the thing doesn't actually break the thing. I am still trying to make the counterexample work, though, because I still think the claim is not true._

some wonderful content, basically a self-contained abstract.

The intent of this section is to give a dependency-free overview of everything in this blog post.

<!--break-->

## My Implicit Assumption

some expository remarks, getting new readers up to speed.

The intent of this section is to introduce AutoDiff and summarize from the previous posts only that which is necessary for an understanding of this post. In particular, I want to talk about `fib`, how/why `d fib` is wrong, and how/why this is not a particularly bad problem (Hint, I wasn't worried about `fib` because I tacitly assumed the below claim).

## Definitions of Terms

We will not define what it means for a real number \\( a \in \mathbb{R} \\) to be representable as a floating-point number `a :: Double`. If you are comfortable knowing that \\( 5 \\), \\( \frac{3}{16} + \frac{3}{2} \\), and \\( 10^{16} \\) are representable as floating-point numbers whereas \\( 2^{-2000} \\), \\( \frac{2}{3} \\), \\( 1 - 1.23 \\), and \\( 10^{16} + 1 \\) are not representable, then you should be fine.

*Definition:* Let \\( a \in \mathbb{R} \\) be representable as `a :: Double`. Let \\( U \subseteq \mathbb{R} \\). The statement "\\( U \\) is a floating neighborhood of \\( a \\)" means that there is a real number \\( \delta > 0 \\) representable as `delta :: Double` satisfying both `a - delta < a` and \\( [a - \delta, a + \delta] \subseteq U \\).

*Definition:* Let \\( U \subseteq \mathbb{R} \\) and let \\( f : U \to \mathbb{R} \\). Let `f :: Double -> Double`. The statement "`f` is a representation-preserving implementation of \\( f \\) on \\( U \\)" means that for each \\( x \in U \\), if \\( x \\) is representable as `x :: Double`, then \\( f(x) \\) is representable as `f x`.

## Claim and Counterexample

**Claim:** _Let \\( a \in \mathbb{R} \\) be representable as `a :: Double`. Let \\( U \\) be a floating neighborhood of \\( a \\). Let \\( f : U \to \mathbb{R} \\) with representation-preserving implementation `f :: Double -> Double`. If \\( f \\) is differentiable on \\( U \\), then `d f` is a representation-preserving implementation of \\( f' \\) on \\( U \\)._

The above claim is a precise wording of the colloquial statement "if \\( f \\) is a differentiable function implemented by `f`, then `d f` is the correct implementation of \\( f' \\)." If we are to trust the results of AutoDiff, then we'd very much want the claim to be true.

My [`fib` example][2] is not enough to invalidate the claim, since the real-valued function that `fib` implements is not differentiable at any integer value other than 1. The claim is invalidated, however, by an elegant counterexample pointed out in the [comment thread][1] to my AutoDiff talk.

*Counterexample:* (Due to Robbie G. of the Santa Monica Haskell Users Group) Let `tricky :: Double -> Double` by `tricky x = if x == 0 then 100 * x else x`. `tricky` is a representation-preserving implementation of the differentiable function \\( id : x \mapsto x \\) on \\( \mathbb{R} \\). `d tricky`, however, is not a representation-preserving implementation of \\( id' \\), since \\( id'(0) = 0 \\) whereas `d tricky 0 = 100`. _<- this simply isn't true!_

_Below: Turns out `tricky` still isn't tricky enough to outsmart AutoDiff!_

```
tricky x = if x == 0 then 100 * x else x

d tricky 0
== diffDual $ tricky $ Dual 0 1
== diffDual $ if Dual 0 1 == 0 then 100 * Dual 0 1 else Dual 0 1
== diffDual $ if Dual 0 1 == Dual 0 0 then 100 * Dual 0 1 else Dual 0 1
== diffDual $ if False then 100 * Dual 0 1 else Dual 0 1
== diffDual $ Dual 0 1
== 1
```

  [1]: http://www.meetup.com/santa-monica-haskell/events/231209938/
  [2]: http://github.com/friedbrice/AutoDiff/blob/master/src/Examples.hs
