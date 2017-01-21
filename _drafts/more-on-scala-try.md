---
layout: post
title: "More on Scala Try"
date: 2017-01-17
permalink: /blog/2017-01-17/
comments: true
tags:
- code
- scala
- style
- types
---

Turns out, Scala's `Try` class gets even worse.
Here are a few examples of how its `map` and `flatMap` methods violate our expectations of what it should mean to map over a data structure.

<!--break-->

## _Try_'s _map_ method

When we map over a data structures, we expect the outer structure to be left invariant while the internal data is operated on.
For example, when we map over a `List[String]` with a `String => Int`, we expect that the structure of the list, _e.g._ its length and the relative order of its entries, remains intact, and that the only thing that happens is that our function acts on the entries of the list.

{% highlight scala %}
val xs: List[String] = List("why", "hi", "there")

val f: String => Int = string => string.length

xs.map(f) == List(f("why"), f("hi"), f("there"))
          == List(       3,       2,          5)
{% endhighlight %}

When we speak of mapping a function `f: X => Y` over an `xo: Option[X]`, our expectations that the outer structures is left invariant means that if `xo == None`, then we expect `xo.map(f) == None`, and if `xo == Some(x)`, then we expect `xo.map(f) == Some(f(x))`.
Whether or not the outer structure is a `None` or a `Some` should be left invariant when `map` resolves.

Now, let's try to work out what our expectations are for mapping `f: X => Y` over a value `xt: Try[X]`.
`xt` is either `Failure(e)` or `Success(x)`, and mapping should preserve this outer structure.
With this in mind, we can work out the correct implementation of `map` for `Try`.

{% highlight scala %}
// ideal implementation of `map` for `Try`
class Try[X] {
  ...
  def map[Y](f: X => Y): Try[Y] = this match {
    case Success(x) => Success(f(x))
    case Failure(e) => Failure(e)
  }
  ...
}
{% endhighlight %}

Notice that this implementation does exactly what we want `Try` to do: if an exception was thrown and caught earlier, then don't do anything, just keep passing the exception up, otherwise go about your business normally, wrapped in `Success`.
But what if `f` itself might throw?
Shouldn't mapping over a Try protect us from thrown exceptions?
The answer to that is no, `map` shouldn't protect us: `flatMap` should.
If we want to catch anything thrown by `f`, all we need to do is wrap `f` in a `Try`.
Then we can use `flatMap` to combine.

{% highlight scala %}
val xt: Try[X] = ...
val f: X => Y = ...

xt.map(f) // exceptions thrown by `f` will not be caught
xt.flatMap(x => Try(f(x))) // exceptions thrown by `f` will be caught
{% endhighlight %}

In fact, this use case of combining two monadic values (namely `xt` and the result of `Try(f(x))`) is exactly what `flatMap` is for.
`map` on the other hand is for when we want to act in a way that is neutral to the outer structure.

So let's see what happens in the Scala REPL.
If `map` behaves the way we expect (_e.g._ the way `List` and `Option` behave), then `Try(12 / 3).map(throwIfTodayIsWednesday)` should throw if today is Wednesday.

{% highlight scala %}
scala> Try(12 / 3)
res0: Try[Int] = Success(4)

scala> throwIfTodayIsWednesday(4)
java.lang.Exception
  at .throwIfTodayIsWednesday(<console>:12)
  ... 29 elided

scala> Try(12 / 3).map(throwIfTodayIsWednesday)
res2: Try[Unit] = Failure(java.lang.Exception)
{% endhighlight %}

Wait, `Try(12 / 3).map(throwIfTodayIsWednesday)` didn't actually throw.
Instead, the exception got caught and got combined with the first `Try`.
If this sounds like a win, then consider the use case where we want exceptions thrown by one part of our stack to be caught and passed up, but we want exceptions thrown by a different part of our stack to halt the runtime.
`map` is, or at least would have been, the appropriate method to use for that use case.

We expect a certain kind of neutral, information-preserving action to occur when we map over a data structure, we don't expect extra logic and branching to be baked in.
In other words, it's fine that `Try` offers this functionality, but it shouldn't be called `map`.
Scala's `Try` class manages to defy our expectation, making it harder for us to reuse code with confidence.

## _Try_'s _flatMap_ Method

