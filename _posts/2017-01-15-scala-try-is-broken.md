---
layout: post
title: "Scala Try is Broken"
date: 2017-01-15
permalink: /blog/2017-01-15/
comments: true
tags:
- code
- scala
- style
- types
---

Scala's `Try` class is broken.
Not "broken" in the sense that it doesn't fulfill its purpose: its purpose is to sandbox `Throwable`s in an algebraic sum construct and provide a monadic interface over the sandbox.
It does that just fine.
`Try` is broken in the sense that one of its methods, `_.toOption` can return malformed `Option` values, violating our trust in the semantics of the `Option` class and leading to potential instability down the call stack.

<!--break-->

## Purpose of _Try_

If you are already familiar with Scala's `Try` class, you might [skip ahead](#how-try-is-broken-and-how-to-use-it-safely).

The `scala.util.Try` class exists as a way to get a type-level representation for the result of operations that might throw.
Take, for example, integer division, which throws when attempting to divide by zero.

{% highlight scala %}
def safeDivide: (Int     , Int    ) => Try[Int]
              = (dividend, divisor) => Try(numerator / denominator)
{% endhighlight %}

Here, since `/` might throw, we have wrapped the division inside the constructor for `Try`.
More subtly, notice the type signature of `safeDivide`.
We don't return an `Int`, we return a `Try[Int]`.

`Try` itself is a sealed abstract class, with exactly two subclasses, `Success` and `Failure`.
A value of type `Try[T]` can either be a `Success[T]`, which is simply a case class wrapping a `T`, or it can be a `Failure[T]`, which doesn't actually contain a `T`, but instead contains a `Throwable`.
In code:

{% highlight scala %}
sealed abstract class Try[T] { ... }

final case class Success[T](value: T)
  extends Try[T] { ... }

final case class Failure[T](exception: Throwable)
  extends Try[T] { ... }
{% endhighlight %}

In general, we, as the caller of a procedure that might throw an exception, have a number of ways we can deal with the possibility of a thrown exception:

- Let the program halt.
- Handle the exception.
- Pass the exception up the call stack.

Letting the program halt is okay if we're at the top level, but not if we're writing a library or utility functions that will be used frequently.
Handling the error requires we provide a fallback value or return a `null`, which means we need to know something about what's going to consume that value.
Again, if you're at the top level of your program you probably know what's going to consume the value, but not if you're writing a function that's going to be used in all kinds of places.

So, for maximal code reuse, we simply want to pass the exception up the call stack and let the caller decide what to do with it (this is exactly the convention in, for example, `node.js`, where the first argument of every function is a placeholder for a possible exception).

Once we have one, how do we consume a value of `Try`?
In `node.js`, every function begins with an explicit null check on the first argument.
The analog of that in Scala would be case matching on the `Try` value.

{% highlight scala %}
val x: Int = ...
val y: Int = ...

val result: Try[Int] = safeDivide(x, y)

result match {
  case Success(v) => ...
  case Failure(e) => ...
}
{% endhighlight %}

In addition to explicit pattern matching, `Try` provides a monadic interface, with methods such as `map` and `flatMap` and with `for` notation.
For example:

{% highlight scala %}
def divider: Try[Int] = for {
  x <- Try(StdIn.readLine("Enter the dividend:\n").toInt)
  y <- Try(StdIn.readLine("Enter the divisor:\n").toInt)
  result <- Try(x / y)
} yield result
{% endhighlight %}

`StdIn.readLine(...).toInt` can fail and throw, so we wrap it in a `Try` and if successful bind the successful values to `x` and `y`.
Then we consume the `x` and `y` in `x / y`, which can also fail and throw, so we wrap that as well and yield it to the caller, wrapped in `Success`.

Here's the important part: If anything inside a `Try` constructor throws, then instead of the program halting, the `Try` will resolve to `Failure(e)` where `e` is the thrown exception as a value of the class `Throwable`.
Thus, any thrown exceptions get passed up to the caller, as was desired.

## How _Try_ is Broken and How To Use It Safely

Where `Try` exists to create a type-level abstraction for the results of operations that might throw, `Option` exists to create a type-level abstraction for values that might be `null`.

Much like `Try`, `Option` is an abstract class with two implementors, `Some` and `None`.
A value of type `Option[T]` is either a `Some[T]` which wraps a `T` or the singleton `None`.

{% highlight scala %}
sealed abstract class Option[+T] { ... }

final case class Some[+T](value: T) { ... }
  extends Option[T]

object None extends Option[Nothing] { ... }
{% endhighlight %}

`Option` provides a monadic interface that I'm sure everyone in the world is familiar with by now, and it's not all all much different from `Try` in that respect.
What we're interested in at the moment is the constructor for `Option`, found in its companion object.

{% highlight scala %}
object Option {
  def apply[T](x: T): Option[T] =
    if (x == null) None else Some(x)
}
{% endhighlight %}

The constructor *codifies the semantics we expect* of `Option` values, namely that the `x` in `Some(x)` *cannot* be `null`.
If we have an `Option[T]` in our hands, and if it is not `None`, then we're safe to extract the `T` and use it without the possibility of having a null `T`.

`Try` and `Option` both deal with failure scenarios, but a value of `Try` contains strictly more information than a value of `Option`, since `Failure` wraps a value whereas `None` doesn't wrap anything.
Because of this, there should be an obvious conversion from `Try[T]` to `Option[T]`, and this is exactly what `Try` gives us in its `_.toOption` method.

{% highlight scala %}
sealed abstract class Try[T] {

  def toOption: Option[T] = this match {
    case Success(v) => Some(v)
    case Failure(e) => None
  }
}
{% endhighlight %}

Here's the problem: `_.toOption` returns `Some(v)`, but `Try` makes no guarantee that the `v` in `Success(v)` is not `null`.
And it shouldn't make such a guarantee.
`Try` has one job, to pass thrown exceptions up the call stack, not to guard against null pointers, and `Try` does its one job well.

The problem is that `Try`'s `_.toOption` method calls `Option`'s implementor `Some` directly instead of calling `Options`'s default constructor.
This can result in the creation of `Some(v)` values where the `v` is `null`, *violating our expectations* of `Option` values.
The preferred implementation of `_.toOption` should replace the call to `Some` with a call to `Option`.

I can understand why the Scala developers would pick the implementation they did.
The implementation of `_.toOption` they chose will only convert `Failure` values to `None`, giving a completely lossless translation of `Success` values to `Some` values.
But information fidelity should not be the goal of such a method, especially when it violates our expectations of the `Option` class.
The goal of the `_.toOption` method should be to return a value that behaves the way the user *expects* `Option` values to behave, and that means no null `Some(v)` values.

If we want to be able to reuse our code, we need to be able to count on the semantics of types like `Try`, for exception passing, and `Option`, for null guarding.
The solution to the problem presented by `Try`'s `_.toOption` method is to either avoid its use or to map over the unsafe `Option` with the default constructor, both illustrated below:

{% highlight scala %}

val tryMe: Try[T] = ...

// Option 1: Avoid using `_.toOption`
val option1: Option[T] = tryMe match {
  case Success(v) => Option(v)
  case Failure(e) => None
}

// Option 2: Map the default constructor
val option2: Option[T] =
  tryMe.toOption.map(Option.apply)

{% endhighlight %}

Either of those approaches will produce an `Option` value that conforms to our expectations.
