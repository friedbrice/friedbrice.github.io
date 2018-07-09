---
layout: post
title: "Getting More Meaning From Your Case Classes"
date: 2018-07-07
permalink: /blog/getting-more-meaning-from-your-case-classes/
redirect-from:
  - /blog/21/
  - /blog/2018-07-07/
comments: true
tags:
  - scala
  - types
---

This will be a small post where I share some techniques we found for
getting some additional type safety from our Scala case classes. We'll
look at case classes that validate their input data and we look at case
classes that allow selecting from among multiple instances of the same
typeclass.

<!--break-->

## Validating a Subset of a Type

In our current project, we have several `AnyVal` case classes that
represent some of our domain objects. Each is backed by a string that
must match a certain regex in order to make sense in our domain model.
In that sense, each of our domain case classes represents a different
subset of the type `String`. To make things a bit simpler in this blog
post, we'll consider the analogous situation where we need a subset of
the type `Double`, but all the same techniques still apply.

Let's start by considering an obviously-broken (judging by its type
signature) `squareRoot` function, and let's make things interesting by
pretending that `NaN: Double` doesn't exist, so now we have to decide
what happens when the input is negative.

{% highlight scala %}
def squareRoot(x: Double): Double = ... x ...
{% endhighlight %}

We have a few options. One thing we can do is go ahead and perform our
operation on the raw bits, passing back corrupt data to the caller.
A better alternative is to raise an exception, a perfectly fine thing
to do in a dynamically-typed language. But Scala is statically typed, so
we can do better.

One way to fix this function is to make the _range_---the set of
potential outputs---larger, by adding a special value to represent
failure. This is exactly what returning an `Option[Double]` would do for
us, but that's just one way to fix this function. The other way to fix
this function is by making the _domain_---the set of legal
inputs---smaller.

{% highlight scala %}
class NonNegative { val get: Double = ... }
def squareRoot(x: NonNegative): Double = ... x.get ...
{% endhighlight %}

Unlike the `Option` approach, the `NonNegative` approach has the
advantage of separating validation and business logic. The idea is that
if we are passed a `NonNegative`, we can be confident that validation
has already occurred at some earlier point. This brings us to our main
point: how do we implement `NonNegative` so that whenever we have one we
can be confident that validation has already occurred?

We chose to use case classes for their brevity and their equality
semantics. For runtime optimization, we extend `AnyVal`.

{% highlight scala %}
case class NonNegative(get: Double) extends AnyVal
{% endhighlight %}

Of course, case classes by default have no validation. We can implement
validation by creating our own `apply` method in the companion object,
instead of relying on the `apply` method the compiler would have
generated for us (this works in Scala 2.12 and above).

{% highlight scala %}
case class NonNegative(get: Double) extends AnyVal

object NonNegative {
  def apply(get: Double): Option[NonNegative] =
    if (get >= 0) Some(new NonNegative(get)) else None
}
{% endhighlight %}

We still have to harden our case class against alternate creation paths:
make the case class final and make the constructor private.

{% highlight scala %}
final case class NonNegative private(get: Double) extends AnyVal

object NonNegative {
  def apply(get: Double): Option[NonNegative] =
    if (get >= 0) Some(new NonNegative(get)) else None
}
{% endhighlight %}

There's still one more backdoor that we haven't boarded up. Take a look:

{% highlight scala %}
// `Option#get` is safe because we pass a literal `5` into the constructor.
val good: NonNegative = NonNegative(5).get

// but now, nefarious agents use the back door into `NonNegative`
val evil: NonNegative = good.copy(get = -5)

// and that's why our invoices are all wrong this month...
def corruptedByEvil: Double = squareRoot(evil)
{% endhighlight %}

The default `NonNegative#copy` method calls the `NonNegative` class
constructor (instead of `NonNegative.apply`), skipping our validation.
We patch this leak up by providing our own `NonNegative#copy` method
instead of relying on the default, compiler-generated `copy` method.

{% highlight scala %}
final case class NonNegative private(get: Double) extends AnyVal {
  def copy(get: Double = this.get): Option[Double] = NonNegative(get)
}

object NonNegative {
  def apply(get: Double): Option[NonNegative] =
    if (get >= 0) Some(new NonNegative(get)) else None
}
{% endhighlight %}

This is, as far as I know, the last leak in our abstraction (if you can
think of another leak, please comment below). Using this light-weight
pattern, we're able to push all of our input validation to the edges of
our app, allowing our business logic to concentrate on managing and
operating on validated data.

## Selecting from Multiple Typeclass Instances

A similar problem we had was supporting multiple typeclass instances for
a single case class. Imagine a case class with hundreds of fields which
represents two distinct domain objects, but is implemented as a single
case class in order to avoid code duplication and accidental drift.

For simplicity, we'll consider a case class with just two fields, but
imagine hundreds.

{% highlight scala %}
case class Content( freeField: String,
                    premiumField: String )

trait Renderable[T] {
  def render(t: T): HTML
}
{% endhighlight %}

Again, we want to use one case class to avoid error-prone duplication
that we'd need to remember to keep synchronized manually. At the same
time, we need  to select different instances of `Renderable` for
different customers. We ended up using mix-in traits to select the
correct typeclass instance.

Below is our full solution. Notice in particular the traits `FreeView`
and `PremiumView`, the implicit class `Views`, and the instances
`renderableFreeView` and `renderablePremiumView`.

{% highlight scala %}
sealed case class Content private( freeField: String,
                                   premiumField: String ) {
  def copy( freeField: String = this.freeField,
            premiumField: String = this.premiumField ): Content =
    Content(
      freeField = freeField,
      premiumField = premiumField
    )
}

object Content {

  sealed trait FreeView extends Content { this: Content => }
  sealed trait PremiumView extends Content { this: Content => }

  def apply( freeField: String,
             premiumField: String ): Content =
    new Content(
      freeField = freeField,
      premiumField = premiumField
    ) with FreeView with PremiumView

  implicit class Views(val self: Content) extends AnyVal {
    def freeView: FreeView = self.asInstanceOf[FreeView]
    def premiumView: PremiumView = self.asInstanceOf[PremiumView]
  }

  implicit val renderableFreeView: Renderable[FreeView] = ...
  implicit val renderablePremiumView: Renderable[PremiumView] = ...
}
{% endhighlight %}

The class casts in `Views` are safe because all creation is funneled
through `Content.apply`, which secretly imbues the returned `Content`
with both `FreeView` and `PremiumView` mix-ins. When we want to pass a
`Content` value to a `Renderable` method, we first take the appropriate
view, either `Views#freeView` or `Views#premiumView`.

{% highlight scala %}
def makeResponse[T: Renderable](body: T): HTTPResponse = ...

val content: Content = ...

val response: HTTPResponse = makeResponse(content.premiumView)
{% endhighlight %}

This pattern has a heavy footprint in terms of boilerplate. Each field
has to be stated five times: once in the case class definition, twice in
the `copy` method, and twice in the `apply` method. Still, we consider
this to be better than the alternative of simply copying the
implementation of `Content` into two distinct case classes and keeping
them synchronized manually.

Notably, we could have avoid all of this nonsense by simply writing
straightforward functions, `renderFreeView(x: Content): HTML` and
`renderPremiumView(x: Content): HTML`, and by simply calling them
explicitly instead of relying on typeclasses and implicit resolution;
however, a library we were using required us to provide these
typeclasses instances as its entry point, creating our need to write
multiple instances for the same underlying case class. While typeclasses
can be convenient and can reduce repetition, it's usually also
worthwhile for library authors to expose non-overloaded versions of
their functions to prevent forcing clients into the situation the above
pattern exists to solve.
