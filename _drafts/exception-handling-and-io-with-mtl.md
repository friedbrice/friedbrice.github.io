---
layout: post
title: "Exception Handling and I/O with MTL"
date: 2018-04-16
permalink: /blog/exception-handling-and-io-with-mtl/
comments: true
tags:
  - haskell
  - exception handling
  - monad
  - monad transformers
---

This post is Part 3 of a series on type-safe failure handling.
[Part 1](/blog/three-models-of-exception-handling/) is in Scala and examines Scala's `Either` class as a mechanism for early-return control flow.
[Part 2](/blog/exception-handling-with-explicit-io/) ports the program to Haskell, where I/O is explicitly encapsulated by the `IO` type.
This post will demonstrate the use of monad transformers to refactor the example from Part 2.

<!--break-->

The explicit nature of Haskell's I/O model creates an extra layer of marshaling that the programmer is forced to juggle.
The standard library provides a menagerie of utility functions for this exact purpose, but those functions tend to add a lot of line noise to our code, masking the code's intent and increasing cognitive load for the programmer.

In this post, we examine some of the pros and cons of using _MTL_, Haskell's _Monad Transformer Library_. Specifically, we will see how to use MTL to separate our error-handling model and our I/O model in our functions, and we will see how to use monad transformers to recombine those models at our entry point.

# What Are Monad Transformers?

While the notion of a _monad transformer_ is not as precisely defined as most other things in Haskell tend to be, we observe that a monad transformer typically consists of three parts:

  1. a type class `MonadFoo` extending `Monad`,
  2. a type constructor `FooT` that accepts a type constructor `m :: * -> *` and produces a type constructor `FooT m :: * -> *`, and
  3. a `MonadFoo` instance for `FooT m` whenever `m` is an instance of `Monad`

(in practice, we will need several type class instances).

The name _monad transformer_ comes from the high-level idea that `FooT` transforms the monad `m` into a new monad, `FooT m`, that combines the effects of `m` and the effects of `MonadFoo`.

Let's step back and look at an example.

{% highlight haskell %}
-- | 1. A type class modeling the effects we're interested in.
class Monad m => MonadError e m where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

-- | 2. A type constructor that accepts an `m :: * -> *`
-- |    and produces `ErrorT e m :: * -> *`.
newtype ErrorT e m a = ErrorT (m (Either e a))

-- | 3. A `MonadError` instance for `ErrorT e m`
-- |    whenever `m` is an instance of `Monad`.
instance Monad m => MonadError e (ErrorT e m) where
  throwError = {- implementation details, pay no attention -}
  catchError = {- implementation details, pay no attention -}
{% endhighlight %}

We should note a few things.
First, some of the above type signatures are simplified for the purposes of this post.
Second, the thing we're calling `ErrorT` in this example is called `ExceptT` in Haskell libraries, but the class is still called `MonadError` for backwards compatibility.
Third, some will object to my use of the term _monad transformer_ to describe the three above abstractions together as a unit.
Some will use the term _monad transformer_ exclusively for the second abstraction (the `FooT` part), often using the term _MTL-style_ for the type classes and instances.

In fact, `FooT` can be used without using the `MonadFoo` type class and instances, but for the purposes of this post we will embrace the type classes.
For an example of using monad transformers that does not rely on the associated type classes, see [A Gentle Introduction to Monad Transformers][3] by kqr.
(I guess that makes this post _A Brutal Introduction to Monad Transformers_?).

# What Problems Do Monad Transformers Solve?

Consider that in the [Part 2][2] we had several functions with return type `IO (Either Response a)`.
Conceptually, we had composed `IO` and `Either e`, but our Haskell code couldn't see things that way.
It took us a non-trivial amount of effort (and generated a non-trivial amount of line noise) to manage the two layers of abstraction.

Annoyingly, we had to rely heavily on a helper function we wrote, `tunnel`, and we needed two distinct version of `implies`:

{% highlight haskell %}
tunnel :: Either e (IO (Either e a)) -> IO (Either e a)
implies :: Bool -> Response -> Either Response ()
implies' :: IO Bool -> Response -> IO (Either Response ())
{% endhighlight %}

Worse, we found ourselves writing functions with signatures like this:

{% highlight haskell %}
getResource :: () -> Request -> IO (Either Response Resource)
{% endhighlight %}

This is miserable Haskell code: why would we depend on an argument of type `()`?
We did this so that we could use `fmap` to apply `getResource` to a value `precon :: Either Response ()`, effectively making execution contingent on the success of preconditions we checked earlier.

This conditional execution is exactly what `do` notation over `Either e` is supposed to model for us, so we shouldn't have to fool around with `()` arguments.
Unfortunately for us, `do` notation was being used by `IO`.
See [Part 2][2] if you need a refresher, specifically take a look at the implementations of `handlePost`.

We want a world were we don't need to pass around `()` arguments, where we can get away with one version of `implies`, and where `tunnel` doesn't exist.
We can accomplish this using MTL and monad transformers.

# What Problems Do Monad Transformers Create?

If the promises made by monad transformers sound too good to be true, that's because they are.
Before we decide to adopt monad transformers in our application, we need to weigh the costs.

**\\(O(n^2)\\) extensibility burden**

Say we have a monad class and transformer `MonadFoo` and `FooT`.
If `m` is an instance of `Monad`, we know that `FooT m` will be an instance of `MonadFoo`.
But what if `m` was created from a transformer `BarT` and is an instance of `MonadBar`?
We want the composite `FooT m` to carry that context with it and be an instance of `MonadBar`, so we need another instance `MonadBar m => MonadBar (FooT m)`.
And down the rabbit hole we go...

For \\(n\\) monad transformer type classes, we need \\(n^2 - n\\) type class instances to smoothly wire everything together in arbitrary order.
Consider the situation where `MonadFoo`, `MonadBar`, and `MonadBaz` are classes for transformers `FooT`, `BarT`, and `BazT`, respectively.
We'll need the following six instances:

{% highlight haskell %}
MonadFoo m => MonadFoo (BarT m)
MonadFoo m => MonadFoo (BazT m)
MonadBar m => MonadBar (FooT m)
MonadBar m => MonadBar (BazT m)
MonadBaz m => MonadBaz (FooT m)
MonadBaz m => MonadBaz (BarT m)
{% endhighlight %}

If we add a fourth transformer class, say `MonadQux` and `QuxT`, then we need to add six more instances:

{% highlight haskell %}
MonadFoo m => MonadFoo (QuxT m)
MonadBar m => MonadBar (QuxT m)
MonadBaz m => MonadBaz (QuxT m)
MonadQux m => MonadQux (FooT m)
MonadQux m => MonadQux (BarT m)
MonadQux m => MonadQux (BazT m)
{% endhighlight %}

The good news (if you can call it that) is that we don't need to modify any existing code in order to add a new transformer class.
The bad news is the amount of new code that we have to write grows each time.
In general, adding transformer class \\(n+1\\) requires writing an additional \\(2n\\) instances.
This is a code maintenance nightmare.
For this reason, it's probably best if you don't write your own custom transformer classes and stick to using predefined ones.

<!-- A partial solution to this problem is to compose our monad transformers as late as possible in our application stack (preferably only at the entry point) and to adopt strong conventions relating to the composition order of monad transformers (so that we only need to maintain linearly-many instances instead of quadratically-many); however, it's often the case that the desired semantics of the composed monad dictates a particular transformer composition order. -->

**Forcing Design Decisions on your Clients**

Haskell has multiple incompatible effects-modeling paradigms, with even more on the horizon.
Exposing MTL-style in library entry points forces clients to either adopt monad transformers as their effects-modeling paradigm or else write a bunch of boiler-plate code in order to pull your library code out of the MTL abstraction and into their preferred abstraction.
Go ahead and use MTL-style monad transformers for you application code if it's your preferred effects-modeling paradigm, but don't make that choice for your library's clients.
Use plain types like `Either` and `IO` in your library entry points, and let the caller lift them into whatever effects-paradigm they use for their application.

# How Do I Use Monad Transformers in My Existing Code?

We'll take a look at our code from [Part 2][2] and see how we can clean it up a bit using MTL.
Using MTL on this small example is absolute overkill---we're introducing complexity for complexity's sake.
This is intentional: the example is kept small for the sake of this post.
When you introduce complexity in your critical code, please always consider whether or not it's worth the cost.

With that disclaimer out of the way, let's get hacking.

Go ahead and clone the [demo project][4] if you are so inclined.
_README.md_ has some instructions for building using _make_.
Everything should work out-of-the-box if you have _GHC_ installed.

To begin, `git checkout mtl-demo` and `watch make test`.

The project implements the same program, a simple HTTP POST handler, three different ways, and you should see that the tests are passing for _Continuations_ and for _Eithers_ but are failing for _Transformers_.

Here's what I see:

<pre>
Running Test Suite

Testing Continuations:
        Response {code = 400, content = "You must provide a non-empty request body"}
        Passed: should handle requests with no body
        Response {code = 401, content = "You must provide an authorization header field"}
        Passed: should handle requests with no auth
        Response {code = 405, content = "Method not allowed: FOO"}
        Passed: should handle requests with wrong method
Passed.

Testing Eithers:
        Response {code = 400, content = "You must provide a non-empty request body"}
        Passed: should handle requests with no body
        Response {code = 401, content = "You must provide an authorization header field"}
        Passed: should handle requests with no auth
        Response {code = 405, content = "Method not allowed: FOO"}
        Passed: should handle requests with wrong method
Passed.

Testing Transformers:
Test.hs: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at ./Transformers.hs:24:18 in main:Transformers
make: *** [Makefile:9: test] Error 1
</pre>

We have failing tests, let's go ahead and open up _Transformers.hs_ in our text editor and poke around a bit.
Also take a look at _Spec.hs_ to see what it is we have to work with.
I'll wait :-)

Back? Let's start thinking about our implementation by comparing the function signatures from _Eithers.hs_ with the function signatures for _Transformers.hs_.

In _Eithers.hs_, we have:

{% highlight haskell %}
getUser :: Request -> Either Response User

getResource :: () -> Request -> IO (Either Response Resource)

execute :: () -> String -> User -> Resource -> IO (Either Response ())

handlePost :: Request -> IO Response
{% endhighlight %}

In _Transformers.hs_, these become:

{% highlight haskell %}
getUser :: MonadError Response m
        => Request -> m User

getResource :: (MonadError Response m, MonadIO m)
            => Request -> m Resource

execute :: (MonadError Response m, MonadIO m)
        => String -> User -> Resource -> m ()

handlePost :: Request -> IO Response
{% endhighlight %}

`handlePost` stays the same, so code that depends on us doesn't have to change at all.
`getUser` stays more-or-less the same, except `Either Response` is replaced by an abstract `m`.
`getResource` and `execute` will no longer need that annoying `()` argument, and---crucially---the no longer return a two-layered `IO (Either Response _)`, they return a single-layer `m _`.

Let's also look at the helper function `implies`:

{% highlight haskell %}
implies :: MonadError e m => Bool -> e -> m ()
failure `implies` fallback = undefined
{% endhighlight %}

How do we define it?
Recall from _Eithers.hs_ (or open it in your editor) that `implies` was defined by `if failure then Left fallback else Right ()`.
We want something similar but using the `MonadError` methods.
`throwError` has type `e -> m a`, so it replaces the `Left` constructor (with type `e -> Either e a`), and the `Monad` method `return` can be used to replace the `Right` constructor.

{% highlight haskell %}
failure `implies` fallback = if failure then throwError fallback else return ()
{% endhighlight %}

A hard part to grok at first is how the parameter `e` gets subsumed by the abstract monad `m`.
Consider the following:

{% highlight haskell %}
Left       ::                   e -> Either e a
Right      ::                   a -> Either e a

throwError :: MonadError e m => e ->        m a
return     :: MonadError e m => a ->        m a
{% endhighlight %}

Imagining replacing the `Either e` with an abstract `m` helped me get grounded when I was first learning this.

But we digress.
What we have now is an `implies` function that works for any `m` that is an instance of `MonadError e`, including the abstract `m`s in the signatures of `getResource` and `execute`.
That's why we can now get away with having only one `implies` function instead of two.

Let's take care of `getUser` next.
For convenience, here's the implementation from _Eithers.hs_:

{% highlight haskell linenos %}
getUser :: Request -> Either Response User
getUser (Request _ _ _ header) = do
  token <- maybe (Left noToken) Right $ lookup "Authorization" header
  is_malformed_token `implies` malformedToken token
  is_user_not_found `implies` noUser token
  return the_user
{% endhighlight %}

Since `implies` is now generic, we don't even need to touch lines 4 or 5.
We'll have to replace `Left` and `Right` in Line 3, though.
We end up with

{% highlight haskell %}
getUser :: MonadError Response m => Request -> m User
getUser (Request _ _ _ header) = do
  token <- maybe (throwError noToken) return $ lookup "Authorization" header
  is_malformed_token `implies` malformedToken token
  is_user_not_found `implies` noUser token
  return the_user
{% endhighlight %}

On to `getResource`.
Here's the version from _Eithers.hs_:

{% highlight haskell linenos %}
getResource :: () -> Request -> IO (Either Response Resource)
getResource method (Request path _ _ _) = do
  let doResource = (\_ -> the_resource_io) :: () -> IO Resource
  notFound <- is_resource_not_found_io `implies'` noResource path
  doResource `traverse` notFound
{% endhighlight %}

Take a look at the `do` block on Line 2.
What monad is it over?
`getResource` returns an `IO _`, so is the `do` block over `IO`?
No, take a closer look, particularly at the bind on Line 4.
We're binding an `Either Response _` to `notFound` in order to conditionally apply `doResource`.
This is a `do` block over `Either Response`, not over `IO`.
All of this is to say that this function's implementation is ugly AF.

Ultimately, we want to do `IO`, but we need to conditional semantics of `Either e`.
This is exactly what motivates our use of MTL.
We want to work over a single abstract monad `m` that combines the conditional semantics of `Either e` and the procedural semantics of `IO`.
Thus the type signature of `getResource` in _Transformers.hs_ is `(MonadError Response m, MonadIO m) => Request -> m Resource`.

Now, no abstraction is perfect, and we'll still need a lot of line noise.
Let's begin, though, by writing purely our intent, and then we'll fix compiler errors as they crop up.
The function we'd like to write is:

{% highlight haskell %}
getResource :: (MonadError Response m, MonadIO m) => Request -> m Resource
getResource (Request path _ _ _) = do
  is_resource_not_found_io `implies` noResource path
  the_resource_io
{% endhighlight %}

Simple, beautiful code.
Unfortunately, it doesn't type check.
`is_resource_not_found_io` is an `IO Bool`, but `implies` requires a `Bool`.
We can't `bind` the `Bool` out using `(>>=)`, because `implies` returns an abstract `m ()`, not an `IO ()`.
We need to _abstractify_ `is_resource_not_found_io`.
This is exactly what the MTL function `liftIO` does for us.

{% highlight haskell %}
liftIO :: MonadIO m => IO a -> m a
{% endhighlight %}

The abstract `m` in the signature of `getResource` is an instance of `MonadIO`, so we can use `liftIO` to take an arbitrary `IO a` and treat it as an `m a`.
Let's use it and see if we can squash this compiler error.
Notice we'll also need to lift `the_resource_io` so that the last line has the correct abstract type.
We now have:

{% highlight haskell %}
getResource :: (MonadError Response m, MonadIO m) => Request -> m Resource
getResource (Request path _ _ _) = do
  liftIO is_resource_not_found_io `implies` noResource path
  liftIO the_resource_io
{% endhighlight %}

We're getting there.
The compiler now tells us that we're trying to plug an `m Bool` into a function that expects a `Bool`.
Fixing things like that is the _first_ interesting thing we learn to do in Haskell!

{% highlight haskell %}
getResource :: (MonadError Response m, MonadIO m) => Request -> m Resource
getResource (Request path _ _ _) = do
  is_resource_not_found <- liftIO is_resource_not_found_io
  is_resource_not_found `implies` noResource path
  liftIO the_resource_io
{% endhighlight %}

Code compiles: success!
We didn't really save any lines, but our code now looks a lot more like idiomatic Haskell and is much easier to read.

Now, me being me, I had to code-golf it further:

{% highlight haskell %}
getResource :: (MonadError Response m, MonadIO m) => Request -> m Resource
getResource (Request path _ _ _) = do
  liftIO is_resource_not_found_io >>= (`implies` noResource path)
  liftIO the_resource_io
{% endhighlight %}

I justify this by saying that the intent of the code is to apply `implies` with arguments `is_resource_not_found_io` and `noResource path`.
The `liftIO` and the `>>=` are just there to make it type check.
Yeah, I don't think it's very readable this way, but it's 50% shorter...

![Cypher meme: I don't even see the line noise.]({{ "/assets/img/line-noise.jpg" | relative_url }}){: .center-image }

Moving on, let's tackle `execute`.

# What are the Alternatives to using MTL?

  [1]: /blog/three-models-of-exception-handling/
  [2]: /blog/exception-handling-with-explicit-io/
  [3]: http://two-wrongs.com/a-gentle-introduction-to-monad-transformers
  [4]: https://github.com/friedbrice/exception-control-flow
