---
layout: post
title: "Exception Handling and I/O with MTL"
date: 2018-04-18
permalink: /blog/exception-handling-and-io-with-mtl/
redirect-from:
  - /blog/19/
  - /blog/2018-04-18/
comments: true
tags:
  - haskell
  - failure handling
  - monad
  - monad transformer
  - design patterns
  - style
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

  1. A type class `MonadFoo` extending `Monad`,
  2. A type constructor `FooT` that accepts a type constructor `m :: * -> *` and produces a type constructor `FooT m :: * -> *`, and
  3. A `MonadFoo` instance for `FooT m` whenever `m` is an instance of `Monad`

(in practice, we will need several type class instances).

The name _monad transformer_ comes from the high-level idea that `FooT` transforms the monad `m` into a new monad, `FooT m`, that combines the effects of `m` and the effects of `MonadFoo`.

Let's step back and look at an example.

{% highlight haskell %}
-- 1. A type class modeling the effects we're interested in.
class Monad m => MonadError e m where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

-- 2. A type constructor that accepts an `m :: * -> *`
--    and produces `ErrorT e m :: * -> *`.
newtype ErrorT e m a = ErrorT (m (Either e a))

-- 3. A `MonadError` instance for `ErrorT e m`
--    whenever `m` is an instance of `Monad`.
instance Monad m => MonadError e (ErrorT e m) where
  throwError = {- blah blah blah -}
  catchError = {- blah blah -}
{% endhighlight %}

We should note a few things.
First, some of the above type signatures are simplified for the purposes of this post.
Second, the thing we're calling `ErrorT` in this example is now called `ExceptT` in Haskell libraries, but the type class is still called `MonadError` for backwards compatibility.
Third, some will object to my use of the term _monad transformer_ to describe the three above abstractions together as a unit.
Some will use the term _monad transformer_ exclusively for the second abstraction (the `FooT` part), often using the term _MTL-style_ for the type classes and instances.

In fact, `FooT` can be used without using the `MonadFoo` type class and instances, but for the purposes of this post we will embrace the type classes.
For an example of using monad transformers that doesn't rely on the associated type classes, see [A Gentle Introduction to Monad Transformers][3] by kqr.
(I guess that makes this post _A Brutal Introduction to Monad Transformers_?)

# What Problems Do Monad Transformers Solve?

Consider that in the [Part 2][2] we had several functions with return type `IO (Either Response a)`.
Conceptually, we had composed `IO` and `Either e` to get the effects of both, but our Haskell code, particularly `do` notation, couldn't see things that way.
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
We did this so that we could use `fmap` to apply `getResource` to a value `precon :: Either Response ()` representing the preconditions under which we wanted to proceed, effectively making execution of `getResource` contingent on the success of said preconditions.

This conditional execution is exactly what `do` notation over `Either e` is supposed to model for us, so we shouldn't have to fool around with `()` arguments.
Unfortunately for us, we could not use `do` notation over both `IO` and `Either e` simultaneously: it works for one at a time.
See [Part 2][2] if you'd like a deeper look.

We deserve a world were we don't need to pass around `()` arguments, where we can get away with one version of `implies`, where `do` notations gives us both short-circuit logic and I/O sequencing at the same time, and where `tunnel` doesn't exist.
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

The good news (if you can call it that) is that we don't need to modify any existing code in order to add new transformer classes.
The bad news is the amount of new code that we have to write grows each time.
In general, adding transformer class \\(n+1\\) requires writing an additional \\(2n\\) instances.
This is a code maintenance nightmare.
For this reason, it's probably best if you don't write your own custom transformer classes and stick to using predefined ones.

<!-- A partial solution to this problem is to compose our monad transformers as late as possible in our application stack (preferably only at the entry point) and to adopt strong conventions relating to the composition order of monad transformers (so that we only need to maintain linearly-many instances instead of quadratically-many); however, it's often the case that the desired semantics of the composed monad dictates a particular transformer composition order. -->

**Forcing Design Decisions on your Clients**

Haskell has multiple incompatible effects-modeling paradigms, with even more on the horizon.
Exposing MTL-style classes in library entry points forces clients to either adopt monad transformers as their effects-modeling paradigm or else write a bunch of boiler-plate code in order to pull your library code out of the MTL abstraction and into their preferred abstraction.
Go ahead and use MTL-style monad transformers for you application code if it's your preferred effects-modeling paradigm, but don't make that choice for your library's clients.
Use plain types like `Either` and `IO` in your library entry points, and let the caller lift them into whatever effects-paradigm they use for their application.

# How Do I Use Monad Transformers?

We'll take a look at our code from [Part 2][2] and see how we can clean it up a bit using MTL.
Using MTL on this small example is absolute overkill---we're introducing complexity for complexity's sake.
This is intentional: the example is kept small for the sake of this post.
When you introduce complexity in your critical code, please always consider whether or not it's worth the cost.

With that disclaimer out of the way, let's get hacking.

Go ahead and clone the [demo project][4] if you are so inclined.
_README.md_ has some instructions for building using _make_.
Everything should work out-of-the-box if you have _GHC_ installed.

To begin, `git checkout mtl-demo` and `make test`.

**Edit:** If `make test` fails, you may need to `cabal install mtl`.
If it still doesn't work, please let me know in an [email](mailto:danielbrice@gmail.com).

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
</pre>

We have failing tests, let's go ahead and open up _Transformers.hs_ in our text editor and poke around a bit.
Also take a look at [_Spec.hs_][8] to see what it is we have to work with.
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
getUser :: MonadError Response m => Request -> m User

getResource :: (MonadError Response m, MonadIO m) => Request -> m Resource

execute :: (MonadError Response m, MonadIO m)
        => String -> User -> Resource -> m ()

handlePost :: Request -> IO Response
{% endhighlight %}

`handlePost` keeps the same signature, so code that depends on us doesn't have to change.
`getUser` stays more-or-less the same, except `Either Response` is replaced by an abstract `m`.
`getResource` and `execute` will no longer need that annoying `()` argument, and---crucially---they no longer return a two-layered `IO (Either Response _)`, they return a single-layer `m _`.

Let's also look at the helper function `implies`:

{% highlight haskell %}
implies :: MonadError e m => Bool -> e -> m ()
failure `implies` fallback = undefined
{% endhighlight %}

How do we define it?
Recall from _Eithers.hs_ (or open it in your editor) that `implies` was defined by `if failure then Left fallback else Right ()`.
We want something similar but using the `MonadError` methods.
`throwError` has type `e -> m a`, so it replaces the `Left` constructor (with type `e -> Either e a`), and the `return` function can be used to replace the `Right` constructor.

{% highlight haskell %}
failure `implies` fallback = if failure then throwError fallback else return ()
{% endhighlight %}

A hard part to grok at first is how the parameter `e` gets subsumed by the abstract monad `m`.
Imagining replacing `Either e` with an abstract `m` helped me get grounded when I was first learning this stuff.

{% highlight haskell %}
Left       ::                   e -> Either e a
Right      ::                   a -> Either e a

throwError :: MonadError e m => e ->        m a
return     :: MonadError e m => a ->        m a
{% endhighlight %}

But we digress.
What we have now is an `implies` function that works for any `m` that is an instance of `MonadError e`, including the abstract `m`s in the signatures of `getResource` and `execute`.
That's why we can now get away with having only one `implies` function instead of two.

Let's take care of `getUser` next.
Here's the implementation from _Eithers.hs_:

{% highlight haskell linenos %}
getUser :: Request -> Either Response User
getUser (Request _ _ _ header) = do
  token <- maybe (Left noToken) Right $ lookup "Authorization" header
  is_malformed_token `implies` malformedToken token
  is_user_not_found `implies` noUser token
  return the_user
{% endhighlight %}

Since `implies` is now generic, we don't even need to touch Lines 4 or 5.
We'll have to replace `Left` and `Right` in Line 3, though.
Replace it with

{% highlight haskell %}
  token <- maybe (throwError noToken) return $ lookup "Authorization" header
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

This function is a little bit unidiomatic.
We define a function `() -> IO Resource` in the middle of a `do` block over `IO`.
We do this so that we can `traverse` it over an `Either` value, which we call `notFound`, effectively making execution of the `IO` action conditional on the success of `notFound`.

We need to do I/O in order to determine whether or not the resource is found, then we want to use the conditional semantics of `Either e` to decide whether or not we should do further I/O and grab hold of the resource.
This is exactly what motivates our use of MTL.
We want to work over a single abstract monad `m` that combines the conditional semantics of `Either e` and the procedural semantics of `IO`.
Thus the type signature of `getResource` in _Transformers.hs_ is `(MonadError Response m, MonadIO m) => Request -> m Resource`.

Now, this abstraction isn't perfect, and we'll still need a lot of line noise.
Let's begin, though, by writing purely our intent, and then we'll fix compiler errors as they crop up.
The function we'd like to write is:

{% highlight haskell %}
getResource :: (MonadError Response m, MonadIO m) => Request -> m Resource
getResource (Request path _ _ _) = do
  is_resource_not_found_io `implies` noResource path
  the_resource_io
{% endhighlight %}

Simple, beautiful code.
Unfortunately, it doesn't type check (try `make test`).
`is_resource_not_found_io` is an `IO Bool`, but `implies` requires a `Bool`.
We can't simply bind away the `IO`, because `implies` returns an abstract `m ()`, not an `IO ()`.
In a sense, we need to _abstractify_ `is_resource_not_found_io`, turning it into an `m Bool`.
This is exactly what the library function `liftIO` does for us.

{% highlight haskell %}
liftIO :: MonadIO m => IO a -> m a
{% endhighlight %}

The abstract `m` in the signature of `getResource` is an instance of `MonadIO`, so we can use `liftIO` to take an arbitrary `IO a` and treat it as an `m a`.
Let's see if using it squashes this compiler error.
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

I justify this refactor by saying that the intent of the code is to apply `implies` with arguments `is_resource_not_found_io` and `noResource path`.
The `liftIO` and the `>>=` are just there to make it type check.

<pre>
  <span style="color: #bbb;">liftIO</span> is_resource_not_found_io <span style="color: #bbb;">>>=</span> <span style="color: #bbb;">(</span>`implies` noResource path<span style="color: #bbb;">)</span>
</pre>

Maybe that's a bit of a stretch...

![Cypher meme: I don't even see the line noise.]({{ "/assets/img/line-noise.jpg" | relative_url }}){: .center-image }

Moving on, let's tackle `execute`.
Here's the version from _Exceptions.hs_:

{% highlight haskell %}
execute :: () -> String -> User -> Resource -> IO (Either Response ())
execute method body usr (Resource path) = do
  let doExecuted = (\_ -> is_executed_io) :: () -> IO Bool
  permitted <- (not <$> is_permitted_io) `implies'` notPermitted path
  executed <- doExecuted `traverse` permitted
  return ((not <$> executed) >>= (`implies` badConnection))
{% endhighlight %}

What do we really want out of this function?
We want to ensure that the user has the correct permissions before executing, then we want to execute and check the exit status.
In non-compiling pseudo-Haskell, we have

{% highlight haskell %}
execute :: (MonadError Response m, MonadIO m)
        => String -> User -> Resource -> m ()
execute body usr (Resource path) = do
  not is_permitted_io `implies` notPermitted path
  not is_executed_io `implies` badConnection
{% endhighlight %}

which, of course, doesn't compile.
First, we need to map `not` over the two `IO Bool`s.

{% highlight haskell %}
  (not <$> is_permitted_io) `implies` notPermitted path
  (not <$> is_executed_io) `implies` badConnection
{% endhighlight %}

Next, we need to lift and bind the `IO Bool`s, as we did with `getResource`.

{% highlight haskell %}
  liftIO (not <$> is_permitted_io) >>= (`implies` notPermitted path)
  liftIO (not <$> is_executed_io) >>= (`implies` badConnection)
{% endhighlight %}

And that takes care of `execute`.
I really enjoy the workflow of writing out our intent in code we know won't compile and then iteratively fixing the errors.
It helps me keep track of what it is, conceptually, that we want this function to do.

Finally, let's implement `handlePost`.
Here's the version from _Eithers.hs_ (with type annotations added):

{% highlight haskell %}
handlePost :: Request -> IO Response
handlePost req@(Request path method body _) = either id id <$> result where

  chkMth :: Either Response ()
  chkMth = (method /= "POST") `implies` notAllowed method

  errBdy :: Either Response String
  errBdy = null body `implies` noBody >> return body

  errUsr :: Either Response User
  errUsr = getUser req

  precon :: Either Response ()
  precon = chkMth >> errBdy >> errUsr >> return ()

  tunnel :: Either e (IO (Either e a)) -> IO (Either e a)
  tunnel eitherIoEither = join <$> sequenceA eitherIoEither

  result :: IO (Either Response Response)
  result = do
    errSrc <- tunnel (getResource <$> precon <*> pure req)
    errExe <- tunnel (execute <$> precon <*> errBdy <*> errUsr <*> errSrc)
    return (errExe >> return (success path body))
{% endhighlight %}

We're going to need three things from `handlePost`.
First, we need it to implement our request-handling logic.
Second, we need it to select a concrete implementation for our abstract monad `m`.
Our above functions need `m` to have `MonadError Response` and `MonadIO` instances, so the minimal concrete implementation we can use is `m = ExceptT Response IO`.
Third, `handlePost` needs to unwrap the `ExceptT Response IO Response` and wrangle it into an `IO Response`.

Let's split out all those distinct tasks, then `handlePost` can simply be their composition.

{% highlight haskell %}
handlePost1 :: (MonadError Response m, MonadIO m)
            => Request -> m Response
handlePost1 = undefined -- our program logic

handlePost2 :: (MonadError Response m, MonadIO m)
            => m a -> ExceptT Response IO a
handlePost2 = undefined -- select a concrete implementation for `m`

handlePost3 :: ExceptT Response IO Response -> IO Response
handlePost3 = undefined -- unwrap the ExceptT

handlePost :: Request -> IO Response
handlePost req = handlePost3 (handlePost2 (handlePost1 req))
{% endhighlight %}

We've left their implementations undefined, so the tests will fail, but the type signatures line up, so this code should compile.
Let's copy it into our _Transformers.hs_ and `make test`.
What happens?

Compiler error?
No Way!

<pre>
Transformers.hs:42:31: error:
    • Ambiguous type variable ‘m0’ arising from a use of ‘handlePost2’
      prevents the constraint ‘(MonadError
                                  Response m0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘m0’ should be.
      These potential instances exist:
        instance [safe] MonadError e (Either e)
          -- Defined in ‘Control.Monad.Error.Class’
        instance [safe] Monad m => MonadError e (ExceptT e m)
          -- Defined in ‘Control.Monad.Error.Class’
        ...plus 11 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘handlePost3’, namely
        ‘(handlePost2 (handlePost1 req))’
      In the expression: handlePost3 (handlePost2 (handlePost1 req))
      In an equation for ‘handlePost’:
          handlePost req = handlePost3 (handlePost2 (handlePost1 req))
   |
42 | handlePost req = handlePost3 (handlePost2 (handlePost1 req))
   |                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
</pre>

`handlePost1` outputs a `(MonadError Response m, MonadIO m) => m Response`.
`handlePost2` accepts a `(MonadError Response m, MonadIO m) => m Response`.
It looks as though `handlePost2` should be able to accept the output of `handlePost1`,
and indeed it can, that's not the problem.

The problem is as follows.
Since no type class constraints appear in the signature of `handlePost`, Haskell has to decide then and there what type class instances to use when evaluating `handlePost` (the decision can't be deferred to a later point, since `handlePost` doesn't carry any constraints).
However, since we're effectively hiding the parameter `m` in between function calls, we're not providing enough contextual information for Haskell to be able to infer a type and lookup a type class instance.

This is not a problem unique to monad transformers.
It crops up when we compose functions with polymorphic outputs and functions with polymorphic inputs.
Below is a minimal example.

{% highlight haskell %}
class Foo a where
  -- no methods, which is fine

bar :: Foo a => String -> a
bar = undefined

baz :: Foo a => a -> Int
baz = undefined

qux :: String -> Int
qux x = baz (bar x)
{% endhighlight %}

Try compiling.

<pre>
    • No instance for (Foo a0) arising from a use of ‘baz’
    • In the expression: baz (bar x)
      In an equation for ‘qux’: qux x = baz (bar x)
   |
63 | qux x = baz (bar x)
   |         ^^^^^^^^^^^
</pre>

I bring this up here because many Haskellers don't encounter this problem until they start using MTL-style monad transformers in their code.
The practice I follow to avoid this problem is to only use abstract monads as part of the output of my functions, never as part of the input.
Hopefully, this side-quest saves you a few hours of frustration.

Incidentally, we could have detected that something in our design was a little bit off by thinking about the type signature of `handlePost2`.
Read it carefully.

{% highlight haskell %}
handlePost2 :: (MonadError Response m, MonadIO m)
            => m a -> ExceptT Response IO a
{% endhighlight %}

It says that no matter what abstract monad `m` we have, we claim that if it is an instance of `MonadError Response` and `MonadIO` we can recreate it concretely as an `ExceptT Response IO`.
In other words, we claim that we can, without knowing our abstract data structure `m`, faithfully create a copy with concrete implementation `ExceptT Response IO`, using only the methods defined by `MonadError` and `MonadIO`.
That's a pretty bold claim, and while I don't know this for a fact, I strongly suspect that such a function is impossible to implement.
A function signature that claims to creates a concrete copy of an abstract object should have set off our alarm bells.
Fortunately, it set off the compiler's alarm bells.

Let's get back to writing `handlePost`.
The fix is very simple.
`handlePost2` was supposed to select a concrete type for `m`.
Simply remove `handlePost2` and replace its functionality with a type annotation.

{% highlight haskell %}
handlePost1 :: (MonadError Response m, MonadIO m)
            => Request -> m Response
handlePost1 = undefined -- our program logic

handlePost3 :: ExceptT Response IO Response -> IO Response
handlePost3 = undefined -- unwrap the ExceptT

handlePost :: Request -> IO Response
handlePost req = handlePost3 (handlePost1 req :: ExceptT Response IO Response)
{% endhighlight %}

You might protest: a moment ago I said you can't get a concrete object from an abstract object.
Yet, the code now compiles (try `make test`), so what gives?
Consider the following.
The type annotation gives us a concrete object from a _particular_ abstraction (namely, from `handlePost1 req`).
`handlePost2` claimed to be able to get a concrete object from _any_ abstraction we fed it.

Here's another way of thinking about it.
The type annotation is just how we specify the implementation the compiled code should use.
A working `handlePost2`, on the other hand, would mean that a terrible person---our rival, say---picks the implementation of `m`, and then we'd be obligated to find some way to get an `ExceptT Response IO` out of it, no matter how nasty of an `m` they pick.

![Our rival, Gary, picking a nasty implementation of m]({{ "/assets/img/our-rival-gary.jpg" | relative_url }}){: .center-image }

Moving on, let's try to write `handlePost3`.
The job of `handlePost3` is to eliminate the `Either`, merging the error path and the happy path.
For brevity, let `e` stand in for `Response`.
First, we use the library function `runExceptT` to unwrap the `ExceptT e IO e` into a nested `IO (Either e e)`.
Then, we map `either id id` over the `IO` to turn the `Either e e` into simply an `e`.
This leads us to the following implementation:

{% highlight haskell %}
handlePost3 = (either id id <$>) . runExceptT
{% endhighlight %}

Here's the above paragraph in diagram form:

![handlePost3 diagramatically]({{ "/assets/img/handle-post.png" | relative_url }}){: .center-image }

With all the ceremony out of the way, we're left to write our actual program.
As has been our habit, we'll write ourselves an outline in code we know won't compile.
We want to (1) assert that the request method is POST, (2) assert that the body is non-empty, (3) get the user, (4) get the resource, (5) execute, and (6) return a success response:

{% highlight haskell %}
handlePost1 :: (MonadError Response m, MonadIO m) => Request -> m Response
handlePost1 req@(Request path method body _) = do
  (method /= "POST") `implies` notAllowed method
  null body `implies` noBody
  usr <- getUser req
  src <- getResource req
  execute body usr src
  return $ success path body
{% endhighlight %}

Let's `make test` and see what compiler errors we get.

What, no compiler errors?
All tests pass?

The ease with which we can write `handlePost1` is why we were chasing after monad transformers all along.
`handlePost1` in _Transformers.hs_ ends up being much more straight-forward than `handlePost` from _Eithers.hs_.
At the end of the day, we get to write a simple, idiomatic Haskell `do` block over a monad that combines the semantics of multiple simpler monads.

Before we close shop, let's refactor things a bit.
We'll rename `handlePost1` to `handlePost'` and we'll merge `handlePost3` into `handlePost`.
This time, type inference is on our side.

{% highlight haskell %}
handlePost' :: (MonadError Response m, MonadIO m) => Request -> m Response
handlePost' req@(Request path method body _) = do
  (method /= "POST") `implies` notAllowed method
  null body `implies` noBody
  usr <- getUser req
  src <- getResource req
  execute body usr src
  return $ success path body

handlePost :: Request -> IO Response
handlePost = (either id id <$>) . runExceptT . handlePost'
{% endhighlight %}

# What are the Alternatives to using MTL?

The party-line mantra I kept repeating throughout this post was that we needed a monad that combined the exit-early semantics of `Either e` with the procedural semantics of `IO`.
What does it really mean for a monad to have "the exit-early semantics of `Either e`"?
It means we can use `do ... <- ... ; ...` instead of `if ... then ... else ...`.
That's it.
We're going through all this trouble just to avoid `if` expressions.

While the above rant is a bit of an oversimplification, it's close to being true.
We're doing a non-trivial amount of work to avoid writing lots of nested conditional branching in our functions.
Whether or not it's worth it depends on the scope of your application, and how much program-logic functions (e.g.`getResource`, `execute`, and `handlePost`) stand to be simplified compared to how much extra incidental complexity you'll be generating.
In my view, [_Transformers.hs_][7] compares favorably to [_Eithers.hs_][6], though I haven't had a chance to compare it with a simpler, `if`-expression-based approach.

The MTL classes `MonadError e` and `MonadIO` allow us to keep our effects separate.
Notice `getUser` does not need a `MonadIO` constraint.
If we give up on keeping this separation, we can simplify our code by having every function return a concrete `ExceptT Response IO a`.
If we do that, however, it becomes anyone's guess which functions are actually doing I/O and which functions aren't doing I/O and have simply been pushed into having the most general signature.
Maybe that separation is important to us, maybe it's not.

Taking this approach to its extreme, we can write our application in continuation-passing style over `IO Response`, where any function can do anything at all.
For example, functions that return `IO (Either e a)` would be refactored to return `(a -> IO e) -> IO e`.
The down side of this is that function signatures no longer give you fine-grained information.
The up side is that the code ends up being very concise.
Take a look at [_Continuations.hs_][5] in the project repo if you're interested in seeing such an approach.

We want to be able to reuse code: that's why we're going to all this trouble.
But ultimately, the ability to reuse code comes from making _fewer assumptions_ about the code that calls yours.
This is exactly why we don't throw exceptions.
Throwing an exception is a tacit assumption that the caller wants to handle errors by killing the program right then and there.
Maybe it's a valid assumption, maybe it's not, but the assumption itself limits the places where our code can be reused.
In the same way, choosing MTL-style or continuation-passing-style for your functions' signatures is an assumption about how the caller wants to consumer your code.
Maybe it's the right assumption for all of your use cases, or maybe you're just introducing unnecessary constraints.
It's a problem that deserves some thought when we choose our designs.

Thank you for your attention throughout this series of posts, wherein I have tried to convince you not to throw exceptions [[Part 1][1]].
I've also showed you that working with encapsulated I/O is not as painful as it might sound, and that Haskell has lots of nice tools that help you wrangle your types (e.g. `traverse`, `sequence`, and friends) [[Part 2][2]].
Finally, I've tried to demonstrate how to cope with MTL and monad transformers, why you would want to, and why you might not want to.
I hope this has been mildly entertaining and mostly instructional.

If you're in Southern California and you'd like me to adapt this post into a presentation for your Haskell or Scala meetup, or if you'd like me to adapt it into a workshop for your business, please email me ([danielbrice@gmail.com](mailto:danielbrice@gmail.com)) or send me a DM on Twitter ([@_fried_brice_](http://twitter.com/_fried_brice_)).

  [1]: /blog/three-models-of-exception-handling/
  [2]: /blog/exception-handling-with-explicit-io/
  [3]: http://two-wrongs.com/a-gentle-introduction-to-monad-transformers
  [4]: http://github.com/friedbrice/exception-control-flow
  [5]: http://github.com/friedbrice/exception-control-flow/blob/master/haskell/Continuations.hs
  [6]: http://github.com/friedbrice/exception-control-flow/blob/master/haskell/Eithers.hs
  [7]: http://github.com/friedbrice/exception-control-flow/blob/master/haskell/Transformers.hs
  [8]: http://github.com/friedbrice/exception-control-flow/blob/master/haskell/Spec.hs
