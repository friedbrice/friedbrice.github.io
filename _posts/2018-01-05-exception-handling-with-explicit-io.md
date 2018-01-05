---
layout: post
title: "Exception Handling with Explicit IO"
date: 2018-01-05
permalink: /blog/exception-handling-with-explicit-io/
redirect_from: 
  - /blog/18/
  - /blog/2018-01-05/
comments: true
tags:
  - code
  - haskell
  - exception handling
  - error handling
  - failure handling
  - effects
  - monad
  - types
---

In a [previous post](/blog/three-models-of-exception-handling), we added exception handling to a hypothetical API server written in Scala. Since Scala implicitly allows arbitrary I/O anywhere, modeling exception handling by adding a layer of indirection using `Either` values was essentially free. I wanted to try implementing the same logic in Haskell, which would force us to deal with intermingling exception handling intermingled with explicit I/O, creating two layers of abstraction that we'll have to juggle.

<!--break-->

# The Problem

Let's do a quick recap of the problem. We are implementing an HTTP API that accepts only POST requests. We have simple `Request` and `Response` types.

{% highlight haskell %}
data Request = Request
  { path :: String
  , method :: String
  , body :: String
  , header :: [(String, String)]
  }

data Response = Response { code :: Int, content :: String } deriving Show
{% endhighlight %}

Our domain types are `User`s and `Resource`s. We assume that this app is responsible for auth, so we may create a `User` value from the `Request` auth token without any I/O. However, we assume that resources are shared between this app and others, so we need to do I/O to get a handle on a `Resource`.

{% highlight haskell %}
newtype User = User { getToken :: String }
newtype Resource = Resource { getPath :: String }

getUser :: Request -> User
getResource :: Request -> IO Resource
{% endhighlight %}

Once we have the `User`, `Resource`, and request body, we execute some database mutation (left undefined) using those three parameters.

{% highlight haskell %}
execute :: String -> User -> Resource -> IO ()
{% endhighlight %}

`handlePost :: Request -> Response` is our entry point. The whole program can be summarized as follows:

1.  Make sure the request method is POST.
2.  Make sure the request body is not empty.
3.  Get the user.
    -   Make sure the "Authorization" header is present.
    -   Make sure the auth token is valid.
    -   Make sure there is a user associated to the auth token.
4.  Get the resource.
    -   Make sure the resource at the request path exists.
5.  Execute the request.
    -   Make sure the user has permission to modify the resource.
    -   Make sure the modification was successfully applied.
6.  Return an appropriate response.

Each _Make sure ..._ step represents a failure where we may need to exit early, returning an appropriate response. Here is the project spec, with nine canned responses:

{% highlight haskell %}
module Spec where

newtype User = User { getToken :: String }
newtype Resource = Resource { getPath :: String }

data Request = Request
  { path :: String
  , method :: String
  , body :: String
  , header :: [(String, String)]
  }

data Response = Response { code :: Int, content :: String } deriving Show

success path body = Response 200 $
  "Successfully posted " ++ body ++ " to " ++ path

noBody = Response 400 $
  "You must provide a non-empty request body"

noToken = Response 401 $
  "You must provide an authorization header field"

malformedToken token = Response 401 $
  "Provided token is malformed: " ++ token

noUser token = Response 401 $
  "No user found for token: " ++ token

notPermitted path = Response 403 $
  "You do not have permission to post on " ++ path

noResource path = Response 404 $
  "No resource found for path: " ++ path

notAllowed method = Response 405 $
  "Method not allowed: " ++ method

badConnection = Response 503
  "Connection error, please try again later"
{% endhighlight %}

Next, we'll take a look at our undefined terms,

{% highlight haskell %}
module Undefined where

import Spec

is_malformed_token = undefined :: Bool
is_user_not_found = undefined :: Bool
the_user = undefined :: User
is_resource_not_found_io = undefined :: IO Bool
the_resource_io = undefined :: IO Resource
is_permitted_io = undefined :: IO Bool
is_executed_io = undefined :: IO Bool
{% endhighlight %}

Terms that yield `Bool`s are prefixed by _is_ and terms that do I/O are suffixed by _io_. Finally, (and just because we can) we'll make a mass-import module,

{% highlight haskell %}
module Project (module Spec, module Undefined) where

import Spec
import Undefined
{% endhighlight %}

# Implementation

**Implementing `getUser`**

We'll augment `getUser` with error handling, so its signature will be `Request -> Either Response User`, and as we do, the effect `implies :: Bool -> Response -> Either Response Unit` will come in handy.

{% highlight haskell %}
import Project

failure `implies` fallback = if failure then Left fallback else Right ()

getUser :: Request -> Either Response User
getUser (Request _ _ _ header) = do
  token <- maybe (Left noToken) Right $ lookup "Authorization" header
  is_malformed_token `implies` malformedToken token
  is_user_not_found `implies` noUser token
  return the_user
{% endhighlight %}

Easy peasy. `getResource` will be a somewhat more challenging.

**First Attempt at `getResource`**

First note the return type,

{% highlight haskell %}
getResource :: Request -> IO (Either Response Resource)
{% endhighlight %}

We cannot return an `Either Response (IO Resource)`, because that would mean we decide if we should exit early without doing any I/O, and then return an `IO` value if we don't exit early. In fact, since `is_resource_not_found_io` has type `IO Bool`, we need to do I/O in order to construct our `Either` value, so the appropriate return type is `IO (Either Response Resource)`.

{% highlight haskell %}
getResource :: Request -> IO (Either Response Resource)
getResource (Request path _ _ _) = do
  is_resource_not_found_io `implies` noResource path
  the_resource_io
{% endhighlight %}

The above method looks concise and clear. Unfortunately, it doesn't compile. `implies` expects a `Bool`, but we have an `IO Bool` to work with.

**Second Attempt at `getResource`**

{% highlight haskell %}
implies' :: IO Bool -> Response -> IO (Either Response ())
failure_io `implies'` fallback = (`implies` fallback) <$> failure_io

getResource :: Request -> IO (Either Response Resource)
getResource (Request path _ _ _) = do
  is_resource_not_found_io `implies'` noResource path
  return <$> the_resource_io
{% endhighlight %}

We add an `implies'` which is something like `implies` but is compatible with I/O. The `return` on the last line maps over the `IO` in order to lift the `Resource` to an `Either Response Resource`. This code compiles, but it does not do what we want it to do.

To see why, suppose `is_resource_not_found_io` comes back false. Then `implies'` returns an `IO (Either Response ())`, but there is no associated bind assignment (e.g., no `<-`), so the data that the `Either Response ()` represents is completely ignored, and the next line that creates a handle to the nonexistent resource gets executed anyway, potentially leading to data corruption and other undefined behavior. But doesn't `do` notation handle the short circuit logic for us? `do` notation for `Either` does, but notice that this `do` block is for `IO`. It's completely indeferent to whether the result of `implies'` is a `Left` value or a `Right` value, proceeding to the final line either way. This is harder than I thought it'd be.

**Third Attempt at `getResource`**

We need to find a way to conditionally call `the_resource_io`. We can do this by introducing a local assignment of type `() -> IO Resource` which, when called, calls `the_resource_io`.

{% highlight haskell %}
getResource :: Request -> IO (Either Response Resource)
getResource (Request path _ _ _) = do
  let doResource = (\_ -> the_resource_io) :: () -> IO Resource
  notFound <- is_resource_not_found_io `implies'` noResource path
  doResource `traverse` notFound
{% endhighlight %}

The result of `implies'` is bound to `notFound`, shedding the `IO` and leaving an `Either Response ()`. Conceptually, we want to apply `doResource` to `notFound`, preserving the short-circuit logic provided by `Either`. To accomplish this, we use the standard library function `traverse` (definition below, with type signature specialized to our use case).

{% highlight haskell %}
traverse :: (a -> IO b) -> Either e a -> IO (Either e b)
f `traverse` (Left e) = return (Left e) -- ignores f, lifting `Left e`
f `traverse` (Right a) = Right <$> f a -- applies f, then maps `Right`
{% endhighlight %}

Notice in the `Left` case, we ignore `f` and return a lifted `Left` value, preserving our desired short-circuit semantics. Armed with `traverse`, our `getResource` function finally mixes I/O and failure handling correctly.

I should note that `traverse` works whenever you replace `IO` and `Either e` with an arbitrary `Applicative` and `Traversable`, respectively.

**Implementing `execute`**

We use what we learned from `getResource` when writing `execute`. Since we don't want to write to the resource if the user doesn't have permission, we want to apply `is_executed_io` conditionally. We create a temporary `() -> IO Bool` to wrap it. Our `do` block will be over `IO`, so we will need to bind some of our `Either` values using `>>=` explicitly.

{% highlight haskell %}
execute :: String -> User -> Resource -> IO (Either Response ())
execute body user (Resource path) = do
  let doExecuted = (\_ -> is_executed_io) :: () -> IO Bool
  permitted <- (not <$> is_permitted_io) `implies'` notPermitted path
  executed <- doExecuted `traverse` permitted
  return ((not <$> executed) >>= (`implies` badConnection))
{% endhighlight %}

`permitted` has type `Either Response ()`. We use `traverse` to conditionally apply `doExecute` and assign the result (of type `Either Response Bool`) to `executed`. We want to finish with ``not executed `implies` badConnection``, but we need to do some type Tetris to make everything fit. `not <$> executed` has type `Either Response Bool`. We use `>>=` to bind its `Bool` result to ``(`implies` badConnection)``, resulting in an `Either Response ()`. Finally, `return` lifts us to `IO (Either Response ())`, as desired.

**Implementing `handlePost`**

As we implement `handlePost`, we'll make use of the following helper function:

{% highlight haskell %}
import Control.Monad (join)

...

tunnel :: Either e (IO (Either e a)) -> IO (Either e a)
tunnel eitherIoEither = join <$> sequenceA eitherIoEither
{% endhighlight %}

`tunnel` commutes the outer `Either` with the `IO` and then maps over the `IO` to combine the two `Either`s inside.

We'll want to conditionally apply `getResource` and `execute` using the same conditional-execution trick we used in their definitions, so we'll add a `()` argument (that we simply ignore) to each.

{% highlight haskell %}
getResource :: () -> Request -> IO (Either Response Resource)
getResource _ (Request path _ _ _) = do ...

execute :: () -> String -> User -> Resource -> IO (Either Response ())
execute _ body user (Resource path) = do ...
{% endhighlight %}

`handlePost` needs to take a `Request` to an `IO Response`, completely eliminating any `Either` context. Let's take a look at `handlePost` and then discuss.

{% highlight haskell %}
handlePost :: Request -> IO Response
handlePost req@(Request path method body _) = either id id <$> result
  where
    chkMth = (method /= "POST") `implies` notAllowed method
    errBdy = null body `implies` noBody >> return body
    errUsr = getUser req
    precon = chkMth >> errBdy >> errUsr >> return ()
    result = do
      errSrc <- tunnel (getResource <$> precon <*> pure req)
      errExe <- tunnel (execute <$> precon <*> errBdy <*> errUsr <*> errSrc)
      return (errExe >> return (success path body))
{% endhighlight %}

We begin by checking and collecting our preconditions. `chkMth` has type `Either Response ()`, `errBdy` has type `Either Response String`, `errUsr` has type `Either Response User`, and `precon` has type `Either Response ()`. We use `precon` to conditionally apply `getResource` and `execute`, and we use `tunnel` to remove nesting.  `errExe` has type `Either Response ()` and gates the final `success` response. Finally, `result` has type `IO (Either Response Response)`, over which we map `either id id` to produce the desired `IO Response`, completing the program.

# Parting Thoughts

This is hard work. I've heard the saying "monads give you one free abstraction," and the meaning of that saying really hit home while I was doing this exercise. Once we had two monads in play, the code became significantly more complicated. Fortunately, we have some help. The heavy lifters are `fmap` (via `<$>`), the applicative `<*>`, `>>` and `>>=`, and `traverse`. In general, I find that when I'm stuck in a quagmire of types, `traverse` (and it's cousins, `for` and `sequenceA`) are exactly what I need to dig my way out. Always keep them in mind.

I made extensive use of standard Haskell tools, such as [Hoogle](https://www.haskell.org/hoogle/) and the REPL. I found that it helps to pay close attention to compiler errors.

Another crutch I lean heavily on is scratch paper and pencil (not pen). For every function here except for `getUser`, I had to break out the old-fashioned notepad and write some type signatures out. Here I am trying to figure out the implementation of `execute`.

![Me, trying to figure out `execute`]({{ "/assets/img/exception-handling-with-explicit-io-1.jpg" | relative_url }})

Notice I write out what I _have_ and what I _want_. If you are a Haskell nomad, I highly recommend you develop your own paper/pencil crutch. Don't underestimate the insight you can gain from changing your vantage point this way. Specifically, paper and pencil helps me with equational reasoning (a kind of value-level reasoning) and type unification (a kind of type-level reasoning). I should probably write posts about those one of these days.

Most of our woes are caused by using `do` notation for `IO` when what we really want is `do` notation for `Either`. One way to avoid these woes is to use explicit `>>=` for our `IO`, reserving `do` notation for our conditional logic. I doubt that this would simplify the code significantly. A different approach we could try is using _monad transformers_ to create a composite monad that combines the sequential execution semantics of `IO` with the short-circuit semantics of `Either`. I've been meaning to learn how to use monad transformers, so if I get motivated enough I'll write up a post reimplementing this program.

# Appendix

Here are working code examples. With all of these files in the same directory and GHC installed, you can run the tests with `runghc Test.hs` and open individual files in GHCi.

**Spec.hs**

{% highlight haskell %}
module Spec where

newtype User = User { getToken :: String }
newtype Resource = Resource { getPath :: String }

data Request = Request
  { path :: String
  , method :: String
  , body :: String
  , header :: [(String, String)]
  }

data Response = Response { code :: Int, content :: String } deriving Show

success path body = Response 200 $
  "Successfully posted " ++ body ++ " to " ++ path

noBody = Response 400 $
  "You must provide a non-empty request body"

noToken = Response 401 $
  "You must provide an authorization header field"

malformedToken token = Response 401 $
  "Provided token is malformed: " ++ token

noUser token = Response 401 $
  "No user found for token: " ++ token

notPermitted path = Response 403 $
  "You do not have permission to post on " ++ path

noResource path = Response 404 $
  "No resource found for path: " ++ path

notAllowed method = Response 405 $
  "Method not allowed: " ++ method

badConnection = Response 503
  "Connection error, please try again later"
{% endhighlight %}

**Undefined.hs**

{% highlight haskell %}
module Undefined where

import Spec

is_malformed_token = undefined :: Bool
is_user_not_found = undefined :: Bool
the_user = undefined :: User
is_resource_not_found_io = undefined :: IO Bool
the_resource_io = undefined :: IO Resource
is_permitted_io = undefined :: IO Bool
is_executed_io = undefined :: IO Bool
{% endhighlight %}

**Project.hs**

{% highlight haskell %}
module Project (module Spec, module Undefined) where

import Spec
import Undefined
{% endhighlight %}

**Eithers.hs**

{% highlight haskell %}
module Eithers where

import Project
import Control.Monad (join)

failure `implies` fallback = if failure then Left fallback else Right ()
failures `implies'` fallback = (`implies` fallback) <$> failures

getUser :: Request -> Either Response User
getUser (Request _ _ _ header) = do
  token <- maybe (Left noToken) Right $ lookup "Authorization" header
  is_malformed_token `implies` malformedToken token
  is_user_not_found `implies` noUser token
  return the_user

getResource :: () -> Request -> IO (Either Response Resource)
getResource method (Request path _ _ _) = do
  let doResource = (\_ -> the_resource_io) :: () -> IO Resource
  notFound <- is_resource_not_found_io `implies'` noResource path
  doResource `traverse` notFound

execute :: () -> String -> User -> Resource -> IO (Either Response ())
execute method body usr (Resource path) = do
  let doExecuted = (\_ -> is_executed_io) :: () -> IO Bool
  permitted <- (not <$> is_permitted_io) `implies'` notPermitted path
  executed <- doExecuted `traverse` permitted
  return ((not <$> executed) >>= (`implies` badConnection))

handlePost :: Request -> IO Response
handlePost req@(Request path method body _) = either id id <$> result
  where
    tunnel eitherIoEither = join <$> sequenceA eitherIoEither
    chkMth = (method /= "POST") `implies` notAllowed method
    errBdy = null body `implies` noBody >> return body
    errUsr = getUser req
    precon = chkMth >> errBdy >> errUsr >> return ()
    result = do
      errSrc <- tunnel (getResource <$> precon <*> pure req)
      errExe <- tunnel (execute <$> precon <*> errBdy <*> errUsr <*> errSrc)
      return (errExe >> return (success path body))
{% endhighlight %}

**Test.hs**

{% highlight haskell %}
module Test where

import Project
import Eithers

request1 = Request "path" "POST" "" [("Authorization", "hunter2")]
request2 = Request "path" "POST" "body" [("Nope", "nada")]
request3 = Request "path" "FOO" "body" [("Authorization", "hunter2")]

main :: IO ()
main = do

  putStrLn ""

  putStrLn "Testing Eithers:"
  eResponse1 <- handlePost request1
  putStrLn $ "    " ++ show eResponse1
  eResponse2 <- handlePost request2
  putStrLn $ "    " ++ show eResponse2
  cResponse3 <- handlePost request3
  putStrLn $ "    " ++ show cResponse3
  putStrLn "Passed."

  putStrLn ""
{% endhighlight %}
