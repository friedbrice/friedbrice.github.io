---
layout: post
title: "Three Models of Exception Handling"
date: 2017-12-28
permalink: /blog/three-models-of-exception-handling
comments: true
tags:
- scala
- exception handling
- continuation
- monad
---

Your hotshot team member just pulled a miracle and, over the weekend, built the API our customers have been promised for the passed year, averting an existential crisis for the company. Works like a charm, except of course when it doesn't. Now, it's up to us to productionalize the prototype by adding appropriate error handling.

<!--break-->

This post illustrates three different models of exception handling we may employ:

-   `throw`ing and `catch`ing `Exception`s,
-   using continuation-passing style to return early, and
-   monadic composition over `Either` values.

The code examples are in Scala, but the principles should easily translate to any language that has first-order functions and parametric polymorphism. If your favorite language does not support Java-style exception handling, with a dedicated `Exception` class and specialized keywords like `throw` and `catch`, we'll see how to implement exception-handling using user-defined classes and functions.

## An Example Problem

We are to implement an API over HTTP which, for simplicity's sake, only accepts POST requests. Users specify a resource in the path of their request, provide an auth token in their header, and the body of their request gets posted to the resource (which probably involves writing to a database, but we leave the precise meaning undefined).

Our domain classes consist of `User`s and `Resource`s. Conceptually, a `User` is identified by their auth token, and a `Resource` is resolved by its path:

{% highlight scala %}
trait User { def token: String }
trait Resource { def path: String }

def getUser(req: Request): User = ???
def getResource(req: Request): Resource = ???
{% endhighlight %}

Next, we need to apply the request body to the resource as the user:

{% highlight scala %}
def execute(content: String, user: User, resource: Resource): Unit = ???
{% endhighlight %}

Of course, we still need to return an appropriate HTTP response even when we cannot complete all of those tasks. The current prototype returns a response only when all the right conditions are met, and halts otherwise.

Here is the project spec, complete with nine canned responses that cover the various things that might go wrong:

{% highlight scala %}
object Spec {

  trait User { def token: String }
  trait Resource { def path: String }

  case class Request(
    path: String,
    method: String,
    body: String,
    header: Map[String, String]
  )

  case class Response(code: Int, content: String)

  def success(path: String, body: String): Response =
    Response(200, s"Successfully posted $body to $path")

  val noBody: Response =
    Response(400, "You must provide a request body")

  val noToken: Response =
    Response(401, "You must provide an authorization header field")

  def malformedToken(token: String): Response =
    Response(401, s"Provided token is malformed: $token")

  def noUser(token: String): Response =
    Response(401, s"No user found for token: $token")

  def notPermitted(path: String): Response =
    Response(403, s"You do not have permission on $path")

  def noResource(path: String): Response =
    Response(404, s"No resource found for path: $path")

  def notAllowed(method: String): Response =
    Response(405, s"Method not allowed: $method")

  val badConnection: Response =
    Response(503, "Connection error, please try again later")
}
{% endhighlight %}

And here we have the prototype, which returns `success` when everything goes right, and halts when anything goes wrong:

{% highlight scala %}
object Prototype {

  // crashes computer if no token, token malformed, or no associated user
  def getUser(req: Request): User = {
    `the user`
  }

  // crashes computer if resource not found
  def getResource(req: Request): Resource = {
    `the resource`
  }

  // crashes computer if user does not have permission or bad connection
  def execute(content: String, u: User, r: Resource): Unit = {
    `is executed?`
  }

  // crashes computer if method is not POST, body is empty,
  // or for any of the reasons the above methods might
  def handlePost(req: Request): Response = {
    if (req.method != "POST") `halt and catch fire`
    if (req.body.isEmpty) `halt and catch fire`
    val usr = getUser(req)
    val src = getResource(req)
    execute(req.body, usr, src)
    success(req.path, req.body)
  }
}
{% endhighlight %}

(Above and in what follows, identifiers surrounded by `` ` `` denote code blocks that we could, in principal, implement, but that we are leaving as undefined for the purposes of this post. [Crazily enough, this is valid Scala syntax.])

## What Exactly Counts as "Exceptional"

Examining the prototype's `handlePost` method, we see the outline of a simple data-processing pipeline:

1.  Check that the method is POST,
2.  Check that the body is not empty,
3.  Get the appropriate `User`,
4.  Grab the appropriate `Resource`,
5.  Execute, using the `Request` body, the `User` and the `Resource`, and
6.  Return `success`.

We use the word "pipeline" intentionally, because subsequent steps rely on the successful completion of prior steps. When one step fails, in order to avoid undefined behavior or crashing the computer, we need to short-circuit processing, escape from the pipeline, and jump to designated error-handling code. That is why we call failures _exceptions._ They are _exceptions to the intended processing pipeline,_ and they require us to skip the _remainder of the computation._

This is a common problem in control flow, and the idiomatic Java way to solve this problem is by `throw`ing `Exception`s and `catch`ing them later. The `Exception` class and the keywords `throw` and `catch` do exactly the kind of short circuiting and redirecting we need.

## Using Exceptions

Let's refactor the prototype's `getUser` method to throw an `Exception` instead of crashing the computer:

{% highlight scala %}
case class NoTokenProvided() extends Exception
case class MalformedToken(token: String) extends Exception
case class NoUserFound(token: String) extends Exception

@throws[NoTokenProvided]
@throws[MalformedToken]
@throws[NoUserFound]
def getUser(req: Request): User = {
  val token = req.header.get("Authorization")
  if (token.isEmpty) throw NoTokenProvided()
  if (`malformed token?`) throw MalformedToken(token.get)
  if (`user not found?`) throw NoUserFound(token.get)
  `the user`
}
{% endhighlight %}

Before we write the method, we enumerate the kinds of failures we may have as case classes. Above the method signature, we replace our earlier human-readable comments with compiler-readable `@throw` annotations. Instead of crashing the computer, we throw, which allows us to catch and dispatch these failures later, but uncaught exceptions will still crash the program.

Halting problem aside, there is presumably some way to determine if a token is malformed or whether a user with a given token exists without crashing the computer, so we encode those as `Boolean`s and use them to decide if we need to exit early. For example:

{% highlight scala %}
if (`user not found?`) throw NoUserFound(token.get)
{% endhighlight %}

We'll need to refactor `handlePost` to catch the exceptions thrown by `getUser`. We wrap the method body in a `try` block and add a `catch` block after:

{% highlight scala %}
def handlePost(req: Request): Response = try {
  ...
  val user = getUser(req)
  ...
  success(req.path, req.body)
} catch {
  case NoTokenProvided() => noToken
  case MalformedToken(token) => malformedToken(token)
  case NoUserFound(token) => noUser(token)
}
{% endhighlight %}

In each case, we simply return the appropriate canned response. Notice `handlePost` does not throw exceptions (at least, it doesn't have any `@throw` annotations). Our helper methods can throw exceptions, but we want to guarantee that `handlePost` returns a `Response`, so we need to catch every possible exception that we're aware of.

Let's see what the whole pipeline looks like, refactored using `Exception`s:

{% highlight scala %}
import Spec._
import Undefined._

object Exceptions {

  case class NoTokenProvided() extends Exception
  case class MalformedToken(token: String) extends Exception
  case class NoUserFound(token: String) extends Exception

  @throws[NoTokenProvided]
  @throws[MalformedToken]
  @throws[NoUserFound]
  def getUser(req: Request): User = {
    val token = req.header.get("Authorization")
    if (token.isEmpty) throw NoTokenProvided()
    if (`malformed token?`) throw MalformedToken(token.get)
    if (`user not found?`) throw NoUserFound(token.get)
    `the user`
  }

  case class NoResourceFound(path: String) extends Exception

  @throws[NoResourceFound]
  def getResource(req: Request): Resource = {
    lazy val path: String = req.path
    if (`resource not found?`) throw NoResourceFound(path)
    `the resource`
  }

  case class NotPermitted(path: String) extends Exception
  case class BadConnection() extends Exception

  @throws[NotPermitted]
  @throws[BadConnection]
  def execute(content: String, usr: User, src: Resource): Unit = {
    if (! `is permitted?`) throw NotPermitted(src.path)
    if (! `is executed?`) throw BadConnection()
  }

  case class MethodNotAllowed(method: String) extends Exception
  case class NoBodyProvided() extends Exception

  @throws[MethodNotAllowed]
  @throws[NoBodyProvided]
  def checkPreconditions(req: Request): Unit = {
    if (req.method != "POST") throw MethodNotAllowed(req.method)
    if (req.body.isEmpty) throw NoBodyProvided()
  }

  def handlePost(req: Request): Response = try {
    checkPreconditions(req)
    val user = getUser(req)
    val resource = getResource(req)
    execute(req.body, user, resource)
    success(req.path, req.body)
  } catch {
    case MethodNotAllowed(method) => notAllowed(method)
    case NoBodyProvided() => noBody
    case NoTokenProvided() => noToken
    case MalformedToken(token) => malformedToken(token)
    case NoUserFound(token) => noUser(token)
    case NoResourceFound(path) => noResource(path)
    case NotPermitted(path) => notPermitted(path)
    case BadConnection() => badConnection
  }
}
{% endhighlight %}

## Analysis of Using Exceptions

This is safer, but our code exploded: it's over twice the size of the prototype. This might not sound like a huge problem, but bear in mind that in a production environment, every line of code is an ongoing maintenance burden, so it really pays in the long run to keep your code clean.

In addition to being verbose, the helper methods are somewhat unusable except in this context, because in order to reuse them, one must anticipate that they will throw and catch appropriately. For example, while we've been extra careful and made sure that our use point, `handlePost` catches everything that might get thrown, we might anticipate a team member extending this API to handle GET and DELETE requests. They'd probably want to reuse some our helper methods, but they'll have to anticipate these throws, and they'll get no help from the compiler as they do.

So, the code is now verbose and the helper methods are unsafe. Let's try to figure out why that is.

First of all, the helper methods are unsafe precisely because we throw, and thrown exceptions are unchecked in Scala. (Thrown exceptions might as well be unchecked in Java, in light of all the ways people have learned to fool the compiler.) If we want compile-time static checking that our methods won't crash the computer when they are reused, we can't be throwing exceptions.

Second, the code explosion is mostly due to repeating ourselves. Each individual error type gets represented five separate times in our program:

1.  Once when we create a case class extending `Exception`,
2.  Once when we annotate our method,
3.  Once at the site where we encounter the error and throw,
4.  Once when we catch the `Exception`, and
5.  Once when we map the exception to the appropriate canned response.

What we really want with this program is to ensure that an appropriate `Response` is created and returned. If we can find a way to do that at the point of failure, then we can cut out the middle-step that involves throwing and catching.

## Using Continuations

Remember above, we said we use `throw` to skip the remainder of the computation and `catch` to jump to some failure-handling code. If we pass the remainder of the computation into each method as an argument, then we can skip it if we need to using simple conditional logic.

Actually doing this is a lot more straightforward than it sounds. Imagine we have a computational pipeline where the end result is a value of some type `C`. Suppose we have some method that is part of our pipeline that takes a value of some type `A` and returns a value of some type `B`. Here's a template for how to refactor that method:

{% highlight scala %}
// some method
def someMethod(a: A): B = {
  val b = `dat B`
  b
}

// same method in continuation-passing style
def someMethod(a: A)(cont: B => C): C = {
  val b = `dat B`
  cont(b)
}
{% endhighlight %}

Instead of returning a `B`, we accept a function that accepts a `B` and continues the computation, resulting in a `C` that our method will then return. It's called _continuation-passing style_ because we pass in a _continuation_, a function that represents the rest of the computational pipeline.

If you've done any Node.js programming, you've used and maybe even written functions that have this shape. If you'd like to get a feel for using and writing functions in this style, I recommend you take a few hours one afternoon and complete the exercises in [Learn You The Node.js](https://github.com/workshopper/learnyounode).

Now, the template we have doesn't show how to deal with failures. Here's a slightly beefier template:

{% highlight scala %}
// exception-passing style
class SomeFailure() extends Exception

def handleSomeFailure(err: SomeFailure): C = {
  `appropriate C for this error`
}

@throws[SomeFailure]
def subroutine(a: A): B = {
  if (`bad stuff`) throw SomeFailure
  val b = `dat B`
  b
}

def pipeline(): C = try {
  val a = `get an A`
  val b = subroutine(a)
  val c = `process a B into a C`
  c
} catch {
  case err: SomeFailure => handleSomeFailure(err)
}

// continuation-passing style
def subroutine(a: A)(cont: B => C): C = {
  if (`bad stuff`) `appropriate C for this error` else
  val b = `dat B`
  cont(b)
}

def pipeline(a: A): C = {
  val a = `get an A`
  subroutine(a) { b =>
  `process a B into a C` }
}
{% endhighlight %}

While it might look somewhat roundabout, the advantages of using continuation-passing style here are that the methods are now compile-time safe (making it easier to reuse without mistake) and shorter (creating less of a maintenance burden) with fewer top-level abstractions (resulting in less cognitive overhead).

The key to understanding code like this is to think of `subroutine` as providing a `B`. In `pipeline`, we call `subroutine`, which produces a `B` that we then name and then go on to process. Try to think of it as an assignment where the name is on the right instead of the left. Something like "`b` gets `subroutine(a)`, and..."

Here's the program, refactored to use continuation-passing style for exception handling instead of the JVM's `Exception` class and `throw` and `catch` keywords:

{% highlight scala %}
import Spec._
import Undefined._

object Continuations {

  def getUser(req: Request)(cont: User => Response): Response = {
    val token = req.header.get("Authorization")
    if (token.isEmpty) noToken else
    if (`malformed token?`) malformedToken(token.get) else
    if (`user not found?`) noUser(token.get) else
    cont(`the user`)
  }

  def getResource(req: Request)(cont: Resource => Response): Response = {
    val path: String = req.path
    if (`resource not found?`) noResource(path) else
    cont(`the resource`)
  }

  def execute(content: String, usr: User, src: Resource)
             (cont: Unit => Response): Response = {
    if (!`is permitted?`) notPermitted(src.path) else
    if (!`is executed?`) badConnection else
    cont(())
  }

  def handlePost(req: Request): Response = {
    if (req.method != "POST") notAllowed(req.method) else
    if (req.body.isEmpty) noBody else
    getUser(req) { usr =>
    getResource(req) { src =>
    execute(req.body, usr, src) { _ =>
    success(req.path, req.body) } } }
  }
}
{% endhighlight %}

We see the code is much shorter, mostly because we no longer need to repeat ourselves so much, and we have the additional benefit of stronger compile-time guarantees that our code is not broken. Also, we handle failures at the point of failure, instead of waiting until the end, which I think in this case is a benefit but does admittedly lead to tighter coupling. (We could regain flexibility by refactoring our methods so that the caller supplies `Response` values to use for the various failure cases. _e.g._ Give `getUser` three additional `Response` arguments.)

## Using Eithers

## Analysis of Continuations and Eithers