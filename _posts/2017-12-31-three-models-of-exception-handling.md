---
layout: post
title: "Three Models of Exception Handling"
date: 2017-12-31
permalink: /blog/2017-12-31/
redirect_from: 
  - /blog/17/
  - /blog/three-models-of-exception-handling/
comments: true
tags:
  - code
  - scala
  - exception handling
  - error handling
  - failure handling
  - effects
  - continuation
  - monad
  - types
  - style
---

A hotshot teammate just pulled a miracle. Over the weekend, they built the API our customers have been promised for the past year, averting an existential crisis for the company. One problem: Only the happy path returns a response; all exceptional circumstances simply crash the computer. So, it's up to us to productionalize the prototype by adding appropriate error handling, and we won't sacrifice compile-time safety and re-usability along the way.

<!--break-->

This post illustrates three different models of error handling:

-   `throw`ing and `catch`ing `Exception`s,
-   using continuation-passing style to return early, and
-   monadic composition of `Either` values.

The terms _error_, _failure_, and (lower-case) _exception_ will be used interchangeably throughout this post. (While a distinction can be useful, we will not need one here.) Capital _Exception_ will refer properly to language-specific notions, such as Java's dedicated `Exception` class, where needed.

The code examples are in Scala, but the patterns should translate to most other languages that have first-order functions and generic type parameters. If your favorite language does not support Java-style exception handling---with a dedicated `Exception` class and specialized keywords like `throw` and `catch`---we'll see how to implement exception handling as user-defined types and functions in a type-safe way.

You can find the summary [refactor template](https://gist.github.com/friedbrice/066c81db89a29826219321efd522febd) and working [code examples](https://gist.github.com/friedbrice/b08d5e5660a824e3f912d4570cdf7f8c) accompanying this post on Gist. Test.scala can be run in an IDE or in the Scala REPL.

# An Example Problem

We are to implement an API over HTTP which, for simplicity's sake, only accepts POST requests. Users specify a resource in the path of their request, provide an auth token in their header, and the body of their request gets posted to the resource (which probably involves writing to a database, but we'll leave the precise meaning undefined).

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

Here's the project spec, complete with nine canned responses that cover the various things that might go wrong. Take a moment to read over the canned responses to get an idea of the kinds of failures we have to handle:

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

(Above and in what follows, identifiers surrounded by `` ` `` denote code blocks that we could implement in principle, but will leave undefined for the purposes of this post.)

# What Exactly Counts as "Exceptional"

Examining the prototype's `handlePost` method, we see the outline of a simple data-processing pipeline:

1.  Check that the method is POST,
2.  Check that the body is not empty,
3.  Get the appropriate `User`,
4.  Grab the appropriate `Resource`,
5.  Execute, using the `Request` body, the `User` and the `Resource`, and
6.  Return `success`.

We use the word "pipeline" intentionally, because subsequent steps rely on the successful completion of prior steps. When one step fails, in order to avoid undefined behavior or crashing the computer, we need to short circuit processing, escape from the pipeline, and jump to designated error-handling code. That is why we call such failures exceptions. They represent _exceptions to the intended processing pipeline,_ and they require us to skip the _remainder of the computation._

This is a common problem in control flow, and the idiomatic Java way to solve this problem is by `throw`ing `Exception`s and `catch`ing them later. The `Exception` class and the keywords `try`, `throw`, and `catch` do exactly the kind of short circuiting and redirecting we need.

# Using Exceptions

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

We enumerate the kinds of failures we may have as case classes. Above the method signature, we replace human-readable comments with compiler-readable `@throws` annotations. Instead of crashing the computer, we throw, which allows us to catch later, but uncaught Exceptions will still crash the program.

We'll need to refactor `handlePost` to catch the exceptions thrown by `getUser`. We wrap the method body in a `try` block and add a `catch` block after:

{% highlight scala %}
def handlePost(req: Request): Response = try {
  ...
  val user = getUser(req)
  ...
} catch {
  case NoTokenProvided() => noToken
  case MalformedToken(token) => malformedToken(token)
  case NoUserFound(token) => noUser(token)
}
{% endhighlight %}

In each case, we simply return the appropriate canned response. Notice `handlePost` itself does not throw. Our helper methods can throw Exceptions, but we want to guarantee that `handlePost` returns a `Response`, so we need to catch every possible Exception that we're aware of. The buck stops at `handlePost`.

Let's see what the whole pipeline looks like refactored using `Exception`s:

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

# Analysis of Using Exceptions

Where the prototype incorrectly handled failures, this new refactor is correct, but our code exploded: It's over twice the size of the prototype. This might not sound like a huge problem, but bear in mind that in a production environment, every line of code is an ongoing maintenance burden, so it really pays in the long run to keep your code clean.

In addition to being verbose, the helper methods are somewhat unusable except in this context, because in order to reuse them, one must anticipate that they will throw and catch appropriately. For example, while we've been extra careful and made sure that our call site, `handlePost`, catches everything that might get thrown, we might anticipate a team member extending this API to handle GET and DELETE requests. They'd probably want to reuse some our helper methods, but they'll have to anticipate these throws, and they'll get no help from the compiler when they do.

So, the code is now bloated and the helper methods are unsafe, making them hard to reuse. Let's try to figure out why that is.

First of all, the helper methods are unsafe precisely because we throw, and thrown Exceptions are unchecked in Scala. (Thrown Exceptions might as well be unchecked in Java in light of all the ways people have learned to fool the compiler.) If we want compile-time static checking that our methods won't crash the computer when they are reused, we simply can't be throwing Exceptions.

Second, the code explosion is mostly due to repeating ourselves. Each individual error type gets represented five separate times in our program:

1.  Once when we create a case class extending `Exception`,
2.  Once when we annotate our method,
3.  Once at the site where we encounter the error and throw,
4.  Once when we catch the `Exception`, and
5.  Once when we map the exception to the appropriate canned response.

Our end goal with this program is to ensure that an appropriate `Response` is created and returned. If we can find a way to cancel the remainder of the computation and exit early with the appropriate response at the point of failure, then we can cut out the middle step that involves throwing and catching.

# Using Continuations

We `throw` in order to skip the remainder of the computation and jump to the `catch` block, were we create an appropriate `Response`. If we pass the remainder of the computation into each method as an argument, then we can skip it simply by not calling it.

Doing this is a lot more straightforward than it sounds. Imagine we have a computational pipeline where the end result is a value of some type `C`. Suppose we have some method that is part of our pipeline that takes a value of type `A` and returns a value of type `B`. Here's a template for how to refactor that method:

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

Instead of returning a `B`, we accept a function that eats a `B` and continues the computation, resulting in a `C` that our method will then return. It's called _continuation-passing style_ because we pass in a _continuation_, a function that represents the rest of the computational pipeline.

If you've done any Node.js programming, you've likely used and even written functions that have this shape. If not and you'd like to get a feel for using and writing functions in this style, I recommend you take a few hours one afternoon and complete the exercises in [Learn You The Node.js](https://github.com/workshopper/learnyounode).

The template we have doesn't show how to deal with failures. Here's a slightly fuller template:

{% highlight scala %}
// exception-passing style:
// wherein an Exception gets passed back to the caller
// instead of the thing we promised them (surprise!).

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

// continuation-passing style:
// wherein the caller supplies a continuation at the call site.

def subroutine(a: A)(cont: B => C): C = {
  if (`bad stuff`) `appropriate C for this error` else {
  val b = `dat B`
  cont(b) }
}

def pipeline(a: A): C = {
  val a = `get an A`
  subroutine(a) { b =>
  `process a B into a C` }
}
{% endhighlight %}

While it might look somewhat roundabout, the advantages of using continuation-passing style here are that the methods are now compile-time safe (making it easier to reuse without mistake) and shorter (creating less of a maintenance burden) with fewer top-level abstractions (resulting in less cognitive overhead).

The key to interpreting code like this is to think of `subroutine` as providing a hypothetical `B`. In `pipeline`, we call `subroutine`, which produces a `B` that we then name and go on to process. Try to think of the call to `subroutine` as an assignment where the name is on the right instead of the left. Something like "`b` gets `subroutine(a)`, and then ..."

To get an idea of how to use continuation-passing style, let's refactor `getResource`:

{% highlight scala %}
def getResource(req: Request)(cont: Resource => Response): Response = {
  val path: String = req.path
  if (`resource not found?`) noResource(path) else
  cont(`the resource`)
}
{% endhighlight %}

Instead of returning a `Resource` we accept a continuation and we return a `Response`. Instead of throwing an Exception in case of failure, we ignore the continuation and return `noResource(path)`.

Let's examine the corresponding change in `handlePost`:

{% highlight scala %}
def handlePost(req: Request): Response = {
  ...
  getResource(req) { src =>
  ... }
}
{% endhighlight %}

No more catching Exceptions and mapping them to their corresponding `Response`: `getResource` will return the appropriate `Response` in case it fails. Notice that `handlePost` does not take a continuation for the same reason that its counterpart method in the exception-passing version above does not throw. `handlePost` is the end of the (pipe)line.

Here's the whole program, refactored to use continuation-passing style for exception handling instead of idiomatic-Java Exception throwing:

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
    if (! `is permitted?`) notPermitted(src.path) else
    if (! `is executed?`) badConnection else
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

We see the code is much shorter, mostly because we're not repeating ourselves so much, and we have the additional benefit of stronger compile-time guarantees that our code is not broken, making it easier (in at least the _Correctness_ dimension) to reuse our helper methods when we inevitably extend this API six months from now. Also, we handle failures at the point of failure, instead of some far-off place in our code, which in this case I feel is a benefit but does admittedly lead to tighter coupling. (We could regain flexibility by refactoring our methods so that the caller supplies `Response` values to use for the various failure cases. E.g., pass `noResource(path)` in as an argument to `getResource`.)

# Using Eithers

Writing in continuation-passing style makes it easier to reuse our helper methods in the following sense: We have rigged their signatures so that there's no way for us to forget to handle failures. However, passing around continuations can be a bit awkward, putting an extra burden on us at the call site. In that sense, these helper methods are a little bit harder to reuse. Continuation-passing style happens to be one of the most-versatile tools in a programmer's toolbox. Using them merely for error handling is kind of like swatting a fly with a wrecking ball.

The `Either` class provides an abstraction that is a little more focused in its scope, making it easier to use. The `Either` class provides short-circuit, pass-through semantics much like `throw`/`catch` does, but `Either` has the advantage of being a concrete data structure, representing the control flow as a first-class value.

That was a bit long-winded, so let's take a look at how `Either` achieves short-circuit, pass-through logic. (The implementation is simplified for the purposes of this post.)

{% highlight scala %}
sealed trait Either[E,A] {
  def map[B](f: A => B): Either[E,B] = mapEither(this,f)
  def flatMap[B](f: A => Either[E,B]): Either[E,B] = bindEither(this,f)
}

final case class Left[E,A](leftValue: E) extends Either[E,A]
final case class Right[E,A](rightValue: A) extends Either[E,A]

def mapEither[E,A,B](ea: Either[E,A], f: A => B): Either[E,B] =
  ea match {
    case Left(e) => Left(e) // if left, ignore f and pass through
    case Right(a) => Right(f(a)) // if right, apply f and wrap result
  }

def bindEither[E,A,B](ea: Either[E,A], f: A => Either[E,B]): Either[E,B] =
  ea match {
    case Left(e) => Left(e) // if left, ignore f and pass through
    case Right(a) => f(a) // if right, apply f and return result
  }
{% endhighlight %}

We see the pass-through logic in `mapEither` and `bindEither`. If an `Either` value is a `Left` value, applying `Either#map` and `Either#flatMap` will safely ignore the supplied functions, preserving the `Left` value along the way. Here's `subroutine` and `pipeline` from the example above refactored to either-passing style:

{% highlight scala %}
// either-passing style:
// wherein we pass an `Either` to the caller.

def subroutine(a: A): Either[C, B] = {
  if (`bad stuff`) Left(`appropriate C for this error`) else {
  val b = `dat B`
  Right(b) }
}

def pipeline(a: A): C = {
  for {
    a <- Right(`get an A`)
    b <- subroutine(a)
  } yield `process a B into a C`
} match {
  case Left(c) => c
  case Right(c) => c
}
{% endhighlight %}

Instead of returning a `B`, `subroutine` returns either a `C` (the exit-early case) or a `B` that we can consume later in `pipeline`. `pipeline` uses Scala's `for`/`yield` syntax to destructure either values and to chain successive computations based on those values. Each `<-` gets compiled into a call to `flatMap` (giving us the desired short-circuit semantics), and `yield` gets compiled into a call to `map`.

In software as in life, nothing is free. Calling `subroutine` is easier here than it was in continuation-passing style, but the cost is that `pipeline` must consume the `Either` produced by the `for`/`yield` block, branching on its possible cases. The branching is trivial, as both cases contain a `C` at this point, but we still need to do it unfortunately. One thing we can do to make it a bit nicer is to use `Either#fold`:

{% highlight scala %}
sealed trait Either[E,A] {
  ...
  def fold[B](withLeft: E => B)
             (withRight: A => B): B = this match {
    case Left(e) => withLeft(e)
    case Right(a) => withRight(a)
  }
}
{% endhighlight %}

Since both cases contain a `C`, we simply want to pass that `C` forward unchanged:

{% highlight scala %}
def pipeline(a: A): C = ( for {
  a <- Right(`get an A`)
  b <- subroutine(a)
} yield `process a B into a C` ).fold(c => c)(c => c)
{% endhighlight %}

As we refactor our program to use either-passing style, we will use the type of the end-result of our pipeline for the left generic parameter of `Either`. In other words, we'll want to work with `Either[Response, _]` values. Let's take a look at `getUser` written in either-passing style:

{% highlight scala %}
def getUser(req: Request): Either[Response, User] = {
  val token = req.header.get("Authorization")
  if (token.isEmpty) Left(noToken) else
  if (`malformed token?`) Left(malformedToken(token)) else
  if (`user not found?`) Left(noUser(token)) else
  Right(`the user`)
}
{% endhighlight %}

This code is fairly readable, compile-time safe, and easy to reuse. But we can still do better: Particularly, the repeated if-else is a bit clunky. We can factor this idiom out into its own method as an _effect_, a method that returns an `Either[Response, Unit]`. Then we can use `for`/`yield` notation to chain effects. We need our effect to return ``Left(`some appropriate response`)`` (thus aborting the remainder of the computation) if some boolean condition is met, so we'll name our effect `failIf` and it will take a `Boolean` and a `Response`. Let's see how this is done:

{% highlight scala %}
def failIf(p: Boolean, response: => Response): Either[Response, Unit] =
  if (p) Left(response) else Right(())
{% endhighlight %}

In fact, there's nothing special about `Response` in that method, so we can make this effect more reusable by using a generic type parameter instead of hard-coding the `Response` type:

{% highlight scala %}
def failIf[E](p: Boolean, e: => E): Either[E, Unit] =
  if (p) Left(e) else Right(())
{% endhighlight %}

Now we have a generic method that we can call in `for`/`yield` blocks at any place where we'd like to return early. Let's see it in action:

{% highlight scala %}
def getUser(req: Request): Either[Response, User] = {
  val token = req.header.get("Authorization")
  for {
    _ <- failIf(token.isEmpty, noToken)
    _ <- failIf(`malformed token?`, malformedToken(token.get))
    _ <- failIf(`user not found?`, noUser(token.get))
  } yield `the user`
}
{% endhighlight %}

Since the result of `failIf` is the unit value `()`, we're safe to throw it away---thus the `_` assignments. We're calling `failIf` for its effect, not for its result.

This is still pretty clunky, and partly because of that first assignment for `token`. `req.header.get("Authorization")` returns an `Option[String]`. If we can turn that `Option[String]` into an `Either[Response, String]`, then we can write the whole method in `for`/`yield` notation and make it much less clunky. In order to turn an `Option[String]` into an `Either[Response, String]`, we'll need to supply a `Response` to create a `Left` value in case the option is empty. The method that does this is `Option#toRight`:

{% highlight scala %}
def getUser(req: Request): Either[Response, User] = for {
  token <- req.header.get("Authorization").toRight(noToken)
  _     <- failIf(`malformed token?`, malformedToken(token))
  _     <- failIf(`user not found?`, noUser(token))
} yield `the user`
{% endhighlight %}

Let's refactor the rest of our program to use either-passing style instead of exception-passing style or continuation-passing style:

{% highlight scala %}
import Spec._
import Undefined._

object Eithers {

  def failIf[E](p: Boolean, e: => E): Either[E, Unit] =
    if (p) Left(e) else Right(())

  def getUser(req: Request): Either[Response, User] = for {
    token <- req.header.get("Authorization").toRight(noToken)
    _     <- failIf(`malformed token?`, malformedToken(token))
    _     <- failIf(`user not found?`, noUser(token))
  } yield `the user`

  def getResource(req: Request): Either[Response, Resource] = for {
    path <- Right(req.path)
    _    <- failIf(`resource not found?`, noResource(path))
  } yield `the resource`

  def execute(content: String, usr: User, src: Resource):
  Either[Response, Unit] = for {
    _ <- failIf(! `is permitted?`, notPermitted(src.path))
    _ <- failIf(! `is executed?`, badConnection)
  } yield ()

  def handlePost(req: Request): Response = {
    for {
      _   <- failIf(req.method != "POST", notAllowed(req.method))
      _   <- failIf(req.body.isEmpty, noBody)
      usr <- getUser(req)
      src <- getResource(req)
      _   <- execute(req.body, usr, src)
    } yield success(req.path, req.body)
  }.fold(identity, identity)
}
{% endhighlight %}

# Analysis of Continuations and Eithers

I avoid throwing exceptions in my own code. I prefer patterns that preserve compile-time safety, as it makes the code easier to test and easier to reuse (this is what's usually meant by "easier to reason about"). Both continuation-passing style and either-passing style preserve compile-time safety, making it harder to write code that is broken by design.

Between the two, I find that either-passing style is a bit easier to fit into larger designs: The methods are easier to invoke, since you don't need to provide a continuation, and their use is more obvious, since they return a concrete data structure that can be assigned or passed around. To use continuation-passing style effectively you may need to exercise a lot of forethought.

That said, `Either` abstracts a single aspect of control flow: short-circuit control flow. For some perspective, consider that `Future` abstracts asynchronous control flow, `List` can be used to abstract non-deterministic control flow, and `Stream` can be used to abstract parallel control flow. Continuation-passing style, on the other hand, abstracts control flow. Period. It can be employed to great effect to create first-class representations of control-flow features that might otherwise require specialized keywords and language semantics. In effect, you can add almost arbitrary functionality to your language using continuation-passing style thoughtfully.

In the end, the choice between either-passing style and continuation-passing style is largely a matter of taste. What is more readable to one person may be less readable to another person for instance. The key take-away is that both idioms allow us to avoid writing methods that throw, making it easier to reuse our code and write correct code.

# Acknowledgements

Thanks to [@anthony__brice](https://twitter.com/anthony__brice) for helpful corrections and suggestions.
