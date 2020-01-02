---
layout: post
title: "The IO Rosetta Stone"
date: 2020-01-01
permalink: /blog/the-io-rosetta-stone/
redirect-from:
  - /blog/22/
  - /blog/2020-01-01/
comments: true
tags:
  - haskell
  - functional programming
  - types
---

Recently to a friend, I quipped that it'd be a good exercise towards demystifying Haskell's `IO` type to write a comparable `IO` type in your favorite language. In this blog post, I do that for Java, Javascript, Python, and Scala.

<!--break-->

**TL;DR:** Here's the [code](http://github.com/friedbrice/io-rosetta-stone).

## Understanding Functional APIs

My favorite way of understanding a type is by looking at its constructors, combinators, and eliminators. But what do I mean by _constructors_, _combinators_, and _eliminators_? Let me show you by looking at a more familiar type first before we tackle `IO`.

{% highlight haskell %}
data Set a
{% endhighlight %}

We call `Set` an _opaque_ type because its authors chose to hide its data constructors. Instead of letting us muck around with the internals, they give us some functions that we can use to create sets.

{% highlight haskell %}
empty     ::                 Set a
singleton ::            a -> Set a
fromList  :: Ord a => [a] -> Set a
{% endhighlight %}

Notice that each of those functions returns a set, and none of them require a set in any of its inputs, so we call these functions _constructors_.

There are also a few functions for manipulating sets.

{% highlight haskell %}
insert       :: Ord a =>     a -> Set a -> Set a
delete       :: Ord a =>     a -> Set a -> Set a
union        :: Ord a => Set a -> Set a -> Set a
intersection :: Ord a => Set a -> Set a -> Set a
{% endhighlight %}

Each function in this group takes one or more sets in its input and returns a set. These functions are thought of as combining their inputs in some way to create new sets, so we call them _combinators_.

Finally, we need a way to get usable information out of a set. The authors provide us with a few functions that take sets as input but don't mention `Set` in their output, so we call them _eliminators_.

{% highlight haskell %}
null       ::                   Set a -> Bool
member     :: Ord a =>     a -> Set a -> Bool
size       ::                   Set a ->  Int
isSubsetOf :: Ord a => Set a -> Set a -> Bool
{% endhighlight %}

Any functional API for a type can be understood by listing out its constructors, combinators, and eliminators, and the dreaded `IO` type is no different.

## API of the IO Type

So, what are the constructors, combinators, and eliminators of the `IO` type?

{% highlight haskell %}
data IO a
{% endhighlight %}

Like `Set`, `IO` is opaque. To find `IO`'s constructors, we notice that `IO` is an instance of the `Monad` type class, which provides the `return` function:

{% highlight haskell %}
return :: a -> IO a
{% endhighlight %}

What are the other constructors? Well, `Prelude` is full of them:

{% highlight haskell %}
putStrLn :: String ->     IO ()
getLine  ::           IO String
...
{% endhighlight %}

At first glance, these don't feel like constructors, but trust me for now.

We get combinators from the `Monad` and `Functor` type classes:

{% highlight haskell %}
fmap  :: (a -> b) ->        IO a -> IO b
(>>=) ::     IO a -> (a -> IO b) -> IO b
(>>)  ::     IO a ->        IO b -> IO b
...
{% endhighlight %}

These functions take one or more `IO`s in their input and produce an `IO` in their output, so they're perfectly reasonable combinators.

One thing you might notice, though, is a curious lack of eliminators. In fact, Haskell has a designated way of eliminating `IO`: naming your function `main`.

{% highlight haskell %}
main :: IO ()
{% endhighlight %}

Naming your function `main` and running your program is how you eliminate the `IO` type and get access to the useful information inside.

The crucial thing about Haskell's `IO` type is that it's _referentially transparent_, which means that a value of type `IO a` doesn't perform any action, it only describes an action. `putStrLn "Hello!"` doesn't print to the screen: it describes printing to the screen. `getLine` doesn't get a line of input, it describes the action of getting a line of input. You could call this the _Command Pattern_ if that helps (but don't dwell on it if it doesn't).

This is why it's reasonable to call `putStrLn` and `getLine`  _constructors_: they allow you to construct `IO` values that describes some actions, and then you can further modify those action using the combinators `fmap` and bind (`(>>=)`). As you write your program, you are describing the actions that should take place, and the only action that does take place is the one you named `main`.

## A Simple Program

I wrote this simple program in order to have something to port over to the other languages. It uses the `IO` constructors, combinators, and eliminator we've listed above (though `(>>=)` and `(>>)` are used implicitly in `do` notation), so we'll have to port those to the other languages as well.

{% highlight haskell %}
appLogic :: String -> String -> String
appLogic x y = "Result: " <> show (read x + read y)

printMaybe :: Maybe String -> IO ()
printMaybe = maybe (return ()) putStrLn

prompt :: Maybe String -> (String -> Maybe String) -> IO String
prompt greet confirm = do
  printMaybe greet
  l <- getLine
  printMaybe (confirm l)
  return l

app :: IO ()
app = do
  x <- prompt
    (Just "Please input two numbers.")
    (Just . ("Got first input: " <>))
  y <- prompt
    Nothing
    (Just . ("Got second input: " <>))
  putStrLn (appLogic x y)

main :: IO ()
main = app
{% endhighlight %}

In each language, I port the `IO` type and its API in one module and I port the app in another module. This is to mimic practical usage, where you'll want the `IO` type to be in its own library.

## Java

First, the `IO` library.

{% highlight java %}
import java.util.function.Function;
import java.util.function.Supplier;

public final class IO<A> {
  private final Supplier<A> run;

  private IO(Supplier<A> run) {
    this.run = run;
  }
{% endhighlight %}

Like in Haskell, Java's `IO` is an opaque type. We accomplish this by making the class constructor and sole instance field private. We provide a few static factory methods as part of the public API.

{% highlight java %}
  /** Constructors */

  public static IO<String> getLine = new IO<String>( () ->
    System.console().readLine() );

  public static IO<Void> putStrLn(String str) {
    return new IO<Void>( () -> {
      System.out.println(str);
      return null;
    });
  }

  public static <A> IO<A> _return(A x) {
    return new IO<A>( () -> x );
  }
{% endhighlight %}

Combinators are instance methods.

{% highlight java %}
  /** Combinators */

  public <B> IO<B> map(Function<A, B> f) {
    return new IO<B>( () ->
      f.apply(this.unsafeRunIO()) );
  }

  public <B> IO<B> bind(Function<A, IO<B>> f) {
    return new IO<B>( () ->
      f.apply(this.unsafeRunIO()).unsafeRunIO() );
  }

  public <B> IO<B> and(IO<B> other) {
    return new IO<B>( () -> {
      this.unsafeRunIO();
      return other.unsafeRunIO();
    });
  }
{% endhighlight %}

A single eliminator, with an ominous name, rounds out the API.

{% highlight java %}
  /** Eliminators */

  public A unsafeRunIO() {
    return run.get();
  }
}
{% endhighlight %}

Now, the app. Unlike the library, the app is not very pretty, but it does demonstrate a few key points.

{% highlight java %}
import java.util.function.Function;
import java.util.Optional;

public final class App {

{% endhighlight %}

We're able to maintain a clean separation between core logic and the tedium of talking to the outside.

{% highlight java %}

  private static String appLogic(String x, String y) {
    return "Result: " + Integer.toString(
      Integer.parseInt(x) + Integer.parseInt(y));
  }

{% endhighlight %}

The `IO` type is extensible, in the sense that we may define our own custom `IO` operations. We're not limited to those found in the `IO` library.

{% highlight java %}
  private static IO<Void> printMaybe(Optional<String> x) {
    return x.map(IO::putStrLn).orElse(IO._return(null));
  }

  private static IO<String> prompt( Optional<String> greet,
                                    Function<String, Optional<String>> confirm ) {
    return printMaybe(greet)
      .and(IO.getLine).bind( l ->
        printMaybe(confirm.apply(l))
          .and(IO._return(l)));
  }

{% endhighlight %}

We maintain referential transparency by waiting until `main` to use `unsafeRunIO`. The result is that `app` is a first-class value. We are free to reuse it anywhere, or inline it, or otherwise refactor as we see fit.

{% highlight java %}
  public static IO<Void> app =
    prompt( Optional.of("Please input two numbers."),
            l -> Optional.of("Got first input: " + l) ).bind( x ->
    prompt( Optional.empty(),
            l -> Optional.of("Got second input: " + l) ).bind( y ->
    IO.putStrLn(appLogic(x, y)) ));

  public static void main(String[] args) {
    app.unsafeRunIO();
  }
}
{% endhighlight %}

## Javascript

The `IO` library in Javascript is delightfully small.

{% highlight javascript %}
const IO = (function() {
  'use strict';

  const IO = (run) => {
    return {
      unsafeRunIO: run,

      map: (f) => IO(() => f(run())),

      bind: (f) => IO(() => f(run()).unsafeRunIO()),

      and: (x) => IO(() => {
        run();
        return x.unsafeRunIO();
      })
    };
  };

  return {
    getLine: IO(() => window.prompt("")),

    putStrLn: (str) => IO(() => {
      window.alert(str);
      return null;
    }),

    return: (x) => IO(() => x)
  };
}());
{% endhighlight %}

The outer `IO` refers to the name of the module. The inner `IO` refers to the name of the class. We're able to hide the class by only exporting our three constructors.

We need an HTML file to run our Javascript app in the browser. It's in this file that we call `unsafeRunIO`.

{% highlight html %}
<html>
  <head>
    <script type="text/javascript" src="./IO.js"></script>
    <script type="text/javascript" src="./App.js"></script>
  </head>
  <body>
    <script type="text/javascript">
      App.main.unsafeRunIO()
    </script>
  </body>
</html>
{% endhighlight %}

Finally, our Javascript port of our app.

{% highlight javascript %}
const App = (function() {
  'use strict';

  const appLogic = (x, y) => 'Result: ' + (parseInt(x) + parseInt(y));

  const printMaybe = (strM) => strM == null ? IO.return(null) : IO.putStrLn(strM)

  const prompt = (greet, confirm) =>
    printMaybe(greet)
      .and(IO.getLine).bind( l =>
        printMaybe(confirm(l))
          .and(IO.return(l)));

  const app =
    prompt( 'Please input two numbers.',
            l => 'Got first input: ' + l ).bind( x =>
    prompt( null,
            l => 'Got second input: ' + l ).bind( y =>
    IO.putStrLn(appLogic(x, y)) ));

  return {
    main: app
  };
}());
{% endhighlight %}

## Python

Python is a delightful language that seems to revel in side effects (mostly in the form of assignment statements). Let's see if we can encapsulate `IO` here.

{% highlight python %}
class IO:

  def __init__(self, run):
    self.unsafeRunIO = run

  def map(self, f):
    return IO(lambda: f(self.unsafeRunIO()))

  def bind(self, f):
    return IO(lambda: f(self.unsafeRunIO()).unsafeRunIO())

  def _and(self, other):
    def go():
      self.unsafeRunIO()
      return other.unsafeRunIO()
    return IO(go)

def _return(x):
  return IO(lambda: x)

def putStrLn(str):
  def go():
    print(str)
    return None
  return IO(go)

getLine = IO(input)
{% endhighlight %}

Hopefully, the `IO` library looks second-nature by now. Remember, the object of the game is to create a data structure that describes the effects to be done, but doesn't do them until `unsafeRunIO` is called.

The only tricky part in Python is that lambdas can't span multiple lines. It's easy enough to get around this by defining and using a named function, though.

{% highlight python %}
import IO

def appLogic(x, y):
  return "Result: " + str(int(x) + int(y))

def printMaybe(strM):
  return IO.putStrLn(strM) if strM is not None else IO._return(None)

def prompt(greet, confirm):
  salute = printMaybe(greet)._and(IO.getLine)
  certify = lambda l: printMaybe(confirm(l))._and(IO._return(l))
  return salute.bind(certify)

app = prompt( "Please input two numbers.",
              lambda l: "Got first input: " + str(l) ).bind(lambda x:
      prompt( None,
              lambda l: "Got second input: " + str(l) ).bind(lambda y:
      IO.putStrLn(appLogic(x, y)) ))

if __name__ == "__main__":
  app.unsafeRunIO()
{% endhighlight %}

## Scala

The Scala `IO` library is even shorter than the Javascript one. We make the `IO` type opaque by marking it as sealed and by making the companion `apply` method private.

{% highlight scala %}
sealed trait IO[A] {

  def unsafeRunIO: A

  final def map[B](f: A => B): IO[B] = IO(f(unsafeRunIO))

  final def flatMap[B](f: A => IO[B]): IO[B] = IO(f(unsafeRunIO).unsafeRunIO)

  final def and[B](x: IO[B]): IO[B] = IO {
    unsafeRunIO
    x.unsafeRunIO
  }
}

object IO {

  private def apply[A](x: => A): IO[A] = new IO[A] {
    def unsafeRunIO: A = x
  }

  val getLine: IO[String] = IO(io.StdIn.readLine)

  def putStrLn(str: String): IO[Unit] = IO(println(str))

  def _return[A](x: A): IO[A] = IO(x)
}
{% endhighlight %}

Since Scala supports call-by-name arguments, we have a very convenient syntax `IO(...)` for "promoting" side-effectful code blocks into first-class actions.

In Scala we benefit from having `for` comprehensions, so our port follows the original Haskell much more closely (thought the port is not quite as pretty as the original).

{% highlight scala %}
object App {

  def appLogic(x: String, y: String): String =
    "Result: " + (x.toInt + y.toInt).toString

  def printMaybe(x: Option[String]): IO[Unit] =
    x.fold(IO._return(()))(IO.putStrLn)

  def prompt( greet: Option[String],
              confirm: String => Option[String]
            ): IO[String] = for {
    _ <- printMaybe(greet)
    l <- IO.getLine
    _ <- printMaybe(confirm(l))
  } yield l

  val app: IO[Unit] = for {
    x <- prompt( Some("Please input two numbers."),
                 l => Some("Got first input: " + l) )
    y <- prompt( None,
                 l => Some("Got second input: " + l) )
    _ <- IO.putStrLn(appLogic(x, y))
  } yield ()

  def main(args: Array[String]) = app.unsafeRunIO
}
{% endhighlight %}

Thanks for reading. I hope you enjoyed this post, and I sincerely hope that I helped eliminate some of the mysticism for you. It's not magic, after all.
