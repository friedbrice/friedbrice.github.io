---
layout: post
title: "Yes, ML Functors Really Are Functors"
date: 1999-12-31
permalink: /blog/yes-ml-functors-really-are-functors/
redirect-from:
  - /blog/2022-01-22/
comments: true
tags:
  - standard ml
  - haskell
  - functional programming
  - category theory
---


Functors in ML bear little resemblance to functors in Haskell. This has led practitioners from both camps to conclude that ML functors aren't _really_ functors (in the category theory sense). This post exists to inform all such claimants that they're mistaken, to demonstrate that functors in ML are indeed functors in the strict sense, and to generally clear the matter up.

<!--break-->


## Functors in Haskell

This blog post is mostly targeted at people who already know some Haskell and have anywhere from a vague sense to expertise in another ML language. In particular I'm going to assume the reader has at least a passing familiarity with Haskell syntax and constructs. I need you know about Haskell's notions of (parametric) polymorphism[^polymorphism] and type classes, at least with about a [LYAH-level](http://learnyouahaskell.com/making-our-own-types-and-typeclasses) of understanding. If you're coming here with little Haskell knowledge, I do apologize; it is not my intent to exclude you. I will note, though, that [LYAH](http://learnyouahaskell.com/) is a quick read ;-)

[^polymorphism]: Which is, of course, the only _real_ polymorphism. Other so-called polymorphisms are merely name overloading.

We're going to orient ourselves by looking at three representative Haskell functors, `Maybe`, pairs (i.e., 2-tuples), and one I contrived specifically for the occasion, `Database`. I'm going to go into a bit more detail than is necessary, just to drive home a few points. Please bear with me as I develop the ideas.

### Maybe

{% highlight haskell %}
data Maybe a = Just a | Nothing
{% endhighlight %}

`Maybe` is polymorphic, so we can consider types such as `Maybe Int`, `Maybe String`, `Maybe Bool`, `Maybe [Double]`, `Maybe (Maybe Bool)`, `Maybe (Char -> Int)`, etc. But, it'll be important to remember that, while `Maybe Int` and `Maybe (Char -> Int)` are types, `Maybe` itself is not a type.

`Maybe` is useful for representing the _possibility of_ data, such as an optional field in a form, or the result of an action that might fail (e.g. division). Of course, we're often in a situation where we'd like to operate on this data if it's present, which we can do by pattern matching.

{% highlight haskell %}
factorial :: Natural -> Natural
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialMaybe :: Maybe Natural -> Maybe Natural
factorialMaybe Nothing = Nothing
factorialMaybe (Just n) = Just (factorial n)
{% endhighlight %}

In general, if `A` and `B` are types and if `f` is a function `f : a -> B`, then we may write a "`Maybe` version" of `f` that applies `f` if the data is present and returns `Nothing` is the data is absent.

Now, suppose we want to (potentially) apply `factorial` and then `show` the result. Notably, there are two ways we can do this. We can pattern-match on the result of `factorialMaybe` in order to `show` that, or we can pattern match at the start, and then apply both `factorial` and `show`.

{% highlight haskell %}
showFacMaybe1 :: Maybe Natural -> Maybe String
showFacMaybe1 maybeNat = case factorialMaybe maybeNat of
  Nothing -> Nothing
  Just n -> Just (show n)

showFacMaybe2 :: Maybe Natural -> Maybe String
showFacMaybe2 Nothing = Nothing
showFacMaybe2 (Just n) = Just $ (show . factorial) n
{% endhighlight %}

We can see by inspection that both `showFacMaybe1` and `showFacMaybe2` yield the same result for any input. Now, some would say that pattern matching is Haskell's bread and butter. Others would say that Haskell's bread and butter is _abstraction_. We see ourselves pattern matching on a `Maybe` value so we can operate on the data if it's there. In each case, we return `Nothing` if the data isn't. Let's abstract that pattern. This will allow us to refactor our `showFacMaybe1` and `showFacMaybe2` functions.

{% highlight haskell %}
applyMaybe :: (a -> b) -> Maybe a -> Maybe b
applyMaybe f Nothing = Nothing
applyMaybe f (Just x) = Just x

showFacMaybe1 :: Maybe Natural -> Maybe String
showFacMaybe1 maybeNat = (applyMaybe show . applyMaybe factorial) maybeNat

showFacMaybe2 :: Maybe Natural -> Maybe String
showFacMaybe2 maybeNat = applyMaybe (show . factorial) maybeNat
{% endhighlight %}

### Pairs

Given types `A` and `B`, Haskell has the type `(A, B)` consisting of all ordered pairs with first component in `A` and second component in `B`. Now, let's suppose we have some particular type `Foo`, and now let's consider all the types of the form `(Foo, a)`, such as `(Foo, Int)`, `(Foo, String)`, `(Foo, Bool)`, `(Foo, [Double])`, `(Foo, Maybe Bool)`, `(Foo, Char -> Int)`, etc.

A value of type `(Foo, A)` can be thought of as an `A` value with some attached metadata. Accordingly, we ought to be able to operate on the `A` value while retaining the metadata. And of course, we can. Mirroring the `Maybe` example, given a function `A -> B`, we can define a function `(Foo, A) -> (Foo, B)` in a formulaic way.

{% highlight haskell %}
applyPair :: (a -> b) -> (Foo, a) -> (Foo, b)
applyPair f (foo, a) = (foo, f a)

showFacPair1 :: (Foo, Natural) -> (Foo, String)
showFacPair1 pairNat = (applyPair show . applyPair factorial) pairNat

showFacPair2 :: (Foo, Natural) -> (Foo, String)
showFacPair2 pairNat = applyPair (show . factorial) pairNat
{% endhighlight %}

Now is a good time to convince yourself that `showFacPair1` and `showFacPair2` are equivalent, in the sense that for any `x :: (Foo, Natural)`, we'll have `showFacPair1 x == showFacPair2 x`.

### A Database Library

Okay, now we're ready to design a database access layer. Our schema relates two kinds of entities, _users_ and _groups_.

{% highlight haskell %}
data Database = Database
  { newUser :: Username -> [GroupId] -> IO UserId
  , getUser :: UserId -> IO (Maybe User)
  , addUserToGroup :: UserId -> GroupId -> IO ()
  , removeUserFromGroup :: UserId -> GroupId -> IO ()
  , removeUser :: UserId -> IO ()
  , newGroup :: Groupname -> IO GroupId
  , getGroup :: GroupId -> IO (Maybe Group)
  , removeGroup :: GroupId -> IO ()
  }
{% endhighlight %}

**TODO:** (->) functor specialized (functions that require a database connection)



### Data with Attached Metadata

**TODO:** Tuple functor specialized (functions that can produce some metadata)

### Generalizing the Pattern

**TODO:** Functor class and instance for above types.

So, we know that you _have_ a (Haskell) functor anywhere there's an instance of the `Functor` class. But in that case, what actually _is_ the functor? In other words, we know there's a functor somewhere that has something to do with `Maybe` values, since we have the instance `instance Functor Maybe`, but _what_ is that functor? What _thing_ can you point to and say, "That's it. That's the functor"? We'll see below.


## Functors in Category Theory

**TODO:** categories

**TODO:** category of types and functions

**TODO:** an example of an (specific) order category

**TODO:** an example of a (specific) group category

**TODO:** an example of a (specific) free category (don't use the word "free")

**TODO:** definition of functor

**TODO:** example of a functor between the order category and the group

**TODO:** haskell functors are functors from haskell to haskell

**TODO:** example of a functor from the free category into a category of types and functions


## Functors in ML

**TODO:** example code of a module signature and several implementations

**TODO:** generating a free category from a signature

**TODO:** show that an implementation is lawful if and only if it's a functor from the generated free category into the category of ML types


## Conclusion

**TODO:** something about how despite these two concepts (Haskell's `Functor` and ML functors) being almost completely unrelated in both syntactic form and programming purpose, and having largely sprung from two very different intellectual traditions, they are both examples of genuine category theory functors, each shows a way that functors can be applied to programming in a way that's distinct and complementary to the other. This motivates the following question: if there are already two distinct and complementary ways to apply the concept of functors to programming that we know of, how many other ways might there be, waiting to be discovered?

## Summary

---
