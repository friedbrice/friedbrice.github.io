---
layout: post
title: "Yes, ML Functors Really Are Functors"
date: 1999-12-31
permalink: /blog/yes-ml-functors-really-are-functors/
redirect-from:
  - /blog/2099-12-31/
comments: true
tags:
  - ml
  - haskell
  - functional programming
  - category theory
---


Functors in ML bear little resemblance to functors in Haskell. This has led practitioners from both camps to conclude that ML functors aren't _really_ functors (in the category theory sense). This post exists to inform all such claimants that they're mistaken, to demonstrate that functors in ML are indeed functors in the strict sense, and to generally clear the matter up.

<!--break-->


## Functors in Haskell

**TODO:** revise the below paragraph, since I'm going into a lot of detail.

I'm going to assume you, dear reader, have a passing familiarity with Haskell syntax and constructs. In particular, I need you know about Haskell's notions of polymorphism and classes, at least at about a [LYAH-level](http://learnyouahaskell.com/making-our-own-types-and-typeclasses) of understanding. If you're coming here from the ML side of the aisle, with little Haskell knowledge, I do apologize; it is not my intent to exclude you. I will note, though, that [LYAH](http://learnyouahaskell.com/) is a quick read ;-)

### Potentially-missing Data

Let's look at one of my personal favorite things in Haskell, `Maybe`.

{% highlight haskell %}
data Maybe a = Just a | Nothing
{% endhighlight %}

`Maybe` is polymorphic, so we can consider types such as `Maybe Int`, `Maybe String`, `Maybe Bool`, `Maybe [Double]`, `Maybe (Maybe Bool)`, `Maybe (Char -> Int)`, etc. But, it'll be important to remember that, while `Maybe Int` and `Maybe (Char -> Int)` are types, `Maybe` itself is not a type.

`Maybe` is useful for representing the possibility of data (such as an optional field in a form, or the result of an action that might fail). Of course, we're often in a situation where we'd like to operate on this data if it's present, which we can do by pattern matching.

{% highlight haskell %}
x :: Maybe Int
x = Just 3

y :: Maybe Int
y = case x of
  Just x' -> Just (factorial x')
  Nothing -> Nothing

z :: Maybe String
z = case y of
  Just y' -> Just (show y')
  Nothing -> Nothing
{% endhighlight %}

Twice, we pattern match on a `Maybe` value in order to operate on the data if it's there. Of course, we're good programmers, and when we spot a pattern (especially one so ubiquitous as operating on possibly-missing data), we factor it out to its own function. The only things that change in those two pattern-matching stanzas are the `Maybe` value (`x` and `y`, respectively) and the operation to potentially apply (`factorial` and `show`, respectively), so this tells us that our function will need two arguments: a function and a `Maybe` value.

{% highlight haskell %}
applyMaybe :: (a -> b) -> Maybe a -> Maybe b
applyMaybe f maybe_a = case maybe_a of
  Just a -> Just (f a)
  Nothing -> Nothing

x :: Maybe Int
x = Just 3

y :: Maybe Int
y = applyMaybe (+ 5) x

z :: Maybe String
z = applyMaybe show y
{% endhighlight %}

That's already much better! I have a feeling we'll get a lot of mileage out of `applyMaybe`.

### A Database Library

Okay, now we're ready to design a database library.

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
