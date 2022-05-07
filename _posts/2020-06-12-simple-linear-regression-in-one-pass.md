---
layout: post
title: "Simple Linear Regression in One Pass"
date: 2020-06-12
permalink: /blog/simple-linear-regression-in-one-pass/
redirect-from:
  - /blog/23/
  - /blog/2020-06-12/
comments: true
tags:
  - haskell
  - functional programming
  - algebra
  - statistics
---

I recently had to implement linear regression for a hobby project. As I'd never learned how to calculate it, this was a great opportunity to learn. As I dug in, I saw that I'd need to be making two passes over my data. Let's see if we can fix that.

<!--break-->

## The Problem

[Simple linear regression](https://en.wikipedia.org/wiki/Simple_linear_regression) is where we take a set of points in the plane and compute the slope and \\(y\\)-intercept of the line that most-closely fits the points.

Precisely, if our data is \\( \left\\{ (x_i, y_i) \vert i \in I \right\\} \\), then we are looking for the pair \\(m\\), \\(b\\) which minimize the cost

\\[
\sum_{i \in I} \left( m x_i + b - y_i \right)^2 \text{,}
\\]

where \\(m\\) is the slope of our model, \\(b\\) is the \\(y\\)-intercept of our model, and \\(y = mx + b\\) is the value our model predicts for arbitrary input \\(x\\).

There are many approaches to this problem, ranging from multi-variable Calculus to geometry to linear algebra. We will use a statistical approach.

(Stating without proof) We have the following:

\\[
m = r \frac{s_y}{s_x}
\\]

and

\\[
b = \bar{y} - m \bar{x}
\\]

where

* \\(\bar{x}\\), \\(\bar{y}\\) are the means of \\(x_i\\), \\(y_i\\),
* \\(s_x\\), \\(s_y\\) are the standard deviations of \\(x_i\\), \\(y_i\\), and
* \\(r\\) is Pearson's correlation coefficient, defined as

\\[
r = \frac{
  \sum_i \left( (x_i - \bar{x}) (y_i - \bar{y}) \right)
}{
  \sqrt{ \sum_i (x_i - \bar{x})^2 \sum_i (y_i - \bar{y})^2 }
}\text{.}
\\]

([This useful video](https://www.youtube.com/watch?v=GhrxgbQnEEU) walks you through an example by hand.)

Recall the formula formula for standard deviation \\(s_x\\) (and analogous for \\(s_y\\)),

\\[
s_x = \sqrt{\frac{\sum_i (x_i - \bar{x})^2}{n - 1}} \text{,}
\\]

where \\(n\\) is the size of the sample \\(I\\).

First, let's simplify the formula for \\(m\\).

\\[
m
  = r \frac{s_y}{s_x}
  = r \frac{
    \sqrt{\frac{\sum_i (y_i - \bar{y})^2}{n - 1}}
  }{
    \sqrt{\frac{\sum_i (x_i - \bar{x})^2}{n - 1}}
  }
  = r \sqrt{\frac{\sum_i (y_i - \bar{y})^2}{\sum_i (x_i - \bar{x})^2}}
\\]

\\[
= \frac{
  \sum_i \left( (x_i - \bar{x}) (y_i - \bar{y}) \right)
}{
  \sqrt{ \sum_i (x_i - \bar{x})^2 \sum_i (y_i - \bar{y})^2 }
}
  \sqrt{\frac{\sum_i (y_i - \bar{y})^2}{\sum_i (x_i - \bar{x})^2}}
\\]

\\[
= \sum_i \left( (x_i - \bar{x}) (y_i - \bar{y}) \right)
  \sqrt{\frac{
    \sum_i (y_i - \bar{y})^2
  }{
    \sum_i (x_i - \bar{x})^2 \sum_i (y_i - \bar{y})^2 \sum_i (x_i - \bar{x})^2
  }}
\\]

\\[
= \sum_i \left( (x_i - \bar{x}) (y_i - \bar{y}) \right)
  \sqrt{\frac{1}{ \left( \sum_i (x_i - \bar{x})^2 \right)^2 }}
\\]

\\[
= \frac{
  \sum_i \left( (x_i - \bar{x}) (y_i - \bar{y}) \right)
}{
  \sum_i (x_i - \bar{x})^2
}
\\]

This final form is much nicer. To work with it, we no longer need to know the standard deviations \\(s_x\\), \\(s_y\\), though we still need to know the means \\(\bar{x}\\), \\(\bar{y}\\).

## Two-pass Implementation

The formulas for \\(m\\) and \\(b\\) above give us a straight-forward way to implement simple linear regression:

1. Find `avg_x` and `avg_y`,

2. Use `avg_x` and `avg_y` in the above formula for `slope` \\(m\\), and

3. Use `avg_x`, `avg_y`, and `slope` in the above formula for `intercept` \\(b\\).

{% highlight haskell %}
simpleLinearRegression :: Fractional a => [(a, a)] -> (a, a)
simpleLinearRegression points =
    (slope, intercept)
    where
    avg_x = sum [x | (x, _) <- points] / fromIntegral (length points)
    avg_y = sum [y | (_, y) <- points] / fromIntegral (length points)

    xys = sum [(x - avg_x) * (y - avg_y) | (x, y) <- points]
    xxs = sum [(x - avg_x) * (x - avg_x) | (x, _) <- points]

    slope = xys / xxs
    intercept = avg_y - slope * avg_x
{% endhighlight %}

This works, but it makes six passes over the list `points`. If this list is very long, then each pass will take a non-trivial amount of time. We can refactor this using monoids and `foldMap` to reduce the number of list passes we need to make.

{% highlight haskell %}
class Semigroup a => Monoid a where
    (<>) :: a -> a -> a -- from `Semigroup`
    mempty :: a
{% endhighlight %}

A type `a` is a monoid if it has an associative binary operation `<>` with an identity element `mempty`.

{% highlight haskell %}
foldMap :: Monoid a => (x -> a) -> [x] -> a -- specialized to lists
{% endhighlight %}

`foldMap` takes a list `[x]` and a mapping function `x -> a` and, if `a` is a monoid, applies the function to the list elements and combines the results using `<>`. Concretely, `foldMap f [x1, x2, x3] == f x1 <> f x2 <> f x3`. `foldMap` of an empty list gives `mempty`.

We'll use the monoids `Sum a` and `(a, b)` in our refactor.

{% highlight haskell %}
-- Import from `Data.Monoid`
newtype Sum a = Sum a

instance Num a => Monoid (Sum a) where
    Sum x <> Sum y = Sum (x + y)
    mempty = Sum 0

instance (Monoid a, Monoid b) => Monoid (a, b) where
    (a1, b1) <> (a2, b2) = (a1 <> a2, b1 <> b2)
    mempty = (mempty, mempty)

-- ... similar instances for 3-tuples, 4-tuples, etc ...
{% endhighlight %}

Armed with these components, let's take a closer look at calculating `avg_x` and `avg_y`. We're currently passing over the list four times to calculate them.

{% highlight haskell %}
avg_x = sum [x | (x, _) <- points] / fromIntegral (length points)
avg_y = sum [y | (_, y) <- points] / fromIntegral (length points)
{% endhighlight %}

We can get this down to three passes if we share the length.

{% highlight haskell %}
n = fromIntegral (length points)
avg_x = sum [x | (x, _) <- points] / n
avg_y = sum [y | (_, y) <- points] / n
{% endhighlight %}

Next, we can use the `Sum a` and `(a, b)` monoids to get both sums in one pass.

{% highlight haskell %}
n = fromIntegral (length points)
(Sum xs, Sum ys) = foldMap (\(x, y) -> (Sum x, Sum y)) points
avg_x = xs / n
avg_y = ys / n
{% endhighlight %}

We're down to two passes, but we can get the length at the same time we get the sums with this one neat trick: count the number of list elements by packing a `Sum 1` into the result of our mapping function.

{% highlight haskell %}
(Sum n, Sum xs, Sum ys) = foldMap (\(x, y) -> (Sum 1, Sum x, Sum y)) points
avg_x = xs / n
avg_y = ys / n
{% endhighlight %}

This is a bit tricky, so let's look at an example to see how it works:

{% highlight haskell %}
foldMap (\(x, y) -> (Sum 1, Sum x, Sum y)) [(x1, y1), (x2, y2), (x3, y3)]
    == (Sum 1, Sum x1, Sum y1)
        <> (Sum 1, Sum x2, Sum y2)
            <> (Sum 1, Sum x3, Sum y3)
    == (Sum 1 <> Sum 1 <> Sum 1,
        Sum x1 <> Sum x2 <> Sum x3,
            Sum y1 <> Sum y2 <> Sum y3)
    == (Sum 3, Sum (x1 + x2 + x3), Sum (y1 + y2 + y3))
{% endhighlight %}

So we've gotten both averages in just one pass! Let's refactor the rest of the program.

{% highlight haskell %}
simpleLinearRegression :: Fractional a => [(a, a)] -> (a, a)
simpleLinearRegression points =
    (slope, intercept)
    where
    avg (x, y) = (Sum 1, Sum x, Sum y)

    (Sum n, Sum xs, Sum ys) = foldMap avg points
    avg_x = xs / n
    avg_y = ys / n

    reg (x, y) =
        (Sum $ x' * y', Sum $ x' * x')
        where
        x' = x - avg_x
        y' = y - avg_y

    (Sum xys, Sum xxs) = foldMap reg points
    slope = xys / xxs
    intercept = avg_y - slope * avg_x
{% endhighlight %}

We make two passes over our list, one using the mapping function `avg` and one using the mapping function `reg`. This is already a huge improvement over our first draft.

## Monoid-valued Functions

At first glance, it seems like two passes is the best we can hope for, since `reg` needs to use of the results `avg_x` and `avg_y` of the first pass. Fortunately, as programmers we have a one-size-fits-all tool for abstracting away information that we don't yet have: functions. We can make `reg` a function that gets `avg_x` and `avg_y` as inputs.

{% highlight haskell %}
reg (x, y) (avg_x, avg_y) =
    (Sum $ x' * y', Sum $ x' * x')
    where
    x' = x - avg_x
    y' = y - avg_y
{% endhighlight %}

I assert that this solves all of our problems, but the astute reader might now notice a subtlety. `reg` has the special requirement that it needs to map our list elements into a monoid. In its original form we have `reg :: (a, a) -> (Sum a, Sum a)`, which does indeed map a list element `(a, a)` into the monoid `(Sum a, Sum a)`. Our new version of `reg`, though, has a different signature. We now have `reg :: (a, a) -> ( (a, a) -> (Sum a, Sum a) )`, which maps a list element `(a, a)` into the function type `(a, a) -> (Sum a, Sum a)`. It turns out that the type `(a, a) -> (Sum a, Sum a)` is a perfectly reasonable monoid already.

{% highlight haskell %}
instance Monoid a => Monoid (x -> a) where
    (f <> g) x = f x <> g x
    mempty = const mempty
{% endhighlight %}

The instance above says that any function type where the target type `a` is a monoid is itself a monoid in a formulaic way and with no conditions on the source type `x`. In other words, _monoid-valued functions always form a monoid_. We just need to make sure that our definition of `<>` is associative and that `const empty` is an identity for `<>`.

{% highlight haskell %}
(f <> (g <> h)) x
    == f x <> (g <> h) x -- definition of `<>` on type `x -> a`
    == f x <> (g x <> h x) -- definition of `<>` on type `x -> a`
    == (f x <> g x) <> h x -- by associativity of `<>` on type `a`
    == ((f <> g) x) <> h x -- definition of `<>` on type `x -> a`
    == ((f <> g) <> h) x -- definition of `<>` on type `x -> a`

(f <> const mempty) x
    == f x <> const mempty x -- definition of `<>` on type `x -> a`
    == f x <> mempty -- definition of `const`
    == f x -- by identity of `mempty` on type `a`

(const mempty <> f) x
    == const mempty x <> f x -- definition of `<>` on type `x -> a`
    == mempty <> f x -- definition of `const`
    == f x -- by identity of `mempty` on type `a`
{% endhighlight %}

The three stanzas above demonstrate that `f <> (g <> h)` is `(f <> g) <> h`, `f <> const mempty` is `f`, and `const mempty <> f` is `f`. Thus, `x -> a` is a valid monoid.

## One-pass Implementation

Armed with this new method of building up monoids, we refactor our program to make just one pass over `points`.

{% highlight haskell %}
simpleLinearRegression :: Fractional a => [(a, a)] -> (a, a)
simpleLinearRegression points =
    (slope, intercept)
    where
    avg (x, y) = (Sum 1, Sum x, Sum y)

    reg (x, y) (avg_x, avg_y) =
        (Sum $ x' * y', Sum $ x' * x')
        where
        x' = x - avg_x
        y' = y - avg_y

    ((Sum n, Sum xs, Sum ys), getReg) = foldMap (\p -> (avg p, reg p)) points

    avg_x = xs / n
    avg_y = ys / n
    (Sum xys, Sum xxs) = getReg (avg_x, avg_y)
    slope = xys / xxs
    intercept = avg_y - slope * avg_x
{% endhighlight %}

## Analysis

Using the ideas above, I wrote three implementations of simple linear regression and compared their performance on a sample data set. I compiled each implementation to a standalone Macos binary using GHC 8.8.3 and the `-O2` optimization flag.

The data set (provided by [Kaggle](https://www.kaggle.com/sohier/calcofi)) features ocean temperatures vs. salinity and consists of 864,862 data points, which I then quadrupled to 3,459,448 data points using Unix `cat`. I ran each implementation four times, using Unix `time` to record how long each run took.

Notably, I'm ignoring memory in this analysis, though memory complexity would be an important question to answer in a more comprehensive study. Also, for reasons unknown to me, the second implementations gives a slightly different result than the first and third. I should probably look into why that is.

### Naive Implementation

We directly port the math over to Haskell, ignoring performance concerns such as multiple list passes and common subexpressions. This one tends to finish in about 15 seconds.

{% highlight haskell linenos %}
-- naive-linear-regression.hs
import Data.Maybe (mapMaybe)

main :: IO ()
main =
    print . simpleLinearRegression . parse =<< getContents

readM :: Read a => String -> Maybe a
readM str =
    case reads str of
        [(x, "")] -> Just x
        _ -> Nothing

parse :: String -> [(Double, Double)]
parse =
    mapMaybe parseLine . lines . filter (/= '\r')
    where
    parseLine raw =
        let (x, y) = break (== ',') raw
        in (,) <$> readM x <*> readM (tail y)

simpleLinearRegression :: Fractional a => [(a, a)] -> (a, a)
simpleLinearRegression points =
    (slope, intercept)
    where
    avg_x = sum [x | (x, _) <- points] / fromIntegral (length points)
    avg_y = sum [y | (_, y) <- points] / fromIntegral (length points)

    xys = sum [(x - avg_x) * (y - avg_y) | (x, y) <- points]
    xxs = sum [(x - avg_x) * (x - avg_x) | (x, _) <- points]

    slope = xys / xxs
    intercept = avg_y - slope * avg_x
{% endhighlight %}

{% highlight console %}
$ time ./naive-linear-regression < 2190_3685_bundle_archive/quad.csv
(-5.520773058533541e-2,34.4409093436518)
real  0m14.858s
user  0m14.063s
sys 0m0.535s

$ time ./naive-linear-regression < 2190_3685_bundle_archive/quad.csv
(-5.520773058533541e-2,34.4409093436518)
real  0m15.055s
user  0m14.489s
sys 0m0.540s

$ time ./naive-linear-regression < 2190_3685_bundle_archive/quad.csv
(-5.520773058533541e-2,34.4409093436518)
real  0m15.084s
user  0m14.521s
sys 0m0.542s

$ time ./naive-linear-regression < 2190_3685_bundle_archive/quad.csv
(-5.520773058533541e-2,34.4409093436518)
real  0m14.882s
user  0m14.323s
sys 0m0.539s
{% endhighlight %}

### One-pass Implementation

We use built-in `Monoid` instances to condense our calculations into one list pass. Surprisingly, this one takes longer than the naive implementation, tending to finish in about 22 seconds.

{% highlight haskell linenos %}
-- one-pass-linear-regression.hs
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(Sum))

main :: IO ()
main =
    print . simpleLinearRegression . parse =<< getContents

readM :: Read a => String -> Maybe a
readM str =
    case reads str of
        [(x, "")] -> Just x
        _ -> Nothing

parse :: String -> [(Double, Double)]
parse =
    mapMaybe parseLine . lines . filter (/= '\r')
    where
    parseLine raw =
        let (x, y) = break (== ',') raw
        in (,) <$> readM x <*> readM (tail y)

simpleLinearRegression :: Fractional a => [(a, a)] -> (a, a)
simpleLinearRegression points =
    (slope, intercept)
    where
    avg (x, y) = (Sum 1, Sum x, Sum y)

    reg (x, y) (avg_x, avg_y) =
        (Sum $ x' * y', Sum $ x' * x')
        where
        x' = x - avg_x
        y' = y - avg_y

    ((Sum n, Sum xs, Sum ys), getReg) =
        foldMap (\p -> (avg p, reg p)) points

    avg_x = xs / n
    avg_y = ys / n
    (Sum xys, Sum xxs) = getReg (avg_x, avg_y)
    slope = xys / xxs
    intercept = avg_y - slope * avg_x
{% endhighlight %}

{% highlight console %}
$ time ./one-pass-linear-regression < 2190_3685_bundle_archive/quad.csv
(-5.5207730585339065e-2,34.44090934365883)
real  0m22.002s
user  0m20.592s
sys 0m1.190s

$ time ./one-pass-linear-regression < 2190_3685_bundle_archive/quad.csv
(-5.5207730585339065e-2,34.44090934365883)
real  0m21.829s
user  0m20.619s
sys 0m1.181s

$ time ./one-pass-linear-regression < 2190_3685_bundle_archive/quad.csv
(-5.5207730585339065e-2,34.44090934365883)
real  0m22.520s
user  0m21.262s
sys 0m1.212s

$ time ./one-pass-linear-regression < 2190_3685_bundle_archive/quad.csv
(-5.5207730585339065e-2,34.44090934365883)
real  0m21.883s
user  0m20.666s
sys 0m1.179s
{% endhighlight %}

### One-pass Unboxed Implementation

Not content to have the naive implementation win, I still had a few tricks up my sleeve to squeeze more calculations out of each CPU cycle. For one, since we know we're consuming our whole input list, we can switch from `foldMap` to the strict `foldMap'`. For another, the one-pass implementation achieves a satisfying level of code reuse through polymorphism and composition of tuples, but those extra pointers add up. We can gain some performance by monomorphising and unboxing all of our data, at the cost of implementing `Semigroup` and `Monoid` instances by hand. This one tends to finish in under 12 seconds.

{% highlight haskell linenos %}
-- one-pass-strict-unboxed-linear-regression.hs
{-# LANGUAGE BangPatterns #-}

import Data.Foldable (foldMap')
import Data.Maybe (mapMaybe)

main :: IO ()
main =
    print . simpleLinearRegression . parse =<< getContents

readM :: Read a => String -> Maybe a
readM str =
    case reads str of
        [(x, "")] -> Just x
        _ -> Nothing

parse :: String -> [Pair]
parse =
    mapMaybe parseLine . lines . filter (/= '\r')
    where
    parseLine raw =
        let (x, y) = break (== ',') raw
        in Pair <$> readM x <*> readM (tail y)

data Pair =
    Pair {-# UNPACK #-} !Double {-# UNPACK #-} !Double
    deriving Show

instance Semigroup Pair where
    Pair x1 y1 <> Pair x2 y2 = Pair (x1 + x2) (y1 + y2)

instance Monoid Pair where
    mempty = Pair 0 0

data Stat =
    Stat
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        {-# UNPACK #-} !Double
        !(Pair -> Pair)

instance Semigroup Stat where
    Stat n1 x1 y1 f1 <> Stat n2 x2 y2 f2 =
        Stat (n1 + n2) (x1 + x2) (y1 + y2) (f1 <> f2)

instance Monoid Stat where
    mempty = Stat 0 0 0 (const mempty)

simpleLinearRegression :: [Pair] -> Pair
simpleLinearRegression points =
    Pair slope intercept
    where
    avgReg !(Pair x y) =
        Stat 1 x y $ \(Pair !avg_x !avg_y) ->
            let
                x' = x - avg_x
                y' = y - avg_y
            in
                Pair (x' * y') (x' * x')

    Stat n xs ys getReg = foldMap' avgReg points

    avg_x = xs / n
    avg_y = ys / n
    Pair xys xxs = getReg (Pair avg_x avg_y)
    slope = xys / xxs
    intercept = avg_y - slope * avg_x
{% endhighlight %}

{% highlight console %}
$ time ./one-pass-strict-unboxed-linear-regression < 2190_3685_bundle_archive/quad.csv
Pair (-5.520773058533541e-2) 34.4409093436518
real  0m11.762s
user  0m11.435s
sys 0m0.148s

$ time ./one-pass-strict-unboxed-linear-regression < 2190_3685_bundle_archive/quad.csv
Pair (-5.520773058533541e-2) 34.4409093436518
real  0m11.678s
user  0m11.530s
sys 0m0.147s

$ time ./one-pass-strict-unboxed-linear-regression < 2190_3685_bundle_archive/quad.csv
Pair (-5.520773058533541e-2) 34.4409093436518
real  0m11.680s
user  0m11.536s
sys 0m0.144s

$ time ./one-pass-strict-unboxed-linear-regression < 2190_3685_bundle_archive/quad.csv
Pair (-5.520773058533541e-2) 34.4409093436518
real  0m11.563s
user  0m11.412s
sys 0m0.147s
{% endhighlight %}

## Conclusions

I was caught off-guard by the test results. I thought it'd go without saying that the one-pass version would trounce the naive version. Instead, the opposite was true. The naive version did better than the polymorphic one-pass version, and I found myself having to really reach to beat the naive implementation in the one-pass unboxed version. It'd be interesting to compare the Core GHC produces for these three implementations to see how many list passes the naive version really ends up doing.

I think this blog post is less a story about how awesome monoids are (They are.) and more a story about how awesome GHC is (It is.). To me, it's amazing that GHC can take the straightforward (and, frankly, kinda sloppy) naive implementation and compile it down to efficient code. To me, this reinforces a general theme in Haskell: do the obvious, simple thing first.


## Appendix

In trying to find out why the six-pass naive version did so well compared to the first one-pass version, I went ahead and implemented the six-pass naive version in python.

{% highlight python %}
import sys

def main():
    print(simpleLinearRegression(read(sys.stdin)))

def read(input):
    points = []
    for line in input:
        try:
            [x, y] = line.split(",")
            points.append( (float(x), float(y)) )
        except:
            pass
    return points

def simpleLinearRegression(points):
    avg_x = sum([x for (x, _) in points]) / len(points)
    avg_y = sum([y for (_, y) in points]) / len(points)

    xys = sum([(x - avg_x) * (y - avg_y) for (x, y) in points])
    xxs = sum([(x - avg_x) * (x - avg_x) for (x, _) in points])

    slope = xys / xxs
    intercept = avg_y - slope * avg_x

    print(len(points))

    return (slope, intercept)

if __name__ == "__main__":
    main()
{% endhighlight %}

The six-pass python version runs in about 3 seconds.

{% highlight console %}
$ time python naive.py < 2190_3685_bundle_archive/quad.csv
(-0.05520773058533541, 34.4409093436518)
real    0m3.384s
user    0m3.156s
sys 0m0.224s

$ time python naive.py < 2190_3685_bundle_archive/quad.csv
(-0.05520773058533541, 34.4409093436518)
real    0m3.375s
user    0m3.159s
sys 0m0.211s

$ time python naive.py < 2190_3685_bundle_archive/quad.csv
(-0.05520773058533541, 34.4409093436518)
real    0m3.395s
user    0m3.166s
sys 0m0.222s

$ time python naive.py < 2190_3685_bundle_archive/quad.csv
(-0.05520773058533541, 34.4409093436518)
real    0m3.352s
user    0m3.134s
sys 0m0.209s
{% endhighlight %}

So my new question is, why the hell is my Haskell so slow? What am I doing wrong here. Will update when I find out.


## Appendix B

Profiling immediately pointed to `readM` as the culprit, taking 78% of the time. After a quick search for "Haskell is read slow?", I found [this SO question](https://stackoverflow.com/questions/29186541/why-is-this-haskell-program-so-much-slower-than-an-equivalent-python-one). The answer suggested refactoring to use _bytestring_ instead, and that cut a big chunk of time out.

Refactored to use _bytestring_, the naive implementation takes just over 4 seconds, the polymorphic one-pass version takes just over 7 seconds, and the strict unboxed one-pass version takes just under 4 seconds, still slower than the naive Python implementation, which takes just under 3.5 seconds.

I think the data set I'm using (about 3.5 million list elements, about 42 megabytes) is just too small to make multiple passes a problem; however, on a significantly larger dataset, such as one that could not fit in memory, a one-pass implementation would OOM building up the closure needed to compute `xys` and `xxs`, so I'm not sure what to do in that situation. If you have an idea leave a comment, or reply on [Twitter](https://twitter.com/_fried_brice_/).

## Appendix 3

Sat down with my math friends (virtually) and we came up with a way to do it in constant memory. Expect that, and some interesting observations made by my Twitter friends, in a follow up post.
