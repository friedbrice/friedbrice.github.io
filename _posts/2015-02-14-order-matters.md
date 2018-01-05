---
layout: post
title: "Order Matters"
date: 2015-02-14
permalink: /blog/order-matters/
redirect_from: 
  - /blog/07/
  - /blog/2015-02-14/
comments: true
tags:
  - code
  - haskell
---

I'm [learning me a Haskell][lyah], very slowly, and I just wanted to share a little anecdote about function order and infinite lists that I thought was funny.
It has to do with the interplay between `filter` and `takeWhile`.

<!--break-->

In [Chapter 6][hof] we learn how to map and filter over lists.
We see a short example of a one-liner that will sum the odd squares smaller than 10,000.

{% highlight text %}
ghci> sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
166650
{% endhighlight %}

Everything seems to work fine.
(I can tell, because I know the sum of the odd squares smaller than 10,000 off the top of my head.)
Let's capitalize on our small victory by defining this as a function.

{% highlight text %}
ghci> let funnySum = sum . takeWhile (<10000) . filter odd . map (^2)
ghci> :type funnySum
funnySum :: [Integer] -> Integer
ghci> funnySum [1..]
166650
{% endhighlight %}

Great!
`funnySum` is now a general function that takes a list of integers and returns an integer.
Let's feed it some other lists of integers.
How about every number that's 3 modulo 4?

{% highlight text %}
ghci> funnySum [n | n <- [1..], n `mod` 4 == 3]
85825
{% endhighlight %}

Now, let's intentionally feed it a list without any odd numbers.
We should just get the sum of the empty list (namely 0) right?

{% highlight text %}
ghci> funnySum [2,4..]
{% endhighlight %}

Nothing gets printed.
The program is looping forever!
What's going on?
The answer has to do with the order in which we apply the two condition-checking mechanisms, `filter odd` and `takeWhile (<10000)`.

Our `funnySum` is defined using `takeWhile (<10000) . filter odd`.
With that definition, `funnySum [2,4..]` looks for the first odd square it encounters, checks to see if it's less then 10000, then adds it to the running sum.
Therein lies the problem: `funnySum` never encounters an odd square, so it can't advance to the step where the odd square is checked against `(<10000)`.

Fortunately, this is easy to fix.
We'll define a new function, `funnySum'` that will still solve the problem but will also be more flexible with allowable inputs.
All we need to do is swap the order of `takeWhile (<10000)` and `filter odd`.

{% highlight text %}
ghci> let funnySum' = sum . filter odd . takeWhile (<10000) . map (^2)
ghci> funnySum [1..]
166650
ghci> funnySum [n | n <- [1..], n `mod` 4 == 3]
85825
ghci> funnySum [2,4..]
0
{% endhighlight %}

We still answer the original questions correctly, and we're able to evaluate the sum of all the odd members of an infinite list of even numbers.
Success!

# Afterward

I originally thought that `filter odd . takeWhile (<10000)` and `takeWhile (<10000) . filter odd` would always return the same results whenever both of them returned a finite list.
However, this is simply not the case, because `filter odd` might remove a list element that would otherwise trigger `takewhile (<10000)`.

Example:

{% highlight text %}
ghci> let f = filter odd . takeWhile (<10000)
ghci> let g = takeWhile (<10000) . filter odd
ghci> let x = [3,10000,5]
ghci> f x
[3]
ghci> g x
[3,5]
{% endhighlight %}

Since they are different function, neither is better than the other.
Use the one that produces what you want for your particular piece of code.

The most general setting where `filter odd . takeWhile (<10000)` and `takeWhile (<10000) . filter odd` agree whenever each returns a finite list is when you know that the input list will be monotonically increasing.
In that case, use `filter odd . takeWhile (<10000)`, because the set of lists for which it terminates properly includes the set of lists for which `takeWhile (<10000) . filter odd` terminates.

# Acknowledgement

Thanks to Miran Lipovača for not only writing a wonderful, succinct, smoothly conversational textbook for learning Haskell, but also for making it freely available on the internet.

  [lyah]: http://learnyouahaskell.com
  [hof]: http://learnyouahaskell.com/higher-order-functions
