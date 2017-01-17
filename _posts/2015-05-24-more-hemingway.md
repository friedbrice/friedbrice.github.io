---
layout: post
title: "More Hemingway"
date: 2015-05-24
permalink: /blog/2015-05-24/
redirect_from: [ /blog/08/ ]
comments: true
tags:
- code
- haskell
- python
---

In a [prior post](/blog/05/) I mentioned that I was working my way through the exercises in [If Hemingway Wrote Javascript][1].
The semester is over, so I finally have some time to return to it.

  [1]: http://www.amazon.com/Hemingway-Wrote-JavaScript-Angus-Croll/dp/1593275854/ref=sr_1_1?ie=UTF8&qid=1422475515&sr=8-1&keywords=if+hemingway+wrote+javascript

I was writing solutions to all the exercises in Haskell, when I basically hit a brick wall when Exercise 5 asks the student to "Write a chainable function that accepts one word per function call but, when called without arguments, will report back all the previously passed words in order."
In Haskell...

<!--break-->

In case you are uninitiated, Haskell is a strongly-typed, purely-functional programming language, which implies that data types for functions' inputs and outputs must be clearly defined, and furthermore that variables cannot be mutated in place.
Exercise 5 asks us to write a function that can accept as input a string _or_ nothing, and then will either produce an IO action _or_ mutate(!) a running list of strings, depending on the type of the input.

I'm not saying that this can't be done in Haskell---It totally can be done in Haskell.
It's just that the notions of _function_ in Haskell and _function_ in other programming languages don't exactly overlap.
Solving this problem in the most elegant way will involve clever use of the `State` monad and the `IO` monad and maybe even defining some of my own monads.
You can see my work in progress on my [github repo](http://github.com/friedbrice/hemingway).

The problem Exercise 5 poses makes much more sense in an object-oriented paradigm: I decided I'd write my Python solution first.
Here was my first go:

{% highlight python %}
# sayIt.py
words = []

def sayIt( x = None ):
    global words
    if x is None:
        message = ' '.join(words)
        print(message)
    else:
        words = words + [x]
{% endhighlight %}

Here, we define the global variable `words` and the function `sayIt` operates more or less how the problem statement requires, except for one detail: sayIt isn't chainable.

In order to make it chainable, we need to (_ed:_ can) make it a method of an object.
If it returns itself upon method calls, then we can just dot subsequent method calls on the end.
Here's my second go:

{% highlight python %}
# sayItClassy.py
class Words:
    def __init__(self):
        self.words = []
    def _(self, x = None):
        if x is None:
            message = ' '.join(self.words)
            print(message)
        else:
            self.words = self.words + [x]
        return self
{% endhighlight %}

When an instance of `Words` is declared, it's given an internal variable called `words` which stores the current state, _ie,_ the list of all the words that we've passed to the object via its sole method `_`.
The method `_` has the behavior of `sayIt` from above, but applies changes to the instance rather than to a global variable.
Thus, in the Python interactive prompt, we get the desired behavior:

{% highlight text %}
>>> sayIt = Words()
>>> sayIt._("my")._("name")._("is")._("Daniel")._()
my name is Daniel
{% endhighlight %}
