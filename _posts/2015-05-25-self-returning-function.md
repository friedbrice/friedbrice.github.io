---
layout: post
title: "Self-returning Function"
date: 2015-05-25
permalink: /blog/self-returning-function/
redirect_from:
  - /blog/09/
  - /blog/2015-05-25/
comments: true
tags:
  - python
---

[Yesterday](/blog/08) I gave a solution to the problem of creating a chainable function by implementing a class with a method that returned the instance.
Today I implemented the solution using a function that returns itself, more in line with the intent of Croll's exercise.

<!--break-->

Working in Python, I defined a prototype function called `sayIt` that was not chainable, and then defined a class that used a similar method to make it chainable.
I decided to change my prototype so that it would invoke itself in its return statement, as suggested by the solutions Croll provides.

Here are the implementations, both as a function with global and as a class:

{% highlight python %}
# sayIt.py

### As a function that returns itself

words = []

def sayIt( x = None ):
    global words
    if x is None:
        message = ' '.join(words)
        print(message)
        words = []
    else:
        words = words + [x]
        return sayIt

### As a class

class Words:
    def __init__(self):
        self.words = []
    def _(self, x = None):
        if x is None:
            message = ' '.join(self.words)
            print(message)
            self.words = []
        else:
            self.words = self.words + [x]
        return self
{% endhighlight %}

And here's its usage in the Python 3 interpreter:

{% highlight text %}
>>> words
[]
>>> sayIt("this")("is")("it")()
this is it
>>> sayIt("hi")("there")
<function sayIt at 0x7f1c5bc6db70>
>>> words
['hi', 'there']
>>> sayIt()
hi there
>>> sayings = Words()
>>> sayings.words
[]
>>> sayings._("hi")._("there")._()
hi there
<__main__.Words object at 0x7f1c5bb98240>
{% endhighlight %}
