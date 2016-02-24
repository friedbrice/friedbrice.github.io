---
layout: post
title: "Automatic Differentiation is Trivial in Haskell"
date: 2015-12-01
permalink: /blog/10/
comments: true
tags:
- code
- haskell
- math
- calculus
- automatic differentiation
---

My first quarter at Bakersfield just ended, and so I found myself with
some time with which I could get back into coding. I read about
[automatic differentiation][1] a while ago, and I've finally learned
enough Haskell to implement it. It turns out Haskell has a lot of
primitive notions that make it really easy. I mean, ridiculously easy.

  [1]: http://en.wikipedia.org/wiki/Automatic_differentiation

<!--break-->

Automatic differentiation is one method whereby a computer can
numerically calculate the derivative of a function. One method, the
brute force method, to numerically calculate derivatives is by
estimating the limit of the difference quotient, the same thing we teach
students to do at the start of the Calculus sequence---estimate the
slope of the tangent line by looking at successively shorter secant
lines. Automatic differentiation is different, though, and basically
involves stating the differentiation rules in a way the computer can
understand. This way, automatic differentiation does not suffer from the
same problems with round-off error as the brute-force method. The
results of automatic differentiation are accurate up to the computer's
full precision.

We accomplish this by writing a data type and `Floating` instance for
_dual numbers_. A dual number is a pair \\( (x, x') \\). \\( x \\) is
thought of as the value of a differentiable function at an unspecified
value of some unspecified parameter, so perhaps you might think of \\( x
\\) being a particular value for a function \\( x(t) \\). \\( x' \\) is
then thought of as the derivative of \\( x(t) \\) at the same value of
\\( t \\). So, you'd think of the dual number \\( (4, 0.5) \\) as saying
_the function's value is 4 right now, and its derivative is 0.5 right
now_.

We'll walk through the implementation together.

{% highlight haskell linenos %}
-- AutoDiff.hs
-- Copyright 2015, Daniel Brice

data Dual = Dual Double Double deriving (Read, Show, Eq)

constantDual :: Double -> Dual
-- ^ Lifts a constant number to the `Dual` type.
constantDual x = Dual x 0

seedDual :: Double -> Double -> Dual
-- ^ Creates a Dual number.
seedDual x x' = Dual x x'

evaluateDual :: Dual -> Double
-- ^ Used to evaluate a function.
evaluateDual (Dual x _) = x

differentiateDual :: Dual -> Double
-- ^ Used to evaluate the derivative of a function.
differentiateDual (Dual _ x') = x'
{% endhighlight %}

On line 4, we introduce the type `Dual` as simply a pair of `Double`s,
and have Haskell create `Read`, `Show`, and `Eq` instances for us.

After that, we write a few convenience functions for doing simple
manipulations.

We'll ultimately want to write a `Floating` instance for `Dual`, but
`Floating` requires `Num` and `Fractional` first, so we write those
instances.

{% highlight haskell linenos %}
instance Num Dual where
  (+) (Dual u u') (Dual v v') = Dual (u + v) (u' + v')
  (*) (Dual u u') (Dual v v') = Dual (u * v) (u' * v + u * v')
  (-) (Dual u u') (Dual v v') = Dual (u - v) (u' - v')
  negate (Dual u u')          = Dual (negate u) (negate u')
  abs (Dual u u')             = Dual (abs u) (u' * (signum u))
  signum (Dual u u')          = Dual (signum u) 0
  fromInteger n               = Dual (fromInteger n) 0

instance Fractional Dual where
  (/) (Dual u u') (Dual v v') = Dual (u / v) ((u' * v - u * v') / v ** 2)
  recip (Dual u u')           = Dual (recip u) (-1 * u' * (recip (u ** 2)))
  fromRational n              = Dual (fromRational n) 0
{% endhighlight %}

Essentially, we think of these instances as teaching Haskell how to add,
subtract, multiply, and divide `Dual`s: the left hand side is what we
want to find, the right hand side shows Haskell how to find it.

If you'll notice from, say, line 3, the first component of the right
hand side is simple multiplication of `Double`s, but the second
component is the product rule. This is the core of the method of
automatic differentiation. The computer will happily multiply two dual
numbers, recording their value in the first component and their
derivative in the second component.

We're teaching the computer the differentiation rules, but the astute
reader will notice that the chain rule is conspicuously absent. If
you'll look at line 12 above, you'll see why we don't need the chain
rule. `recip u` is simply \\( 1/u \\). If we write out the second
component of the right hand side we get \\( -u'/u^2 \\), with the chain
rule correcly applied. Building the function's derivative into the
structure of our data allows us to build the chain rule into all of our
computations, automatically. (We'll still need the multivariable chain
rule, but multivariable Calculus is beyond the scope of my little
program here.)

Now on to the `Floating` instance for `Dual`. Depending on your point of
view, this could be where the magic happens, or this could be the most
boring part.

{% highlight haskell linenos %}
instance Floating Dual where
  pi                = Dual pi 0
  exp (Dual u u')   = Dual (exp u) (u' * exp u)
  sqrt (Dual u u')  = Dual (sqrt u) (u' / (2 * sqrt u))
  log (Dual u u')   = Dual (log u) (u' / u)
  sin (Dual u u')   = Dual (sin u) (u' * cos u)
  cos (Dual u u')   = Dual (cos u) (- u' * sin u)
  tan (Dual u u')   = Dual (tan u) (1 / ((cos u) ** 2))
  asin (Dual u u')  = Dual (asin u) (u' / (sqrt(1 - u ** 2)))
  acos (Dual u u')  = Dual (acos u) (- u' / (sqrt(1 - u ** 2)))
  atan (Dual u u')  = Dual (atan u) (u' / (1 + u ** 2))
  sinh (Dual u u')  = Dual (sinh u) (u' * cosh u)
  cosh (Dual u u')  = Dual (cosh u) (u' * sinh u)
  tanh (Dual u u')  = Dual (tanh u) (u' * (1 - (tanh u) ** 2))
  asinh (Dual u u') = Dual (asinh u) (u' / (sqrt(1 + u ** 2)))
  acosh (Dual u u') = Dual (acosh u) (u' / (sqrt(u ** 2 - 1)))
  atanh (Dual u u') = Dual (atanh u) (u' / (1 - u ** 2))
  (**) (Dual u u') (Dual v v')
    = Dual (u ** v) (u ** v * (v' * (log u) + (v * u' / u)))
  logBase (Dual u u') (Dual v v')
    = Dual (logBase u v) (((log v) * u' / u - (log u) * v' / v) / ((log u) ** 2))
{% endhighlight %}

The `Floating` typeclass requires us to provide methods that calculate
the values of the elementary functions for our data type, which we do
above. In every case, the first component of the right hand side falls
back on the default methods that Haskell provides for `Double`s, and the
second component calculates the derivative. Notice that the chain rule
is built into each individual differentiation rule.

Last, here's a short `main` that you can uncomment if you'd like to have
something that you can actually compile to an executable, but I prefer
simply loading my file into ghci.

{% highlight haskell linenos %}
--f :: Dual -> Dual
--f x = x ** 3 - sin (x ** 2)

--main = do
--  putStrLn "What's the derivative of f(x) = x^2 - sin(x^2) at x = 2?"
--  print . differentiateDual . f $ Dual 2 1
{% endhighlight %}

Let's play with our new toy in ghci. Define any function that takes a
`Floating a` input, ie, any function that relies on the methods defined
for the `Floating`, `Fractional`, and `Num` typeclasses. To find the
derivative of your function at \\( a \\), simply have Haskell evaluate
the function at `Dual a 1`.

{% highlight text %}
AutoDiff.hs> let f x = x ** 3 - sin (x ** 2)
AutoDiff.hs> :type f
f :: Floating a => a -> a
AutoDiff.hs> f 2
8.756802495307928
AutoDiff.hs> f (Dual 2 1)
Dual 8.756802495307928 14.614574483454447
{% endhighlight %}

Success!

If you play around with the auto-differ a little more, you'll find a
bug. I'm aware of it, and I know how to fix it, but it might be fun to
see if you can find the bug yourself, and then see if you can think of
an idea for how to fix it.
