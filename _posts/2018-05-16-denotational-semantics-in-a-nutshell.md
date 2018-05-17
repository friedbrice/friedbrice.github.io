---
layout: post
title: "Denotational Semantics in a Nutshell"
date: 2018-05-16
permalink: /blog/denotational-semantics-in-a-nutshell/
redirect-from:
  - /blog/20/
  - /blog/2018-05-16/
comments: true
tags:
  - denotational semantics
  - programming language theory
---

Wherein I attempt to give a 10,000-foot overview of semantics of
programming languages.

<!--break-->

First off, I am a mathematician, not a programming language theorist.
Please comment below if I say anything that's completely off-base or
even just a bit misleading: I'll be happy to make the necessary
corrections.

*\*Clears throat\**

## Operational and Logical Semantics

You can study a programming language by its _operational semantics_ or
by its _logical semantics,_ which both try to answer the question "when
are two programs equivalent?".

Two programs are _logically equivalent_ only when they are the same in
all possible interpretations of the primitive language symbols. As a
result, not a lot of programs are logically equivalent. In contrast, two
programs are _operationally equivalent_ when they produce the same
result for all possible inputs, whether or not they have
logically-equivalent source code.

## Denotational Semantics

Operational semantics ignores side effects, looking only at the inputs
(say, from _stdin_) and the outputs (say, to _stdout_). _Denotational
semantics_ is kinda part-way between logical semantics and operational
semantics. In denotational semantics, you define a mapping between your
programming language and some mathematical model (e.g. a finite-state
machine or an abstract algebra or a category). In that way, each program
written in your language is identified with some mathematical entity in
the model. The particular mathematical entity that a given program maps
to is considered to be the intended interpretation of that program.

Two programs are _denotationally equivalent_ relative to a given model
if they both map to the same mathematical entity in that model. In other
words, two programs are denotationally equivalent if they are the same
entity in the intended interpretation. Contrast that to logical
equivalence, which requires the programs be the same for all possible
interpretations.

## The "M" Word

Moggi pointed out that if your denotational model is a certain kind of
category (a cartesian-closed category), then you could use monads in the
model category (not in the programming language itself) to promote
certain classes of side effects to first-class mathematical entities in
that model [[1][1]]. This allows you to formally model and study side
effects and develop a notion of equivalence of side-effectual programs
that is more fine-grained than operational semantics while being less
restrictive than logical semantics.

It wasn't long before a couple of zealots took Moggi's ideas about using
monads in the meta-language a step further by using monads in the formal
language, adding them as first-class features of their "too cute for its
own good" toy PL, Haskell [[2][2], [3][3]].

## A Simple Language

Here's a simple language:

{% highlight none %}
    <digit> ::= "0" | "1" | ... | "9"
   <digits> ::= "" | <digit> <digits>
  <literal> ::= <digit> <digits>
    <value> ::= "input" | "time" | <literal>
<operation> ::= "sleep" | "store"
<statement> ::= <operation> <value>
              | "output"
              | "rm -rf /"
{% endhighlight %}

A program written in this language is a sequence of statements, one per
line. Here's a very simple one:

{% highlight none %}
store time
sleep input
store time
output
{% endhighlight %}

`input` gets the first eight bytes of stdin and reads them as a 64-bit
unsigned int, ignoring any extra bytes. It is read at program start and
is immutable thereafter. If at program start there are fewer than eight
bytes in stdin, then the missing bytes are treated as 00000000.

`time` gets the current system time, in millis since epoch, each time it
is used.

`store n` mutates an eight-bit stored value by reading its value as a
64-bit unsigned int and then adding `n` to it (overflows are allowed and
silent, no data consistency requirements here).

`sleep n` causes the program to wait for `n` millis before continuing.

`output` prints the currently-stored eight bytes of stdout.

`rm -rf /` does the unthinkable.

Since operational equivalence requires that program outputs be the same
whenever the inputs are the same, the following program

{% highlight none %}
store time
output
{% endhighlight %}

is not operationally equivalent to itself, since we can cause it to
produce a different output while feeding it the same input simply by
running the program at different times.

Perhaps more importantly, the two programs

{% highlight none %}
store 5
output
{% endhighlight %}

and

{% highlight none %}
store 5
rm -rf /
output
{% endhighlight %}

are operationally equivalent (since deleting everything does not put
bytes on stdout), even though we'd probably prefer they not be.

## A Simple Denotational Semantics

We'll give a denotational semantics for our language, writing the model
in Haskell.

{% highlight haskell %}
data ProgramState = ProgramState {
  _input  :: Word,
  _clock  :: Word,
  _store  :: Word,
  _output :: [Word],
  _burnIt :: Bool
  }

store :: Word -> ProgramState -> ProgramState
store n w = w { _store = _store w + n }

sleep :: Word -> ProgramState -> ProgramState
sleep n w = w { _clock = _clock w + n }

input :: ProgramState -> Word
input w = _input w

time :: ProgramState -> Word
time w = _clock w

output :: ProgramState -> ProgramState
output w = w { _output = _output w ++ [_store w] }

rmrf :: ProgramState -> ProgramState
rmrf w = w { _burnIt = True }

data Init = Init { _stdin :: ByteString, _millis :: Word }

type Program = Init -> ProgramState
{% endhighlight %}

Now we can define a mapping from programs written in our simple
language to values of type `Program` in our Haskell model. This allows
us to treat two programs as denotationally equivalent (relative to our
model) if they both get mapped to the same `Program` value.

A few things to note:

First, since there's no way to define an `Eq` instance for functions,
Haskell won't be able to automatically prove denotational equivalence of
parsed programs for us. However, since we now at least have a sensible
definition of program equivalence, we could still prove the equivalence
of two programs (relative to our model) on paper ourselves, if we had
to. Or if there's no time for that, we could always use QuickCheck.

Second, I keep saying cryptic things like "relative to our model." This
is to emphasize that the Haskell model we wrote is by no means the only
valid model for our simple language. In fact, while it is an improvement
over the operational semantics (now we can _prove_ that a program
doesn't pave the root directory), it's still flawed. For instance, our
model treats all operations besides `sleep` as though they take no time
to complete. While this is perhaps reasonable for our purposes, keep in
mind that a given denotational model is not necessarily the only
reasonable model of a programming language.

## References

  1. E. Moggi, Notions of computation and monads.
  _Information and Computation_, July 1991.
  [http://www.sciencedirect.com/science/article/pii/0890540191900524][1].

  [1]: http://www.sciencedirect.com/science/article/pii/0890540191900524

  2. S. Peyton Jones, P. Wadler, Imperative functional programming.
  _ACM Symposium on Principles Of Programming Languages (POPL)_, Jan 1993.
  [http://www.microsoft.com/en-us/research/wp-content/uploads/1993/01/imperative.pdf][2].

  [2]: http://www.microsoft.com/en-us/research/wp-content/uploads/1993/01/imperative.pdf

  3. _[Haskell-cafe] A backhanded compliment and a dilemma_.
  [http://mail.haskell.org/pipermail/haskell-cafe/2016-October/125281.html][3]

  [3]: http://mail.haskell.org/pipermail/haskell-cafe/2016-October/125281.html
