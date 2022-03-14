---
layout: post
title: "Sets, Classes, and Wabbits"
date: 2022-03-14
permalink: /blog/sets-classes-and-wabbits/
redirect-from:
  - /blog/2022-03-14/
comments: true
tags:
  - math
  - logic
  - set theory
---

Set theory is a bit of a trickster.
Some of its ideas go back to Plato (forms) and Aristotle (categorical syllogisms), but sets don't quite capture the objects of these disciplines.
Rather, the objects of those disciplines are captured more closely by the concept of _classes_ (as in _classifications_).
Sets vs. classes?
What's the difference?
Why split hares?
This blog post will go into how Set Theory fails to capture our intuitive notions around classification.

<!-- break -->

We intuitively classify things, drawing comparisons and making distinctions.
Questions about a platonic form are best understood as questions about the class---not necessarily set---of all things that meets some defining criteria.
Similarly, Categorical Syllogism is the logic of inferring class---not necessarily set---membership based on logical predicates.
We'd like a mathematical theory of sets that corresponds to our intuitive notions of classification.
That is, we'd like to be able to take an arbitrary logical predicate and then form the set of all things that satisfy said predicate.
Unfortunately, such a vision is untenable, as its unrestricted application leads to paradox.

>  How about a nice, close shave? / Teach your whiskers to behave. / Lots of lather, lots of soap. / Please hold still, don't be a dope. / Now we're ready for the scraping / There's no use to try escaping. / Yell and scream and rant and rave. / It's no use, you need a shave! -- The Rabbit of Seville

In Seville, Spain there lives and works a sole barber, Conejito.
Now, Seville is a rather big city, with the better part of a million sevillians.
It'd be absurd to think that Conejito is every sevillian's barber.
Instead, Conejito is barber to those sevillians, and only those sevillians, who are not their own barber.
So then, who is Conejito's barber?
Either Conejito is his barber or he is his own barber.
If Conejito is his barber, then that just means that Conejito is Conejito's barber.
That's fine, broadly speaking a person can be their own barber, but is there any reason why Conejito in particular can't be his own barber?
There is a reason.
An established facts about Conejito is that he is barber to a sevillian if and only if that sevillian is not their own barber.
If someone is their own barber, then the _only if_ part entails that Conejito can't be that person's barber.
Applied to Conejito, that means Conejito can't be his own barber.
So that's fine, we've established that Conejito can't be his own barber.
Except now, the _if_ part entails that Conejito is the barber of any sevillian that isn't their own barber.
In particular, this would make Conejito his own barber.
Adopting either alternative leads us immediately to the other alternative, yet they are exclusive alternatives, and so we have no viable resolution.
This is to say that the very idea of there being a person such as Conejito is self-contradictory.

This might sound scary and bad at first, but it's really not a big problem at all.
It's certainly not the paradox I alluded to earlier.
We could have seen Conejito's self-contradictory nature formally.
Let \\(B(u,v)\\) be the proposition that \\(u\\) is \\(v\\)'s barber.
Then,
\\[ (\forall x) \big( B(y,x) \iff \neg B(x,x) \big) \\]
is the proposition that \\(y\\) is barber to everyone who is not their own barber.
Assume for contradiction that at least one such barber exists,
\\[ (\exists y) \Big( (\forall x) \big( B(y,x) \iff \neg B(x,x) \big) \Big) \\]
Now, let \\(c\\) stand for one such barber in particular, so we have
\\[ (\forall x) \big( B(c,x) \iff B(x,x) \big) \text{.} \\]
Since this statement applies generally to all \\(x\\), it applies in particular when \\(x\\) is \\(c\\), yielding
\\[ B(c, c) \iff \neg B(c, c) \text{.} \\]
At this point, the contradiction should be apparent, but let's finish the proof just out of caution. Splitting the bi-implication, we have
\\[ \big( B(c, c) \implies \neg B(c, c) \big) \wedge \big( \neg B(c, c) \implies B(c,c) \big) \text{.} \\]
Applying [Material Implication](https://en.wikipedia.org/wiki/Material_implication_(rule_of_inference)) to both conjuncts gives
\\[ \big( \neg B(c, c) \vee \neg B(c, c) \big) \wedge \big( B(c, c) \vee B(c,c) \big) \\]
which reduces to
\\[ \neg B(c, c) \wedge B(c, c)\text{,} \\]
a contradiction, whereby we conclude the negation of our assumption,
\\[ \neg (\exists y) \Big( (\forall x) \big( B(y,x) \iff \neg B(x,x) \big) \Big) \text{.} \\]

All this amounts to is the fact that the class of all sevillians who are barber to those, and only those, sevillains who aren't their own barber is empty.
This is not a paradox, it's simply an observation.
\\[ \Big\\{ y \\,\Big\vert\\, (\forall x) \big( B(y,x) \iff \neg B(x,x) \big) \Big\\}  = \emptyset \\]

Now we're ready to begin the main course. Let's assume we have a coherent theory of sets, where we can take any logical predicate and form a set consisting of all entities in the universe that satisfy said predicate (_ie._ imagine we have unrestricted set comprehension).

Consider in particular the predicate that \\(x\\) is not a member of the set \\(y\\), \\(x \notin y\\).
For various reasons that I won't get into today, we'd like to consider something like a subuniverse of all entities that are well-behaved in the following sense.
Specifically, we'd like to only consider sets that do not contain themselves.
Thus, it's reasonable to consider the set of all sets that do not contain themselves, taking that to be our favored subuniverse.
\\[ U = \\{ x \\,\vert\\, x \notin x \\} \\]

The question we now face: is \\(U\\) a member of \\(U\\)?
By \\(U\\)'s definition, we find that \\(U\\) is a member of \\(U\\) if and only if \\(U \notin U\\).
\\[ U \in U \iff U \notin U \text{.} \\]

We've seen this exact logical form before, when we saw why Conejito can't exist.
In the same way, it tells us that no such set \\(U\\) can exist.
This is not to say that \\(U\\) is empty.
If \\(U\\) were merely empty we'd be disappointed, but it wouldn't necessarily be a paradox.
But the situation we have here is far more subtle.
We see that the set \\(U\\) itself can't exist, empty or otherwise, in exactly the same way that Conejito can't exist: nothing that satisfies the logical form
\\[ \varphi(x,x) \iff \neg \varphi(x,x) \\]
can exist no matter what the predicate \\(\varphi\\) may be (as we demonstrated above, in fact).

This flies in the face of our desire to be able to form sets corresponding to arbitrary logical predicates, showing us that no coherent system of set theory will correspond exactly to our intuitive notion of classes.
The solution---which I might write up in a subsequent blog post one of these days---is to be very careful and precise about what sets you allow yourself to form.
We end up relying on a few basic constructions and then build up our universe of sets inductively (_ie._ recursively) from there.
