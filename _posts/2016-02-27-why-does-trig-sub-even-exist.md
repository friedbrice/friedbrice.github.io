---
layout: post
title: "Why does trig-sub even exist?"
date: 2016-02-27
permalink: /blog/14/
comments: true
tags:
- calculus
- integration
- math
- trigonometry
---

I was writing an exam for my students, and I wanted to give them a trig
sub integral that wasn't too difficult (there's this infinitesimal sweet
spot between ridiculously too difficult and utterly trivial). I noticed
something about virtually every single trig sub integral that I imagined
or ran into -- almost all of them could be solved without any trig.

Case in point: consider \\( \int \frac{x^3}{\sqrt{x^2 + 1}} \, dx \\)

<!--break-->

Standard trig sub methods have us use \\( x = \tan(t) \\) since
\\( x^2 + 1 \\) appears in our integral. Making said substitution, we
solve the integral as follows:

$$
\begin{align*}
  \int \frac{x^3}{\sqrt{x^2 + 1}} \, dx
  &= \int \frac{\tan^3(t)}{\sqrt{\tan^2(t) + 1}} \, dx
  & x &= \tan(t)
  \\ &= \int \frac{\tan^3(t)}{\sqrt{\tan^2(t) + 1}} \sec^2(t) \, dt
  & dx &= \sec^2(t) \, dt
  \\ &= \int \frac{\tan^3(t)\sec^2(t)}{\sec(t)} \, dt
  \\ &= \int \tan^3(t)\sec(t) \, dt
  \\ &= \int (\sec^2(t) - 1)\sec(t)\tan(t) \, dt
  \\ &= \int (u^2 - 1)\sec(t)\tan(t) \, dt
  & u &= \sec(t)
  \\ &= \int u^2 - 1 \, du
  & du &= \sec(t)\tan(t) \, dt
  \\ &= \frac{1}{3} u^3 - u + C
  \\ &= \frac{1}{3} \sec^3(t) - \sec(t) + C
\end{align*}
$$

To simplify further, we need to draw a right triangle that satisfies
\\( x = \tan(t) \\), and from that triangle deduce that
\\( \sec(t) = (x^2 + 1)^{1/2} \\), giving us

$$
  \int \frac{x^3}{\sqrt{x^2 + 1}} \, dx
  = \frac{1}{3} (x^2 + 1)^{3/2} - (x^2 + 1)^{1/2} + C
$$

Sometimes, you just gotta do what you gotta do, and so this is all well
and good in the name of science. But what if I told you there was a way
to solve this integral that involved no trig whatsoever?

The approach we can use is back substitution. Instead of setting
\\( x = \tan(t) \\), we'll let \\( u = x^2 + 1 \\).

$$
\begin{align*}
  \int \frac{x^3}{\sqrt{x^2 + 1}} \, dx
  &= \int \frac{x^3}{\sqrt{u}} \, dx
  & u &= x^2 + 1
  \\
  &= \frac{1}{2} \int \frac{x^2}{\sqrt{u}} \, du
  & du &= 2x \, dx
  \\
  &= \frac{1}{2} \int \frac{u - 1}{\sqrt{u}} \, du
  & x^2 &= u - 1
  \\
  &= \frac{1}{2} \int \frac{u}{\sqrt{u}} - \frac{1}{\sqrt{u}} \, du
  \\
  &= \frac{1}{2} \int u^{1/2} - u^{-1/2} \, du
  \\
  &= \frac{1}{2} \left( \frac{2}{3}u^{3/2} - 2u^{1/2}\right) + C
  \\
  &= \frac{1}{3}u^{3/2} - u^{1/2} + C
  \\
  &= \frac{1}{3}(x^2 + 1)^{3/2} - (x^2 + 1)^{1/2} + C
\end{align*}
$$

At first I thought that this must simply be a fluke coincidence, but as
I looked at more problems, this method kept working.

It seems as thought we can get every integral that is traditionally
taught using trig substitution by back substitution instead, so long as
we just remember a few basic templates:

$$
\begin{align*}
  \int \frac{1}{\sqrt{A^2 - (Bx)^2}} \, dx
  &= \frac{1}{AB}\sin^{-1}(Bx/A) + C
  \\
  \int \frac{1}{A^2 + (Bx)^2} \, dx
  &= \frac{1}{AB}\tan^{-1}(Bx/A) + C
  \\
  \int \frac{1}{Bx\sqrt{(Bx)^2 - A^2}} \, dx
  &= \frac{1}{AB}\sec^{-1}(Bx/A) + C
\end{align*}
$$

Frankly, we don't even have to remember the templates, since we can get
them with the \\( u \\)-substitution \\( u = Bx \\) and a little bit of
algebra.

Now, trig is hard for students to grasp. It takes a lot of time away
from what could otherwise be used to teach quantitative reasoning,
logic, statistics, etc. On top of that, Calculators and Computer Algebra
Systems do all of your trig for you. This simply motivates the question:

> Why do we keep putting so much effort into teaching trig when a
> calculator can do it better than a person and the time could be used
> on something more relevant?

For a long time, I thought the answer was obvious. We need to teach
trigonometry because once you get into Calc II, you'll have some
integrals that you'll _need_ trig to solve, and by then it'd be too late
to start teaching trig. But seeing how trig-substitution is basically
unnecessary as an integration technique, I have to wonder -- why do we
even teach trig-sub? And if we don't need trigonometry for trig-sub,
then why do we even still teach trigonometry at all?

We could realign the entire Mathematics curriculum. Teach just the
basics of how to evaluate trig functions using a calculator or a table,
and make advanced trig a special topics course for the few people who do
actually still need to understand it really well. We could free up
hundreds of hours for more relevant material in the process.

Now, I'm not really quite that radical, so I'm interested to hear in the
comments why trigonometry (beyond a basic understanding of how to
evaluate trig functions and what they tell us) should stay part of the
core Calculus curriculum (maybe those of you who teach/do Physics know
why). Or if you can think of an integral that actually does _require_
trig-sub to solve (aside from the trivial integration technique of
guessing the solution and checking by differentiating) I'd be interested
in seeing it.
