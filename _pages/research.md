---
layout: page
title: Research
permalink: /research/
date: 2014-12-03
topnav: true
---

My research interests include [Lie algebras][] and [representation
theory][], [linear algebra][] and [matrix][] theory, and related topics.
Specifically, I've done work in derivations of Lie algebras and zero
product determined algebras.

  [Lie algebras]: http://en.wikipedia.org/wiki/Lie_algebra
  [representation theory]: http://en.wikipedia.org/wiki/Representation_theory
  [linear algebra]: http://en.wikipedia.org/wiki/Linear_algebra
  [matrix]: http://en.wikipedia.org/wiki/Matrix_(mathematics)

* [Derivations of parabolic Lie algebras](#derivations)
* [Zero product determined algebras](#zpd)
* [List of publications](#publications)
* [List of presentations](#presentations)

<h2 id="derivations">Derivations of parabolic Lie algebras</h2>

A _derivation_ on a Lie algebra \\( L \\) is a map \\( f: L \to L \\)
satisfying

\\[
  f\big([x,y]\big) = \big[f(x), y\big] + \big[x, f(y)\big]
\\]

for all \\( x, y \in L \\).

Huajun Huang and I proved that the derivations of a parabolic Lie
algebra (a Lie algebra that is realized as parabolic subalgebras of some
reductive Lie algebra) are the sums of inner derivations and linear maps
into the center that kill the derived algebras.

Explicitly, given a parabolic Lie algebra \\( L \\), the derivations
algebra \\( \mathrm{Der} (L) \\) decomposes as the direct sum of ideals

\\[
  \mathrm{Der} (L) = \mathrm{ad} (L) \oplus \mathcal L
\\]

where

\\[
  \mathcal L = \left\\{ f: L \to L ; f(L) \subseteq Z(L), f([L,L]) = 0 \right\\}.
\\]
We are presently preparing these results for publication
([article](#brice0000derivations)).

<h2 id="zpd">Zero product determined algebras</h2>

An algebra \\( A \\) is _zero product determined_ if for each bilinear map
\\( \varphi: A \times A \to V \\) satisfying

\\[
  \varphi(x, y) = 0 \text{ whenever } xy=0
\\]

there is a linear map \\( f: A^2 \to A \\) such that

\\[
  \varphi(x, y) = f(xy)
\\]

for all \\( x, y \in A \\).

Huajun Huang and I wrote a paper on general zero product determined
algebras that will appear in _Linear and Multilinear Algebra_ in 2015,
and we're currently working on a second paper concerning specifically
zero product determined Lie algebras. I'll go into more details some
time in the future.

<h2 id="publications">List of Publications</h2>

A selection of my peer-reviewed and submitted research articles.

<ul>
  {% assign pubs = site.data.publications | sort: 'date' | reverse %}
  {% for pub in pubs %}
    <li>
      <a href="{{ site.baseurl }}/assets/publications/{{ pub.file }}">
        {{ pub.title }}
      </a>
      · {{ pub.date | date: "%b, %Y" }}<br />
      <em>{{ pub.description }}</em>
    </li>
  {% endfor %}
</ul>

<h2 id="presentations">List of Presentations</h2>

Selected conference and seminar presentations.

<ul>
  {% assign press = site.data.presentations | sort: 'date' | reverse %}
  {% for pres in press %}
    <li>
      <a href="{{ site.baseurl }}/assets/presentations/{{ pres.file }}">
        {{ pres.title }}
      </a>
      · {{ pres.date | date: "%b, %Y" }}<br />
      <em>{{ pres.description }}</em>
    </li>
  {% endfor %}
</ul>