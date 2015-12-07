---
layout: post
title: "Graham Scan Convex Hull Algorithm"
date: 2015-12-06
author: Daniel
permalink: /blog/11/
tags:
- code
- haskell
- math
- geometry
- convex hull
- graham scan
---

Was spending my free time working through [Real World Haskell][1] by
O'Sullivan, Stewart, and Goerzen.
I can appreciate how many of the exercises end up being very mathy.
For instance, the [Chapter 3][2] exercises culminate in an
implementation of [Graham's scan][3] algorithm for finding the
[Convex Hull][4] of a finite set of points in the plane.
Here's my implementation.

  [1]: http://book.realworldhaskell.org/
  [2]: http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
  [3]: https://en.wikipedia.org/wiki/Graham_scan
  [4]: https://en.wikipedia.org/wiki/Convex_hull

<!--break-->

*Input:* A finite list of points \\( (x_i, y_i) \\) in the plane.
_For Example:_ \\( \{ (0,0), (1,0), (1,1), (0,1), (-10,-10) \} \\).

Output: The (ordered) list of vertices of the smallest regular polygon
containing all of the input points.
_For Example:_ \\( \{ (-10,-10), (1,0), (1,1), (0,1) \} \\).
