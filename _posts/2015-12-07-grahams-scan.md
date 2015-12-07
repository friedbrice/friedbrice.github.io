---
layout: post
title: "Graham's Scan Convex Hull Algorithm"
date: 2015-12-07
author: Daniel
permalink: /blog/11/
tags:
- code
- haskell
- math
- geometry
- convex hull
- graham scan
- Real World Haskell
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
  [3]: http://en.wikipedia.org/wiki/Graham_scan
  [4]: http://en.wikipedia.org/wiki/Convex_hull

<!--break-->

**Input:** A finite list of points \\( (x_i, y_i) \\) in the plane.

_Sample Input:_ \\( \\{ (0,0), (0.5, 1), (1,0), (1,1), (0,1), (-10,-10) \\} \\).

**Output:** The (ordered) list of vertices of the smallest convex
polygon containing all of the input points.

_Sample Output:_ \\( \\{ (-10,-10), (1,0), (1,1), (0,1) \\} \\).

By _ordered_ we mean that traversing the list of vertices should be
analogous to taking a walk around the outside edge of the polygon,
without cutting through the interior.

**Strategy:**

1.  Find the leftmost point on the input list (using lowest
    \\(y\\)-coordinate to break ties). Call it \\(b\\).

2.  Order the remaining input points by the slope formed by the line
    joining said point with \\(b\\) (using lower \\(x\\)-coordinate
    to break ties).

    This is where the algorithm gets its name. We chose the leftmost
    point \\(b\\) as a pivot, and then we scan across the plane by
    pivoting a line through \\(b\\) across the plane, using this
    sweeping motion to order the points of the input list.

    ![img1](nopic.png)
    ![img2](nopic.png)

3.  Starting with \\(b\\) and taking three points at a time, determine
    determine if a left turn or a right turn is made at the second point
    in order to proceed to the third point.

    ![img3](nopic.png)
    ![img4](nopic.png)

4.  If a left turn is make, then the middle point is extremal, and thus
    is a vertex of the convex hull. If a right turn is made, then the
    middle point is interior to the convex hull, so throw the middle
    point away.

    ![img5](nopic.png)
    ![img6](nopic.png)

5.  Continue removing points until what remains produces only left
    turns.

    ![img7](nopic.png)
    ![img8](nopic.png)

Let's take a look at the code. The whole file is 72 lines, and you can
[see it][5] in its entirety on GitHub, but we dissect it below.

  [5]: http://github.com/friedbrice/RealWorldHaskell/blob/master/ch3/exB12.hs

{% highlight haskell linenos %}
import Data.List (sortBy)

-- | Type for encoding relative direction.
data Direction = GoLeft | GoStraight | GoRight | GoBackwards | GoNowhere
                 deriving (Eq, Read, Show)

-- | Type for encoding points in the Cartesian plane.
data Point = Point Double Double
             deriving (Eq, Read, Show)

xProj :: Point -> Double
xProj (Point x _) = x

yProj :: Point -> Double
yProj (Point _ y) = y
{% endhighlight %}

Relative directions are hard. Our type for describing them includes
possibilities arising from bad/confusing data. For representing points,
we could have simply used `type Point = (Double, Double)`, but giving
them a distinct data constructor doesn't hurt. We also write some
convenience functions for pulling coordinates off of a `Point`.

{% highlight haskell linenos %}
reverseDictSort :: [Point] -> [Point]
-- ^ Sorts by leftmostness (picking lowest in a tie)
reverseDictSort = do
  sortBy (\p1 p2 -> compare (yProj p1) (yProj p2))
  sortBy (\p1 p2 -> compare (xProj p1) (xProj p2))

slopeSort :: Point -> [Point] -> [Point]
-- ^ Sorts a list of points by the slope they form with the given point.
slopeSort (Point x y) = sortBy (\p1 p2 -> compare (slope p1) (slope p2))
  where slope p = (yProj p - y) / (xProj p - x)
{% endhighlight %}

`reverseDictSort` and `slopeSort` implement steps one and two described
above.

`reverseDictSort` sorts on \\(y\\)-coordinate first, then on
\\(x\\)-coordinate, so the result will be a list sorted by \\(x\\)
first, then by \\(y\\), turning this sample input
\\[ \\{ (0,0), (0.5, 1), (1,0), (1,1), (0,1), (-10,-10) \\} \\]
into this
\\[ \\{ (-10,-10), (0,0), (0,1), (0.5, 1), (1,0), (1,1) \\}\text{.} \\]

With this sample data, we get \\(b = (-10,-10)\\). `slopeSort` will sort
\\( \\{ (0,0), (0,1), (0.5, 1), (1,0), (1,1) \\} \\) by the slope formed
with \\(b\\), resulting in
\\[ \\{ (1,0), (0,0), (1,1), (0.5, 1), (0,1) \\}\text{.} \\]

The meat of the algorithm is the function that determines relative
direction among three points.

{% highlight haskell linenos %}
direction :: Point -> Point -> Point -> Direction
-- ^ Given points p1, p2, and p3, returns the direction to p3 when
--   traveling from p1 through p2.
--   Relative directions are hard.
--   After checking a few edge cases, we do a coordinate transformation
--   that makes the problem easy to check.
direction p1@(Point x1 y1) p2@(Point x2 y2) p3@(Point x3 y3) = do
  if p1 == p2 || p2 == p3 -- Silly input.
  then GoNowhere
  else if p1 == p3 -- Silly input.
  then GoBackwards
  else do
    -- Here, we do a coordinate transformation that moves p1 to the
    -- origin and moves p2 to (1,0). This puts p3' somewhere in the
    -- plane, and then we branch on the trichotomy of y3'.
    let det = (x2 - x1) ** 2 + (y2 - y1) ** 2
        a   = (x2 - x1) / det
        b   = (y2 - y1) / det
        c   = - (y2 - y1)
        d   = x2 - x1
        x3' = a * (x3 - x1) + b * (y3 - y1)
        y3' = c * (x3 - x1) + d * (y3 - y1)
    if y3' > 0 -- p3' is in the upper half plane.
    then GoLeft
    else if y3' < 0 -- p3' is in the lower half plane.
    then GoRight
    else if x3' > 1 -- p3' is on the x-axis, beyond (1,0).
    then GoStraight
    else GoBackwards -- p3' is on the x-axis behind (1, 0).
{% endhighlight %}

I've annotated the code itself,
so I hope that explains most of its inner workings.
There's not much else I can add without repeating myself.
It's worth mentioning that the [Wikipedia article][3] on Graham's scan
suggests a shorter method for assigning direction.
The method they give, however, misses a few edge cases, and accounting
for those edge cases results in a function no less complicated as
mine above.

Finally, we have `grahamScan`, which implements the algorithm.

{% highlight haskell linenos %}
grahamScan :: [Point] -> [Point]
-- ^ Returns the vertices of the convex hull of the input list.
grahamScan input = walkPerimeter sorted
  where
    (b : bs) = reverseDictSort input -- sort by coordinates
    sorted   = (b :) . slopeSort b $ bs -- Sort by slope
    walkPerimeter (p1 : p2 : p3 : ps) =
      -- exmine three points at a time
      if direction p1 p2 p3 == GoLeft
      then p1 : (walkPerimeter (p2 : p3 : ps)) -- GoLeft -> p1 is good
      else walkPerimeter (p1 : p3 : ps) -- not GoLeft -> p2 is bad
    walkPerimeter ps = ps -- base case, fewer than three points -> end
{% endhighlight %}

On line 4, we find \\(b\\). Line 5 sorts the remaining points by slope,
and pushes \\(b\\) back on to the top of the result. At this point, we
will have in hand a list that looks like this:
\\[ \\{ (-10,-10), (1,0), (0,0), (1,1), (0.5, 1), (0,1) \\}\text{.} \\]

Finally, we take a walk along the perimeter, throwing away points
that don't form a left turn. We take a look at the first three entries,
and if we get anything other than `GoLeft`, we throw away the second
point and start over. If we get `GoLeft`, then we peel the first point
out of the loop and continue with the remaining points.

\\[ \mathrm{walkPerimeter} ((-10,-10) : (1,0) : (0,0) : (1,1) : (0.5, 1) : (0,1)) \\]
Since \\( \mathrm{direction} ((-10,-10), (1,0), (0,0)) \equiv
\mathrm{GoLeft} \\), we peel peel off \\( (-10,-10) \\).
\\[ \equiv (-10,-10) : (\mathrm{walkPerimeter} ((1,0) : (0,0) : (1,1) : (0.5, 1) : (0,1))) \\]
Since \\( \mathrm{direction} ((1,0), (0,0), (1,1)) \equiv
\mathrm{GoRight} \\), we delete \\( (0,0) \\).
\\[ \equiv (-10,-10) : (\mathrm{walkPerimeter} ((1,0) : (1,1) : (0.5, 1) : (0,1))) \\]
\\( \mathrm{direction} (1,0), (1,1), (0.5,1) \equiv \mathrm{GoLeft} \\),
so peel off \\( (1,0) \\).
\\[ \equiv (-10,-10) : (1,0) : (\mathrm{walkPerimeter} ((1,1) : (0.5, 1) : (0,1))) \\]
\\( \mathrm{direction} (1,1), (0.5,1), (0,1) \equiv
\mathrm{GoStraight} \\), so delete \\( (0.5,1) \\).
\\[ \equiv (-10,-10) : (1,0) : (\mathrm{walkPerimeter} ((1,1) : (0,1))) \\]
Only two points left, keep them both.
\\[ \equiv (-10,-10) : (1,0) : (1,1) : (0,1) \\]

Let's try it out. Load the file into ghci.

{% highlight haskell %}
ghci> let input = [Point 0 0, Point 0.5 1, Point 1 0, Point 1 1, Point 0 1, Point (-10) (-10)]
ghci> grahamScan input
[Point (-10.0) (-10.0),Point 1.0 0.0,Point 1.0 1.0,Point 0.0 1.0]
ghci> let input = [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 2 2]
ghci> grahamScan input
[Point 0.0 0.0,Point 2.0 2.0,Point 0.0 3.0]
{% endhighlight %}
