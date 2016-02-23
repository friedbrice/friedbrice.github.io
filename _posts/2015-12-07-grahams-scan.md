---
layout: post
title: "Graham&#8217;s Scan Convex Hull Algorithm"
date: 2015-12-07
permalink: /blog/11/
comments: true
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

**Input:** A finite list of points \\( (x,y) \\) in the plane.

_Sample Input:_ `(0,0), (0.5, 1), (1,0), (1,1), (0,1), (-10,-10)`

**Output:** The (ordered) list of vertices of the smallest convex
polygon containing all of the input points.

_Sample Output:_ `(-10,-10), (1,0), (1,1), (0,1)`

By _ordered_ we mean that traversing the list of vertices should be
analogous to taking a counterclockwise walk around the outside edge of
the polygon, without cutting through the interior.

**Strategy:** By _extremal point_ we mean a point in the input set that
is a vertex of the final convex polygon. Our task is to identify all of
the extremal points and order them.

We first find an obviously-extremal point, then we use a method not
unlike ray tracing to order the remaining points. Once the remaining
points are ordered, we can determine which points are extremal by
taking a walk along the points, making sure we only ever take left
turns (by throwing away points when we don't make a left).

1.  Remove any duplicates from the input list.

2.  Find the leftmost point on the input list (using lowest
    \\(y\\)-coordinate to break ties). Call it `b`.
    The description of `b` makes it clear that `b` is extremal.

3.  For each of the remaining points `p`, find the slope and
    distance between `b` and `p`. If multiple `p`s form the
    same slope with `b`, then obviously the ones closer to `b` can't
    be extremal, so only keep the furthest.

    At this step, we worry about what will happen to points directly
    above `b`, since the slope will be undefined. Haskell's `Double`
    implementation will treat these slopes as `Infinity`.

4.  Order the remaining points by increasing slope. Then append `b`
    to the front of the ordered list. We now have a list `ps` of
    points that starts with `b`, is ordered by increasing slope,
    and contains at most one point for each slope.

    A quick check in GHCi shows `(1.0 / 0.0) > 1.0` evaluates to `True`,
    so if there were any points directly above `b`, the furthest
    such point will be the last entry of `ps`.

5.  Now, we take a walk along our list, looking at three consecutive
    points at a time. For each triple of points `p1`, `p2`, `p3` we
    imagine that we travel from `p1` through `p2` to `p3`,
    and we calculate the direction we turn at `p2`.

    Since our list is ordered in increasing slope, anything other than a
    left turn at `p2` indicates that `p2` can't be extremal, so
    in that case we throw it away. If we do make a left turn at
    `p2`, then because of how our list is ordered `p2` is
    extremal, and we can then move on to considering the trip from
    `p2` through `p3` to `p4`, and so on.

Let's take a look at the code. The whole file is 98 lines, and you can
[see it][5] in its entirety on GitHub, but we dissect it below.

  [5]: http://github.com/friedbrice/RealWorldHaskell/blob/master/ch3/exB12.hs

First, imports and a general convenience function.

{% highlight haskell linenos %}
import Data.List (groupBy, sortBy)

removeDuplicates :: Eq a => [a] -> [a]
-- ^ Why isn't this in Prelude?
removeDuplicates = foldr skipIfElem []
  where
    skipIfElem x ys = if x `elem` ys
                      then ys
                      else x : ys
{% endhighlight %}

`removeDuplicates` does exactly what it sounds like it does.

> **Edit:** The function I'm looking for is `nub` from `Data.List`.

Next we introduce a datatype for points in the plane and a few
functions that we'll need later.

{% highlight haskell linenos %}
-- | Type for encoding points in the Cartesian plane.
data Point = Point { xProj :: Double, yProj :: Double }
             deriving (Eq, Read)

-- | Show instance displays points in usual mathematical notation.
instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

p :: (Double, Double) -> Point
-- ^ Function for creating points in usual mathematical notation.
p (x, y) = Point x y

slope :: Point -> Point -> Double
-- ^ Calculates the slope between two points.
slope (Point x1 y1) (Point x2 y2) = (y2 - y1) / (x2 - x1)

norm :: Point -> Point -> Double
-- ^ Calculates the taxicab distance between two points.
norm (Point x1 y1) (Point x2 y2) = abs (x2 - x1) + abs (y2 - y1)

coordinateSort :: [Point] -> [Point]
-- ^ Sorts by lowest x-coord, then by lowest y-coord.
coordinateSort = sortBy (\(Point x1 _) (Point x2 _) -> compare x1 x2)
               . sortBy (\(Point _ y1) (Point _ y2) -> compare y1 y2)
{% endhighlight %}

> **Edit:** Using a type alias, such as `type Point = (Double, Double)`
> instead of a `data` declaration
> allows us to use the default `Ord`, `Read`, and `Write` instances,
> greatly simplifying the code. These changes are reflected on the
> GitHub version of `exB12.hs`.

We could implement this algorithm using angle instead of slope, but
slope is easier to calculate and ends up being equivalent. _I.e._ if we
were to calculate the angle that each point forms with the ray moving in
the positive \\(x\\) direction eminating from a fixed point, we'd end up
needing to calculate `atan` of the slope, anyway, and since `atan` is
monotonic, sorting on `atan . slope` is the same as sorting on `slope`,
so why compute `atan`? Likewise, we chose to use the [taxicab metric][6]
instead of the usual distance formula, since the taxicab metric is
easier to calculate (we avoid `sqrt`) and results in the same sort.

  [6]: http://en.wikipedia.org/Taxicab_geometry

Next we define a datatype for relative directions and a function that
discerns the direction we need to turn at `p2` in order to proceed to
`p3` from `p1`.

{% highlight haskell linenos %}
data Direction = GoLeft | GoStraight | GoRight | GoBackwards | GoNowhere
                 deriving (Eq, Read, Show)

direction :: Point -> Point -> Point -> Direction
-- ^ Given points p1, p2, and p3, returns the direction to turn at `p2`
--   when traveling from `p1` through `p2` to `p3`.
direction p1@(Point x1 y1) p2@(Point x2 y2) p3@(Point x3 y3) = do
  -- Check a few easy edge cases.
  if p1 == p2 || p2 == p3 -- direction is not well-defined
  then GoNowhere
  else if p1 == p3 -- direction is backwards
  then GoBackwards
  else do
    -- Here, we do a coordinate transformation that moves `p1` to the
    -- origin and moves `p2` to (1,0). This puts `p3'` somewhere in the
    -- plane, and then we branch on the trichotomy of `y3'`.
    let det = (x2 - x1) ** 2 + (y2 - y1) ** 2
        a   = (x2 - x1) / det
        b   = (y2 - y1) / det
        c   = - (y2 - y1)
        d   = x2 - x1
        x3' = a * (x3 - x1) + b * (y3 - y1)
        y3' = c * (x3 - x1) + d * (y3 - y1)
    if y3' > 0 -- `p3'` is in the upper half plane
    then GoLeft
    else if y3' < 0 -- `p3'` is in the lower half plane
    then GoRight
    else if x3' > 1 -- `p3'` is on the x-axis, beyond (1,0)
    then GoStraight
    else GoBackwards -- `p3'` is on the x-axis behind (1, 0)
{% endhighlight %}

Lines 17 through 23 perform a coordinate transformation on `p3`. We
could pull the coordinate transformation out of `direction` and make it
a distinct top-level function, but I don't see any reason to at this
point.

Wikipedia [suggests][7] embedding our points in three space
and using the \\(z\\)-coordinate of the cross
product of the vector \\(\vec{p_1 p_2}\\) with the vector
\\(\vec{p_2 p_3}\\) to determine direction.
That approach works, but it misses a good number of edge cases,
and taking care of those edge cases results in a function that's
not any simpler than the one at hand.

  [7]: http://en.wikipedia.org/wiki/Graham_scan#Algorithm

We're ready for the algorithm itself:

{% highlight haskell linenos %}
grahamScan :: [Point] -> [Point]
-- ^ Takes a list of points in the plane and returns the vertices of
--   the smallest convex polygon containing the input points, ordered
--   counterclockwise around the perimeter.
grahamScan input = walkPerimeter . sort $ input
  where
    sort ps = (b :)
            . map (\(p,_,_) -> p)
            -- ^ forget slopes and norms, and push `b`
            . map last
            -- ^ take the furthest from each slope group
            . map (sortBy (\(_,_,n1) (_,_,n2) -> compare n1 n2))
            -- ^ sort each slope group by norm
            . groupBy (\(_,s1,_) (_,s2,_) -> s1 == s2)
            -- ^ group by slope
            . sortBy (\(_,s1,_) (_,s2,_) -> compare s1 s2)
            -- ^ sort by slope
            . map (\p -> (p, slope b p, norm b p)) $ bs
            -- ^ calculate slope and norm for remaning points
      where
        (b : bs) = coordinateSort . removeDuplicates $ ps
        -- ^ remove duplicates and find the lowest-leftmost point `b`
    walkPerimeter (p1 : p2 : p3 : ps) =
      -- ^ examine three points at a time
      --   notably, assumes p1 is good
      if direction p1 p2 p3 == GoLeft
      then p1 : (walkPerimeter (p2 : p3 : ps)) -- GoLeft -> p2 is good
      else walkPerimeter (p1 : p3 : ps) -- not GoLeft -> p2 is bad
    walkPerimeter ps = ps -- base case, fewer than three points -> end
{% endhighlight %}

The algorithm flows through two phases: `sort` massages our input data
into a form that is easy to recurse on, `walkPerimeter` process the
sorted data, three points at a time.

I've annotated `sort` and `walkPerimeter`, and you'll see they
implement the five-step strategy described at the start of this post.
One thing to remember while reading the annotations, though: _read from
bottom to top_. This is functional language, and `sort` is a composition
of a bunch of functions. The rightmost (here, lowest) function is the
first to touch our input data.

`walkPerimeter` assumes that its first argument is a point that we
intend to keep (it has no way of deleting the first point it's fed),
and this is okay since we know `b` is an extremal point.

Let's give it a test drive in GHCi.

{% highlight text %}
ghci> let ps = [p(0,0), p(0.5,1), p(1,0), p(1,1), p(0,1), p(-10,-10)]
ghci> grahamScan ps
[(-10.0,-10.0),(1.0,0.0),(1.0,1.0),(0.0,1.0)]
ghci> let ps = [p(0,0), p(0,1), p(0,2), p(0,3), p(1,1), p(2,2), p(0.5,2)]
ghci> grahamScan ps
[(0.0,0.0),(2.0,2.0),(0.0,3.0)]
{% endhighlight %}

Let's noise-up our data a little and see what happens.

{% highlight haskell linenos %}
noiseEm :: [Point] -> [Point]
noiseEm = concatMap noiseIt
  where
    noiseIt (Point x y) = [Point (x + e) (y + e) | e <- [-0.1, 0, 0.1]]
{% endhighlight %}

{% highlight text %}
ghci> let ps = [p(0,0),p(0,1),p(0,2),p(0,3),p(1,1),p(2,2),p(0.5,2)]
ghci> grahamScan ps
[(0.0,0.0),(2.0,2.0),(0.0,3.0)]
ghci> grahamScan . noiseEm $ ps
[(-0.1,-0.1),(2.1,2.1),(0.6,2.1),(0.4,1.9),(0.1,3.1),(-0.1,2.9)]
{% endhighlight %}

I'd like to write some robust some testing suites,
but I don't really know how.
Anyway, that's enough for tonight.
