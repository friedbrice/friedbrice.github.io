---
layout: post
title: "6-by-6 Sudoku Solver"
date: 2015-02-07
author: Daniel
permalink: /blog/06/
tags:
- code
- haskell
- puzzles
---

Last weekend, I needed to quickly find all the solutions to a 6-by-6
sudoku in 2-row-by-3-column blocks
([eg](http://www.google.com/search?q=6+by+6+sudoku)).
There's an abundance of sudoku solvers online, for 9-by-9 sudokus.
I decided it'd be an instructive use of my time to modify an existing
solver to operate on the 6-by-6 variety, using, of course, Haskell.

<!--break-->

The solver I chose to modify is the almost-ten-year-old
[Haskell sudoku solver in 707 bytes][1]. I chose this solver
mostly because of its small size: there'd be fewer moving parts to
understand. This first thing I tried, in order to turn a 9-by-9 sudoku
solver into a 6-by-6 sudoku solver, was a naive <code>s/9/6/g</code>.
It compiled, but it didn't work.

  [1]: http://web.math.unifi.it/~maggesi/haskell_sudoku_solver.html

Of course, it didn't work because it wasn't checking 2-by-3 blocks
corrently!

The original source is devoid of comments, so the second thing I did
was annotate the code heavily. You can find my fully-annotated version
on [GitHub][2], but here's the original with only a few annotations
added, for brevity:

  [2]: http://github.com/friedbrice/Haskell/blob/master/sudoku.hs

{% highlight haskell linenos %}
import Data.List
import System.IO

type T = (Int,Int) -> [Int]
-- where (i,j) is a position, T (i,j) is a list of possible entries.

main = do
  s <- getContents
  putStr $ unlines $ map disp $ solve [input s]

solve :: [T] -> [T]
-- iterates mark on multiple Ts.
solve s = foldr search s idx where
    search p l = [mark (p,n) s | s <- l, n <- s p]

mark :: ((Int,Int),Int) -> T -> T
-- this is where the magic happens.
-- ((i,j),n) is a known entry on our sudoku board.
-- s is a type T, and as above, it returns lists of possible entries.
-- mark takes s and pars it down, based on our known ((i,j),n).
mark (p@(i,j),n) s q@(x,y) =
  if p==q then [n] else
  -- if q's entry is already known, do nothing
  if x==i || y==j || e x i && e y j then delete n $ s q else s q
  -- if q shares a row, col, or 3x3 block with p, then q can't be n
  where e a b = div (a-1) 3==div (b-1) 3

disp :: T -> String
-- takes results and turns it into something show-able.
disp s  = unlines [unwords [show $ head $ s (i,j) | j <- [1..9]] | i <- [1..9]]

input :: String -> T
-- takes input and turns it into something usable.
input s = foldr mark (const [1..9]) $
  [(p,n) | (p,n) <- zip idx $ map read $ lines s >>= words, n>0]

idx :: [(Int,Int)]
-- global constant, acts as the domain of members of T.
idx = [(i,j) | i <- [1..9], j <- [1..9]]
{% endhighlight %}

The crux of the block issue is on lines 24 and 26. I pulled those out
of the definition of mark and made them a separate function, named
sameBlock. Then I modified it to check for 2-by-3 blocks instead of
3-by-3 blocks. Below is my version, modified to solve 6-by-6 sudokus
with 2-by-3 blocks. Notice line 18 in the definition of
<code>sameBlock</code>.

{% highlight haskell linenos %}
import Data.List (lines, unlines, words, unwords, delete)
import System.IO (getContents, putStr)

type T = (Int,Int) -> [Int]

idx :: [(Int,Int)]
idx = [(i,j) | i <- [1..6], j <- [1..6]]

myInit :: T
myInit = const [1..6]

input :: String -> T
input s = foldr mark myInit $
  [(p,n) | (p,n) <- zip idx $ map read $ lines s >>= words, n>0]

sameBlock :: (Int,Int) -> (Int,Int) -> Bool
sameBlock (i,j) (x,y) =
  div (x-1) 2 == div (i-1) 2 && div (y-1) 3 == div (j-1) 3

mark :: ((Int,Int),Int) -> T -> T
mark (p@(i,j),n) s q@(x,y) =
  if p == q then [n]
  else if x == i || y == j || sameBlock p q then delete n . s $ q
  else s q

solve :: [T] -> [T]
solve s = foldr search s idx
  where search p l = [mark (p,n) s | s <- l, n <- s p]

disp :: T -> String
disp s  = unlines [unwords [show $ head $ s (i,j) | j <- [1..6]] | i <- [1..6]]

main :: IO ()
main = do
  s <- getContents
  putStr . unlines . map disp . solve $ [input s]
{% endhighlight %}

This version behaves as expected. The next step is to use this sudoku
solver to analyze a puzzle my friends and I designed for the Puzzle
Potluck Iron Puzzler competition. We'll look at that puzzle in the
next post.

