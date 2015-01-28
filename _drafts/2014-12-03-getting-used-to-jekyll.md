---
layout: post
title: "Getting Used to Jekyll"
date: 2014-12-03
#categories: math update
author: Daniel
permalink: /blog/00/
tags: code jekyll yaml liquid
---

Hokay, I'm starting to make my site more robust.

I rearranged the directory structure of the blog, so that all blog posts
live in /blog/ordinal_number/index.html. I like that a lot better than
the default, where the year is a directory, each month is a directory,
each day is a directory, and finally the blog post page is a file in
there---What a mess. I think it's much cleaner now: each blog post is
it's own directory in the master /blog/ directory.

I've also added several top-level pages. I now have an [About Me page][ap],
a [Research page][rp], and a [Teaching page][tp]. This is going to be a
wonderful site!

  [ap]: {{ site.baseurl }}/about/
  [rp]: {{ site.baseurl }}/research/
  [tp]: {{ site.baseurl }}/teaching/

I've also been sold on the idea of using markdown. I'm writing my blog
posts and pages in markdown, and it's a major improvement over working
in HTML. Markdown is literate, like LaTeX, and it's such a pleasure to
create content in.

There are still a few things I need to learn:

* <strike>How to cross link between different pages (the links above are
  dead).</strike>[see below](#links)
* How to use the Liquid templating system. Particularly for
  algorithmically-generated content (like for my list of publications).
* How to use _data/*.yml as a poor-man's database (like for my list of
  publications).
* Why the date isn't working for my blog posts.

Once I pin those down, though, I'll be set to write my real website.
Then, I've still to learn how to use github pages. It never ends >_<;

<strong id="links">Edit:</strong> Figured out how to link to internal
pages. Use the syntax \{\{ baseurl \}\}/page_dir/ as the URL. It seems
like \{\{ site.url \}\}/page_dir/ also works. I need to figure out the
difference between these, by testing.

**Re-edit:** Okay, so, \{\{ baseurl \}\}/page_dir/ and /page_dir/ do the
same thing, since my baseurl is set to null in _config.yml. However,
\{\{ site.url \}\} refers to the url key in _config.yml, and so
\{\{ site.url \}\}/page_dir/ will evaluate to an absolute link. This
breaks the testing environment, so I supposed I will always use relative
links, and I think I'll use the "full" relative link instead of the
short relative link, for the sake of portability.

**Re-re-edit:** Okay, final (hopefully) update on the issue of links.
Use \{\{ site.baseurl \}\} to ensure that I'm using the value defined in
_config.yml.

