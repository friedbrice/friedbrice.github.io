---
layout: post
title: "Daniel's Day"
date: 2015-01-28
permalink: /blog/daniels-day/
redirect_from:
  - /blog/03/
  - /blog/2015-01-28/
comments: true
tags:
  - bash
  - haskell
  - jekyll
---

It's been a while since I've posted.
Busy winter break, and just now getting to a place where where I'm on top of things in the new semester.
Yesterday, I wanted to get two things done (aside from real work):

1. Find some professional Math [mailing lists](#mailing-lists) to join.
2. Get [posting on my blog](#blogging) again, and maybe even work on some of the design elements.

<!--break-->

# Mailing Lists

So, I'm trying to learn to code.
Aside from one semester of qBASIC in high school and one semester of Javascript in undergrad, I have no formal experience with programming, but I know I can learn and I want to demonstrate it.
The languages I want to start out with are Haskell and C (I know, I'm a glutton for punishment).

So... any simple task, such as googling Math mailing lists, becomes a beginning programming homework exercise.

I found that I wanted to try about 60 different search phrases, and I found that they all had the same basic form:

{% highlight haskell %}
{ place } + " " + { subject } + " mailing list"
{% endhighlight %}

I could make a double-nested loop in C... But this is the perfect time to use Haskell list comprehension!
So, I made my *first ever* Haskell program:

{% highlight haskell %}
-- search_terms.hs
places = [ "Southern California"
         , "California"
         , "West Coast"
         , "Western"
         , "Southwest"
         , "South West"
         ]

subjects = [ "Mathematics"
           , "Algebra"
           , "Linear Algebra"
           , "Lie Theory"
           , "Lie Algebras"
           , "Lie Groups"
           , "Representation Theory"
           , "Matrix"
           , "Category Theory"
           , "Homological Algebra"
           ]

searches = [i ++ " " ++ j ++ " mailing list" | i <- places, j <- subjects]

main = mapM_ print searches
{% endhighlight %}

Compiling gives an executable called `search_terms`.
`search_terms` returns one search phrase per line (wrapped in quotes for some reason) which I could output to a plain text file and then copy and paste each line into Google.
But why would I go to *all that work* if I could automate it in Bash!

A while ago, I made a crappy little Bash script that lets me google things from the command line.
Here's an abridged version:

{% highlight bash %}
#!/bin/bash

# web_search.sh
# takes all arguments and opens search page in a new chromium window

# invoking will search google by default
# -w will search wikipedia

function google_search {
    search_string=$(echo ${call_arg} | sed -e 's/ /+/g')
    echo "search_string=${search_string}"
    chromium-browser --app=http://www.google.com/search?q=${search_string}
    exit
    }

function wikipedia_search {
    search_string=$(echo $* | sed -e 's/ /+/g')
    echo "search_string=${search_string}"
    chromium-browser --app=http://en.wikipedia.org/w/index.php?search=${search_string}
    exit
    }

call_arg=$(echo $*)
echo "call_arg=${call_arg}"

while getopts ":w:m" opt; do
    case ${opt} in
        :)
            echo "Made it to google_search"
            google_search
            ;;
        w)
            echo "Made it to wikipedia_search"
            wikipedia_search ${OPTARG}
            ;;
        \?)
            echo "Invalid options: -${OPTARG}"
            echo "Exiting"
            exit 1
            ;;
        esac
    done

echo "Didn't trigger any of the cases. Defaulting to google_search"
google_search $*
{% endhighlight %}

Example: `web_search.sh -w "hello world"` will search Wikipedia with the search term "hello world".
It's a little buggy: I'd like to be able to not wrap the search term in quotes, but that breaks it.
However, `search_terms` returns strings pre-wrapped in quotes.
Prefect!

Now, I just have to loop over the lines of `search_terms`'s output.

{% highlight bash %}
while read x; do
  web_search.sh $x
done < $(search_terms)
{% endhighlight %}

This will open 60 browser windows, each one with a different search phrase.\
That was fun!

# Blogging

It's been a while since I've edited this blog, so I had to relearn some Jekyll basics.
In particular, `jekyll build` and `jekyll serve` don't work the way I expected them to.
`jekyll build` spits out a version error, in fact.

After some frustration and googling, I relearned that I needed to use [Bundler](http://bundler.io/) to instance my Ruby environment.
This will keep the versions of Ruby and Jekyll I'm using to build my site synchronized with the versions that GitHub Pages is using to build my site.
A few of the common commands are recorded below, mostly so I can come back here two months from now when I've forgotten everything again:

1. Keep Bundler, Jekyll, and Ruby up to date.

{% highlight nosyntax %}
bundle update
{% endhighlight %}

2. Build site.

{% highlight nosyntax %}
bundle exec jekyll build
{% endhighlight %}

3. Run local development server.

{% highlight nosyntax %}
bundle exec jekyll serve
{% endhighlight %}

Okay, now that that's recorded for posterity, my next project involves some custom Jekyll plugins that GitHub Pages doesn't support.
In order to use them, I'll need to turn off server-side Jekyll, build my site locally, and push the built site up to GitHub Pages.
That's a project for another day (read: *another month*).
