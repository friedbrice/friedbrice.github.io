---
layout: post
title: "A decent and reliable Haskell dev setup for VS Code in 2022"
date: 2022-07-18
permalink: /blog/haskell-vscode/
redirect-from:
  - /blog/2022-07-18/
comments: true
tags:
  - haskell
  - programming
  - tooling
---

The sickest experience possible for Haskell development in July 2022 is undoubtedly [VS Code](https://code.visualstudio.com/) with the [Haskell plugin](https://github.com/haskell/vscode-haskell) powered by [Haskell Language Server](https://hackage.haskell.org/package/haskell-language-server) (HLS).
When HLS works for your project.
This guide will demonstrate a simpler, lower-featured alternative that hopefully is reliable enough to work with your project in those cases where HLS does not.

<!-- break -->

## Synopsis

HLS is superb and provides a rich Haskell experience that genuinely boosts productivity.
However, there are various reasons why HLS might not work with a particular Haskell project. The project might be built using a version of GHC that HLS doesn't support.
The project might be very `TemplateHaskell`-heavy or otherwise just too damn big for HLS to function with acceptable latency.
That said, progress continues on HLS (and on GHC) to make it faster and more stable, with the goal that we'll all be able to use it for all of our projects.

In the mean time, we still would like some of the luxuries our text editors are capable of providing when given a suitable source of information.
Following this guide, you'll get:

- Basic code suggestions (i.e. autocomplete) based on textual analysis of your source file.

- Goto definition for top-level symbols and modules within your project.

- Project-specific Hoogle search and Haddock documentation for your project and its dependencies (and transitive dependencies).

- Problems reporting.
  Compiler errors and warnings will appear in both the _Problems_ view in VS Code and as inline squiggly underlines in your source code.

Finally, we'll discus tying these features together into a cohesive, convenient workflow.

### Why VS Code?

This guide is for VS Code, because that's the editor I know and use.
I'm sure you can get similar features for Emacs and the various Vis using the approaches described here, though.
If you do, drop a comment and link to your blog post :-D

### Why Stack?

I rely on [Stack](https://docs.haskellstack.org/en/stable/README/) to reduce the number of dev tools I need to manage manually.
Alternatively, the same setup should work if you install the necessary tools yourself (at the correct versions for your specific project).
Or if you know of Cabal commands that more-or-less correspond to the Stack commands I use, then these workflows should still work just fine.

You can use Stack for these editor integrations even if you don't use Stack to build your project.
In your project root directory (rather, the directory that has the Cabal file defining your package), use `stack init` to create a _stack.yaml_ file.
Check [stackage.org](https://stackage.org) and find your version of GHC on the list _Latest releases per GHC version_; Click into that.
Somewhere on the page, you'll see something that looks more or less like

{% highlight yaml %}
resolver: lts-18.28
{% endhighlight %}

or

{% highlight yaml %}
resolver: nightly-2022-07-18
{% endhighlight %}

Replace the `resolver` property in your _stack.yaml_ file with the one you found on Stackage.
You should be good to go, unless you're on Apple Silicon.
Stack will work on Apple Silicon, but it takes a bit of finagling (please read on).

### Stack on Apple silicon {#stack-apple-silicon}

Because of incompatibilities between GHC versions, Stack treats GHC as a project dependency: Stack will download and use the version of GHC appropriate to your project.
However, the GHC binaries that Stack currently distributes are incompatible with Apple M1 (and presumably M2) chips.
This is a known issue, and it will hopefully be fixed soon.

Meanwhile, in order to get around this, you'll need to configure Stack to use the GHC binary you installed from [GHCUp](https://www.haskell.org/ghcup/).

1. Make sure GHCUp's _bin_ directory is included in your shell's `PATH` variable.

   For example, on my Mac OS 12.4 system, GHCUp's _bin_ directory is _/Users/daniel/.ghcup/bin_, and this directory appears near the front of my `PATH` variable.

2. Use GHCUp to _install_ and _set_ the version of GHC you need for your project.

3. Add a _stack.yaml_ file to your project root if there isn't one already.

4. In _stack.yaml_, add the following top-level properties:

   {% highlight yaml %}
   system-ghc: true
   install-ghc: false
   skip-ghc-check: true
   {% endhighlight %}

Stack will now use the GHC binary provided by GHCUp.

## Basic code suggestions

VS Code ships with a rudimentary form of code suggestions (a.k.a. autocomplete) built-in.
It's not very smart, being driven by a simple textual analysis of your source file, but it's better than nothing.

Here are VS Code settings that work for me.
You might want to tweak some of them to fit your preferred workflow.

{% highlight json %}
{
  "editor.acceptSuggestionOnCommitCharacter": false,
  "editor.acceptSuggestionOnEnter": "smart",
  "editor.quickSuggestions":
  {
    "comments": "inline",
    "other": "inline",
    "strings":  "inline"
  },
  "editor.quickSuggestionsDelay": 2000,
  "editor.snippetSuggestions": "none",
  "editor.suggest.filterGraceful": false,
  "editor.suggest.preview": true,
  "editor.suggest.showStatusBar": true,
  "editor.suggest.showWords": true,
  "editor.suggestOnTriggerCharacters": false,
  "editor.suggestSelection": "first",
  "editor.wordBasedSuggestionsMode": "matchingDocuments"
}
{% endhighlight %}

With these settings, you'll see the top suggestion as ghost text.
You can apply the suggestion by hitting the _Tab_ key.
You can see the full list of suggestions by hitting _Control_+_Space_.

I have these settings in my user-level VS Code settings, so that they apply to every workspace.
(_Workspace_ is just the word VS Code uses for what I've been calling your project).
Alternatively, you might want these settings to apply only in specific workspaces.
If so, create a _.vscode/settings.json_ in your project and put these properties there.

## Goto definition

This feature depends on the [ctagsx](https://marketplace.visualstudio.com/items?itemName=jtanx.ctagsx) VS Code plugin.

We will use a Haskell tool called [Hasktags](https://hackage.haskell.org/package/hasktags) to generate a file named _tags_.
The _tags_ file contains symbol information from your project source code.
_ctagsx_ relies on this file to provide Goto definition functionality in VS Code.

It's not as great as what you get with HLS.
HLS is able to index all of the defined entities in your source code, including local variables.
Hasktags is only capable of indexing module-level definitions, but that's better than nothing.
It indexes types, classes, and variables---whether or not they're exported---and it indexes modules.
Right click on the entity you want to chase, and pick _Goto Definition_.

Caveat: to jump to a module you'll need to select the whole module name before you right click.
For example, to be taken to the source code for a module named `Foo.Bar` you need to select that entire module name---from `F` to `r`---before right clicking.

All you need to do is install the _ctagsx_ plugin and generate a _tags_ file for your source code.

**Generate _tags_ file:**

{% highlight shell %}
stack exec hasktags -- --ctags .
{% endhighlight %}

If you want an up-to-date index, you'll need to regenerate your _tags_ file every time you modify your code.
Obviously, doing this manually would be very tedious.
We'll automate it later.

## Hoogle and Haddock

This feature depends on the [vscode-goto-documentation](https://marketplace.visualstudio.com/items?itemName=cxfksword.goto-documentation) plugin.

By now we should have Goto Definition working for the types, functions, classes, and modules in our project.
Unfortunately, Goto Definition won't work for entities defined in our dependencies.
The next best thing, though is having Hoogle search and Haddock documentation for them.
(And Haddock includes source code, so we kinda do get Goto Definition in a roundabout way.)

**Build initial Hoogle database (also builds docs):**

{% highlight shell %}
stack hoogle --rebuild
{% endhighlight %}

**Start Hoogle server (server will listen at _localhost:8080_):**

{% highlight shell %}
stack hoogle --server &
{% endhighlight %}

You'll now have Hoogle and Haddock for your project and its dependencies when you open your browser to _localhost:8080_.

As with our _tags_ file, we'll have to update Hoogle's database every time our source code changes.
Fortunately, we can do this while the Hoogle server is running.
This needs to be done every time we change a source file. (so we'll automate it, later).

**Hot-rebuild Hoogle database:**

{% highlight shell %}
stack hoogle -- generate --local <package_name_of_your_project>
{% endhighlight %}

When we're done working for the day, we'll want to tear down our server to free up Port 8080.

**Kill Hoogle server:**

{% highlight shell %}
pkill hoogle
{% endhighlight %}

I know what you're about to say.
Opening our browser, navigating to _localhost:8080_, and keying in the name of a function or type is laborious.
Fortunately, we can get all this with just a right click once we configure our plugin appropriately.
Create a _.vscode/settings.json_ in your project if there isn't one already, and add the following property:

{% highlight json %}
{
  "goto-documentation.customDocs": {
    "hs": "http://localhost:8080/?hoogle=${query}"
  }
}
{% endhighlight %}

You can now right-click on a function, class, module, or type in your source code and pick _Goto Document_ to get taken to the Hoogle results.
This works both for symbols we get from our dependencies and symbols we've defined ourselves.
The best part is we don't even need a live internet connection.

## Problems reporting

This feature depends on the [haskell-ghcid](https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid) VS Code plugin.

Here's the real meat of our faux-IDE setup.

[Ghcid](https://hackage.haskell.org/package/ghcid) is a simple, reliable file watcher that spawns a Ghci session and presses `:r` for you (the _reload_ command) whenever a Haskell source file changes.
It's reliable precisely because it has no notion of projects, build tools or text editors.
It relies simply on Ghci.
It'll even let you choose how to start Ghci, so that all of your dependencies will be registered and all of your modules will be in scope.

(I don't know why I felt the need to explain all that. It's hard to imagine anyone is writing Haskell and doesn't know about Ghcid.)

In the old days, we were forced to use Ghcid in a crude way.
We'd have it running in a terminal, side-by-side with our editor.
Today, we have [editor plugins](https://github.com/ndmitchell/ghcid/tree/master/plugins) for Emacs, Neovim, and VS Code.
Go install the plugin, you'll be happy you did!

The plugin is very simple.
It reads a file, named _ghcid.txt_.
It assumes the file contains info on compiler errors and warnings, and it parses that info and feeds it into VS Code's problems reporting API.
Your errors and warnings show up in VS Codes _Problems_ pane (which provides Jump-to-source-location functionality) and inlined into your source code as squiggly underlines.

Your responsibility here is to bootstrap this whole process by starting Ghcid in a terminal and then forget about it.
You'll need to use the `--outputfile` flag to tell Ghcid that it should be writing info to _ghcid.txt_.

**Start Ghcid with file output:**

{% highlight shell %}
stack exec ghcid -- --command 'stack repl --ghc-options "-fno-code -ignore-dot-ghci -ferror-spans"' --outputfile ghcid.txt
{% endhighlight %}

Replace `stack repl ...` with whatever command you use to load your project into Ghci.
For example, if you use Cabal to build your project, it might be something like this

{% highlight shell %}
stack exec ghcid -- --command 'cabal repl --ghc-options "-fno-code -ignore-dot-ghci -ferror-spans"' --outputfile ghcid.txt
{% endhighlight %}

The most important thing here is `--outputfile ghcid.txt`.
This Ghcid option it to write errors and warnings to _ghcid.txt_, where our VS Code plugin is expecting to find them.

The second most important thing here is `-fno-code -ignore-dot-ghci -ferror-spans`.
These options tell Ghci that it should only be type checking (for faster reloads) and that it should be reporting full source spans for errors (so your editor can make squiggles).
`-ignore-dot-ghci` is more superstitious than anything, but it's in there because sometimes your `.ghci` file can mess up your project repl.
(If you rely on a _.ghci_ file to load your project repl, then you probably already know enough about Ghci to know that you should overrule my suggestion and omit that option.)

Now, just minimize your terminal and pull your editor up.
Chasing compiler errors has never been easier!

## Putting it all together

Our problems reporting (and squiggles!) are powered by Ghcid running in a minimized terminal somewhere.
Well, Ghcid has a little-known feature that allows you to run an arbitrary Ghci command every time it reloads (so, every time a source file in your project changes).
We'll use this feature to automate the re-indexing of our _tags_ file and our Hoogle database.
This Ghcid feature is controlled by the `--test` option.

But we don't want to run a Ghci command, we want to run a couple shell commands.
Well, Ghci has a little-known feature that allows you to run arbitrary shell commands.
At the Ghci prompt, type `:!` and then a space and then any shell command.
Try it with something innocuous, like `ls`

{% highlight plaintext %}
ghci> :! ls
{% endhighlight %}

That's rather delightful!

So, when we invoke Ghcid, we'll use the `--test` option to get Ghci to run the shell commands that re-index our _tags_ file and Hoogle database.

Here's everything conveniently in a shell script:

{% highlight shell %}
# start-ide.sh
stack hoogle --rebuild # build Hoogle database for our dependencies
stack hoogle --server 2> /dev/null 1> /dev/null & # Hoogle on localhost:8080
(stack exec ghcid -- \ # on file saves...
  --command 'stack repl --ghc-options "-fno-code -ignore-dot-ghci -ferror-spans"' \ # type-check
  --test ':! stack exec hasktags -- --ctags . && stack hoogle -- generate --local ${your_package_name}' \ # re-index source info
  --outputfile ghcid.txt) || true # write errors and warnings to ghcid.txt
(pkill ghcid && echo "Killed Ghcid.") || echo "No Ghcid process to kill." # superstitiously cleanup ghcid process (ctrl+c should have done it, though)
(pkill hoogle && echo "Killed Hoogle.") || echo "No Hoogle process to kill." # really cleanup Hoogle process
{% endhighlight %}

(You can find the name of your package in your project's Cabal file.)

So, when I go sit down to work, I open a terminal to my project directory and I run `./start-ide.sh`.
Then I minimize that terminal and forget about it.
I open my project in VS Code, and I have

- basic code suggestions;
- goto definition for my types, classes, functions, and modules;
- goto documentation for all types, classes, functions, and modules;
- errors and warnings in VS Code's _Problems_ pane; and
- inline squiggles for errors and warnings.

And I have it all reliably, whether or not HLS works for my project.

## Summary

Here are the steps written as tersely as possible, free of commentary, while still being totally complete.
Some uses will be able to skip some of these steps.

**Initial Setup:**

1. If on an Apple M1 or M2 system (optional otherwise), install GHCUp and:

   1. Use it to install a version of GHC appropriate for your project.

   2. Configure your shell so that GHCUp's bindir is on your `PATH`.

2. Install Stack.

3. Install _ctagsx_ VS Code plugin.

4. Install _vscode-goto-documentation_ plugin.

5. Install _haskell-ghcid_ VS Code plugin.

6. Create _.vscode/settings.json_ in your project directory with these contents:

   ```
   {
     "editor.acceptSuggestionOnCommitCharacter": false,
     "editor.acceptSuggestionOnEnter": "smart",
     "editor.quickSuggestions":
     {
       "comments": "inline",
       "other": "inline",
       "strings":  "inline"
     },
     "editor.quickSuggestionsDelay": 2000,
     "editor.snippetSuggestions": "none",
     "editor.suggest.filterGraceful": false,
     "editor.suggest.preview": true,
     "editor.suggest.showStatusBar": true,
     "editor.suggest.showWords": true,
     "editor.suggestOnTriggerCharacters": false,
     "editor.suggestSelection": "first",
     "editor.wordBasedSuggestionsMode": "matchingDocuments",
     "goto-documentation.customDocs": {
       "hs": "http://localhost:8080/?hoogle=${query}"
     }
   }
   ```

7. If your project doesn't have a _stack.yaml_ file, create one with `stack init`, and

   1. Check stackage.org to find the latest resolver that matches your version of GHC.

   2. Change the `resolver` property in _stack.yaml_ to the resolver you just found.

8. If on an Apple M1 or M2 system, add these lines to your _stack.yaml_:

   ```
   system-ghc: true
   install-ghc: false
   skip-ghc-check: true
   ```

9. Create an executable script named _start-ide.sh_ in your project dir with contents:

   ```
   stack hoogle --rebuild # build Hoogle database for our dependencies
   stack hoogle --server 2> /dev/null 1> /dev/null & # Hoogle on localhost:8080
   (stack exec ghcid -- \ # on file saves...
     --command 'stack repl --ghc-options "-fno-code -ignore-dot-ghci -ferror-spans"' \ # type-check
     --test ':! stack exec hasktags -- --ctags . && stack hoogle -- generate --local ${your_package_name}' \ # re-index source info
     --outputfile ghcid.txt) || true # write errors and warnings to ghcid.txt
   (pkill ghcid && echo "Killed Ghcid.") || echo "No Ghcid process to kill." # superstitiously cleanup ghcid process (ctrl+c should have done it, though)
   (pkill hoogle && echo "Killed Hoogle.") || echo "No Hoogle process to kill." # really cleanup Hoogle process
   ```

**Every time you work on your project:**

1. Run `./start-ide.sh` in a terminal and minimize it.
