---
layout: post
title: "Haskell GoF - Abstract Factory"
date: 2022-01-19
permalink: /blog/abstract-factory/
redirect-from:
  - /blog/2022-01-19/
comments: true
tags:
  - haskell
  - java
  - design patterns
  - abstract factory
  - haskell gof
---

The beginning of a series wherein we show how to implement selected design patterns from
[Design Patterns: Elements of Reusable Object-Oriented Software](https://en.wikipedia.org/wiki/Design_Patterns). This week, we take a look at the venerable _Abstract Factory_ pattern.

<!-- break -->

A common question new Haskell programmers have is, "How do I implement _\<this-or-that>_ OOP design pattern in Haskell." These curious and resolute dames and gents are (quite reasonably) hoping to leverage their background in object-oriented programming towards their new goal of learning functional programming and Haskell. Unfortunately, it's not always straightforward to translate patterns that rely on the familiar notions of class hierarchies, inheritance, instantiation, virtual methods, collaborators, encapsulation, and mutation to the alien landscape of datatypes, typeclasses, parametricity, immutability, pattern matching, and higher-order functions that is Haskell. Worse, many a young and impressionable Haskellers develop misleading sets of analogies between OOP concepts and Haskell features, ultimately leading to confusion and frustration down the line. For example, Haskell typeclasses bear only _the most superficial resemblance_ to Java interfaces.

This series has two goals: (1) to provide new Haskell programmers with a menagerie of translations between familiar OOP code idioms and Haskell, and (2) to dispel dubious analogies between OOP languages features and Haskell language features by replacing them with well-founded and practical analogies. As the book of design patterns is often affectionately (and sometimes derisively) referred to as [Gang of Four](https://proxy.c2.com/cgi/wiki?GangOfFour), we'll unimaginatively call this series _Haskell GoF._

Expect new posts once every blue moon.

## Identifying the Pattern

We should begin our explorations with a shared understanding of the topic at hand. In particular, what makes something a _design pattern?_ The concept of a design pattern became popular in programming circles due to the broadly influential book on (brick and mortar) architectural design, [The Timeless Way of Building](https://en.wikipedia.org/wiki/The_Timeless_Way_of_Building) by Christopher Alexander, architect and emeritus professor at the University of California, Berkeley.

> Each pattern describes a problem which occurs over and over again in our environment, and then describes the core of the solution to that problem, in such a way that you can use this solution a million times over, without ever doing it the same way twice. -- Christopher Alexander

To paraphrase Dr. Alexander, a design pattern is an _outline of a solution_ to a common problem that _does not admit a generic solution._ In programming, we often come across a problem that does admit its own generic solution. Since the solution to such a problem is generic, we need only write the solution down once, package that solution into a library, and import the library wherever the need arises. Such solutions are not design patterns. A design pattern for a recurring programming problem is a solution template that resists codification.

Next, we need to understand why certain solutions (or outlines of solutions) can't be codified. One reason is that sometimes the problem itself is vague. Often, we can spot similarities in various coding problems---similarities that lead to similar but somewhat different solutions---without being able to identify a precise defining characteristic of such a class of problems. Thus, while a vague problem usually can't have a precise solution, it can have a vague solution. This vague solution becomes a design pattern.

Another reason a solution might not be realizable as a library is that we might be missing the right language features. There's lots of talk about the relative abstraction power of various programming languages. Really, this so-called power of abstraction is simply the ability to parametrize repeated syntactical forms by the sub-expressions by which those forms vary, enabling elimination of the syntactic repetition. (What else do you think a function is?) Through their features and syntactic rules, different languages enable and prohibit abstraction of various syntactical forms.[^lisp] The repeated syntactic forms that can't be abstracted become design patterns [instead of libraries](http://wiki.c2.com/?DesignPatternsAreMissingLanguageFeatures).

[^lisp]: Notice that the incredible facility for abstraction in Lisp has less to do with having copious language features (as Lisp has  hardly any) and more to do with the elegant simplicity of its syntax.

Before proceeding, there's one more point of terminology on which we should agree, since it shows up so frequently in discussions of patterns: the usage of the word _abstract._ Programmers tend to attach a peculiar meaning to the word _abstract_. They often mean "complicated implementation hidden behind a simple API," but this is not a description of something abstract. On the contrary, this is a description of something very real and concrete; we have a full implementation inside there, after all. A human body is quite complex inside, but the (non-lethal) ways in and out are clearly defined and simple, so should we call human bodies abstract? On top of that, programmers already have another word for this concept of implementation hiding: _encapsulation._ Let's not use the term _abstract_ to describe a complex, concrete system with a small, tidy surface when we already have a much more evocative word to describe it. _Abstract_ and _concrete_ are at odds, anyway.

Consider: what's abstract about abstract art? A central feature of abstract art is that it avoids depiction of identifiable, concrete objects. Instead, we have pure geometry of shapes, lines, brushstrokes, and color. Instead of an art piece having a definite meaning, different onlookers will be able to imagine different meanings. The piece is open to the interpretation of the onlooker, and this property cuts to the heart of what it means for something to be abstract. Indeed, _abstract_ means "open to interpretation."

![Dependency structures induced by encapsulation compared to the dependency structure induced by abstraction]({{ "/assets/img/encapsulation-vs-abstraction.png" | relative_url}}){: .center-image }

Abstraction helps us decouple our code in ways that encapsulation can't. On the left, we see the dependency structure we get when we encapsulate a third-party library. On the right, we abstract it. When we abstract it, fewer of our code units depend on the third-party library, decoupling our application code from our external dependencies.

Now that we agree (at least for the purposes of this discussion) on what our words mean---on what design patterns are and why we might need them and on the nature of abstraction more generally---we're ready to tackle our first pattern, the _Abstract Factory._

## Abstract Factory

> Provide an interface for creating families of related or dependent objects without specifying their concrete classes. -- GoF

Suppose we'd like to write an algorithm or build up a data structure using a bespoke set of primitives while allowing the interpretation of that algorithm or data structure into several different concrete forms. The Abstract Factory Pattern gives us a way to accomplish this.

That's a mouthful, so let's consider a practical example: graphical user interfaces (GUIs). Each software platform (Linux, Mac, Web, Windows, etc.) provides its own toolkit for rendering things to the screen. These toolkits don't necessarily have any commonalities between them, so if we want to write a cross-platform program, we're left to repeatedly implement our GUI for each platform we target. Alternatively, we can come up with our own set of primitives for describing our program's GUI. We can write more complicated components in terms of our primitives, and in turn combine those components to create our complete GUI, abstractly (that is, without reference to any of the concrete, platform-dependent toolkits). Then, instead of implementing every high-level component in each toolkit, we need merely define how each primitive (and each combining rule) is implemented in each of the various toolkits. We thus interpret the exact same abstract, platform-independent GUI code into concrete GUIs appropriate to each target platform. Qapla'!

Let's look at an implementation in classic Java.[^classic-java]

[^classic-java]: Java, first released in 1995, lacked type parameters before its fifth edition in 2004. This lack of type parameters had a profound impact on the nature of the code people wrote in Java. You wouldn't think so, but the lack of type parameters inevitably leads to the style of mutation-heavy, `void`-method-driven APIs we see in so many Java libraries today. When we give examples in this series, we'll stick to _classic Java,_ that is, Java without type parameters. This fits in better with the style of programming in GoF (published in 1994), which also lacks type parameters.

{% highlight java %}
interface Widget {}

class HtmlWidget implements Widget {
  void addStyle(String key, String value) {...}
  void addAttribute(String key, String value) {...}
}

class Text extends HtmlWidget {
  Text(String content) {...}
}

class Tag extends HtmlWidget {
  Tag(String tag, HtmlWidget[] children) {...}
}

class LatexWidget implements Widget {}

class BareText extends LatexWidget {
  BareText(String text) {...}
}

class Command extends LatexWidget {
  Command(String name, String[] parameters) {...}
}

class Environment extends LatexWidget {
  Environment(String name, String[] parameters, LatexWidget content) {...}
}

interface AbstractWidgetFactory {
  Widget column(Widget[] children);
  Widget row(Widget[] children);
  Widget image(String path);
  Widget text(String content);
  Widget heading(String content);
}
{% endhighlight %}

(The symbol `...` indicates details that we've omitted for brevity.)

Each of `HtmlWidget` and `LatexWidget` wraps[^wrapping] a third-party toolkit for one of our target platforms. Notice that they have very different APIs. Nevertheless, the differences in their APIs won't stop us from finding a mutual abstraction: we will still be able to make an implementation of `AbstractWidgetFactory` for both of them, despite their idiosyncrasies.

[^wrapping]: We have to wrap the third-party toolkits because `Widget` is our class, so they can't extend it. Later on, we'll see how to do this wrapping using the Adapter Pattern.

Before moving on, it's important to understand that `AbstractWidgetFactory` is not an (abstract widget) factory. Rather, it is an abstract (widget factory). In other words, it is not a concrete factory that produces hypothetical widgets; it hypothesizes a factory that produces real widgets. The factory itself is abstract, not the widgets. `AbstractWidgetFactory` is abstract in the sense that its methods are not implemented and must be given an interpretation by any implementing classes.[^interfaces]

[^interfaces]: Indeed, this was the case for all interfaces before Java's eighth edition. Interfaces are a mechanism for abstraction. As such, the `Abstract` in `AbstractWidgetFactory` is redundant. We already know it's abstract, because it's an interface. One can only presume that the GoF authors used this redundant naming convention as a pedantic device to drive home their points. Unfortunately, the obtusely-redundant naming convention became part of the Java culture, leading to such absurdities as "Since `ConcreteWidgetFactory` implements `AbstractWidgetFactory`, a `ConcreteWidgetFactory` is an `AbstractWidgetFactory`." No wonder programmers are so confused about the meaning of the word _abstract!_

![Dependency structure of our abstract factory example]({{ "/assets/img/abstract-factory-example.png" | relative_url }}){: .center-image }

Using the primitives provided by `AbstractWidgetFactory`, we can build up larger reusable components, and ultimately combine those components to build out our entire GUI. This can be accomplished without any knowledge of (e.g. without depending on) the various concrete subtypes of `Widget`. As such, we have a component that works with _any_ subtype of `Widget`---even ones that haven't been conceived of yet---so long as we're given a factory that will produce widgets of that type. In this way, we completely decouple the implementation of our GUI from the hardware toolkits our target platforms provide. Here's an example of one reusable component, a `table`.

{% highlight java %}
interface TableCol {
  Widget title();
  Widget getField(TableRow row);
}

interface TableRow {
  Widget field(TableCol col);
}

Widget table(AbstractWidgetFactory factory, TableCol[] cols, TableRow[] rows) {
  ArrayList rows0 = new ArrayList();

  ArrayList labels = new ArrayList();
  for (TableCol col : cols) {
    labels.add(col.title());
  }
  rows0.add(factory.row((Widget[]) labels.toArray()));

  for (TableRow row : rows) {
    ArrayList fields = new ArrayList();
    for (TableCol col : cols) {
      fields.add(row.field(col));
    }
    rows0.add(factory.row((Widget[]) fields.toArray()));
  }

  return factory.column((Widget[]) rows0.toArray());
}
{% endhighlight %}

Since `table` is written in terms of the `AbstractWidgetFactory` primitives, we will be able to use it generically with any of the platform toolkits by passing in different implementations of `AbstractWidgetFactory`. "Write once run anywhere" never felt so visceral!

Finally, let's see the implementation of `AbstractWidgetFactory` for `HtmlWidget`.[^latex]

[^latex]: I'm not going to implement `AbstractWidgetFactory` for `LatexWidget` in Java, because it's a huge pain in the ass. I will implement it in Haskell, though.

{% highlight java %}
class HtmlWidgetFactory implements AbstractWidgetFactory {

  public HtmlWidget column(Widget[] children) {
    for (Widget child : children) {
      ((HtmlWidget) child).addStyle("display", "block");
    }
    return new Tag("div", (HtmlWidget[]) children);
  }

  public HtmlWidget row(Widget[] children) {
    for (Widget child : children) {
      ((HtmlWidget) child).addStyle("display", "inline-block");
    }
    return new Tag("div", (HtmlWidget[]) children);
  }

  public HtmlWidget image(String path) {
    HtmlWidget img = new Tag("image", new HtmlWidget[]{});
    img.addAttribute("source", path);
    return img;
  }

  public HtmlWidget text(String content) {
    return new Tag("p", new HtmlWidget[]{new Text(content)});
  }

  public HtmlWidget heading(String content) {
    return new Tag("h2", new HtmlWidget[]{new Text(content)});
  }
}
{% endhighlight %}

One glaring flaw in this design is that we have to perform unsafe type casts from `Widget`s to `HtmlWidget`s in order to implement `column` and `row`. Nothing can stop us from accidentally passing `LatexWidget`s into an `HtmlWidgetFactory`, causing a runtime error that will crash our program. Indeed, the GoF authors are aware of and admit to this flaw. The flaw can be completely overcome with the use of type parameters, but lacking type parameters, the GoF authors suggest that this danger can be mitigated by ensuring that only one factory is ever in scope at a time and that all `Widget`s in your program be created from that one factory.[^scope]

[^scope]: The GoF authors suggest that the programmer accomplish this careful control of scope using the _Singleton_ pattern, which we will examine in a subsequent post.

And that's all there is to the Abstract Factory. We've accomplished our goal of writing our UI once against a set of abstract primitives (_i.e._ the methods of `AbstractWidgetFactory`), affording us the ability to interpret that UI against each of our concrete toolkits by simply interpreting the primitives (_i.e._ by implementing a concrete subclass of `AbstractWidgetFactory`). This is the crucial characteristic of the Abstract Factory pattern. Our operative code does not have a dependency on either `HtmlWidget` or `LatexWidget`, nor does it have a dependency on their respective factories. Neither does `AbstractFactory` have a dependency on either of `HtmlWidget` or `LatexWidget`. This is important.

As a point of contrast, we can imagine a code structure where we encapsulate `HtmlWidget` or `LatexWidget` (or both) behind an API that is more convenient for our immediate usage. This is a great practice, but it's not an abstract factory.[^adapter] Sometimes you want such a code structure, but for the problem we're considering today it's inadequate; it doesn't accomplish our goal of having platform-independent code. Our code would depend on the wrapper class, which in turn depends on the concrete, platform-dependent subclass(es) of `Widget` that the wrapper encapsulates. This restricts us to only those toolkits that the author of the wrapper class thought to include. It also forces us to depend on _all_ the included toolkits, when we'd likely only want to depend on one per any given executable.

[^adapter]: It is an example of the _Adapter Patter,_ though, which we'll examine in a subsequent post.

On the other hand, the genuine abstraction characteristic of the _Abstract Factory Pattern_ (versus the encapsulation provided by the _Adapter Pattern)_ allows us to decouple our operative code from dependency on any concrete toolkit. Without abstraction, we cannot achieve this separation.

## Haskell Abstract Factory

All in one go, I'll show you my Haskell version of the above code. We'll go into more detail below, so just give it a quick skim for now.

{% highlight haskell %}
{-# LANGUAGE DerivingVia, NamedFieldPuns #-}

module AbstractFactory where

import Data.Foldable (fold)
import Data.Map (Map, insert)

----
-- GUI primitives.

class Widget a where
  column :: [a] -> a
  row :: [a] -> a
  image :: FilePath -> a
  text :: String -> a
  heading :: String -> a

----
-- Toolkit-independent GUI components.

table :: Widget a => [(a, r -> a)] -> [r] -> a
table cols rows =
  column (headingRow : dataRows)
  where
    headingRow = row (fmap fst cols)
    dataRows = fmap makeDataRow rows
    makeDataRow r = row (fmap (($ r) . snd) cols)

aboutMe :: Widget a => a
aboutMe =
  column
    [ heading "About me"
    , text
      "Welcome to my site.\
      \ My name is Daniel.\
      \ Thanks for visiting."
    , row
      [ image "skip.png"
      , text
        "Please enjoy a picture of my dog,\
        \ Scipio, or ‟Skip” for short.\
        \ Skip is a German Shepherd-Australian\
        \ Cattle Dog mix, and even though he's five\
        \ years old, he's got boundless energy to keep\
        \ me company on our long hikes through the hills.\
        \ Skip and I do almost everything together."
      ]
    , row
      [ text
        "One thing Skip can't do with me, though,\
        \ is Swing dance. Here's a gif of me doing\
        \ a Lindy Circle, which looks way more impressive\
        \ that it really is. (It's pretty much just the\
        \ basic step...)\n\
        \\n\
        \I'm learning, slowly 😅"
      , image "lindy-circle.gif"
      ]
    , heading "Places I've lived"
    , let
        tblCols =
          [ (text "City", fmtCity)
          , (text "Duration (years)", fmtYears)
          ]
        tblRows =
          [ ("Rialto", 19.5)
          , ("Camarillo", 4.5)
          , ("Auburn", 7)
          , ("Bakersfield", 1)
          , ("Santa Barbara", 1)
          , ("Ventura", 4)
          , ("San Francisco", 0.2)
          ]
        fmtCity (c, _) = text c
        fmtYears (_, y) = text (show y)
      in
        table tblCols tblRows
    ]

----
-- Toolkit 1: HTML

-- Yes, partial fields. Don't 'at' me!
data Html
  = Tag
    { tag :: String
    , styles :: Map String String
    , attrs :: Map String String
    , children :: [Html]
    }
  | Text String

html :: String -> [Html] -> Html
html tag0 =
  Tag tag0 mempty mempty

style :: String -> String -> Html -> Html
style key val html0 =
  case html0 of
    Text {} -> style key val $ html "span" [html0]
    Tag {styles} -> html0 {styles = insert key val styles}

attr :: String -> String -> Html -> Html
attr key val html0 =
  case html0 of
    Text {} -> attr key val $ html "span" [html0]
    Tag {attrs} -> html0 {attrs = insert key val attrs}

instance Widget Html where
  column chl =
    html "div" (fmap (style "display" "block") chl)

  row chl =
    html "div" (fmap (style "display" "inline-block") chl)

  image path =
    attr "source" path (html "image" [])

  text str =
    html "p" [Text str]

  heading str =
    html "h2" [Text str]

----
-- Toolkit 2: Latex

newtype Latex = Latex [String]
  deriving (Semigroup, Monoid) via [String]

bare :: String -> Latex
bare str = Latex [str]

command :: String -> [String] -> Latex
command name params =
  Latex ["\\" <> name <> mkParams params]

env :: String -> [String] -> Latex -> Latex
env name params body =
  fold
    [ Latex $ ["\\begin{" <> name <> "}" <> mkParams params]
    , body
    , Latex $ ["\\end{" <> name <> "}"]
    ]

mkParams :: [String] -> String
mkParams params =
  fold ["{" <> param <> "}" | param <- params]

instance Widget Latex where
  column chl =
    env "center" [] $
      env "tabular" ["c"] $
        foldMap (<> Latex [" \\\\"]) chl

  row chl =
    env "center" [] $
      env "tabular" [fold ["c " | _ <- chl]] $
        foldMap (<> Latex [" & "]) chl

  image path =
    command "includegraphics" [path]

  text = bare

  heading str =
    command "section" [str]
{% endhighlight %}

The first thing one might notice is the curious absence of anything called a _factory_. With that in mind, let's take a closer look at the `Widget` typeclass.

{% highlight haskell %}
class Widget a where
  column :: [a] -> a
  row :: [a] -> a
  image :: FilePath -> a
  text :: String -> a
  heading :: String -> a

data Html = ...
instance Widget Html where ...

data Latex = ...
instance Widget Latex where ...
{% endhighlight %}

Our `Widget` typeclass represents the notion that certain types can be said to be `Widget` types.[^typeclass] A type is a `Widget` type if we have ways to create and combine values of that type the in the ways we expect to be able to create and combine widgets (namely, that appropriate functions `column`, `row`, `image`, `text`, and `heading` exist). That is to say, in this translation of the above Java code, it's the _typeclass_ `Widget` that plays the role of the abstract factory, and it's the _typeclass instances_ `instance Widget Html` and `instance Widget Latex` that play the roles of the concrete factory implementations. The type parameter `a`, oddly enough, plays the role of the Java version's `Widget` interface.

[^typeclass]: This is where we get the term _type class_. A type class is a class (or set) of types that all satisfy some required conditions. Type class instances define precisely how a particular member of the type class satisfies the required conditions.

The most important question is this: does the Haskell version solve the core problem that the _Abstract Factory Pattern_ is meant to solve? Recall the core problem, as stated by GoF.

> Provide an interface for creating families of related or dependent objects without specifying their concrete classes. -- GoF

We see that, indeed, the `Widget` typeclass provides us with a means of creating GUI widgets without specifying their concrete ~~classes~~ types. The evidence lies in `table` and `aboutMe`.

{% highlight haskell %}
table :: Widget a => [(a, r -> a)] -> [r] -> a
aboutMe :: Widget a => a
{% endhighlight %}

`table` and `aboutMe` each creates a widget of an unspecified type `a`. That is, `table` and `aboutMe` can be used to create widgets of any type, even widget types we haven't thought of yet: they're polymorphic over all widget types. By writing polymorphic functions with a `Widget` typeclass constraint, we're able to completely decouple our GUI code from any concrete GUI toolkit. Furthermore, as long as we can write a `Widget` typeclass instance to bootstrap the process, we're able to translate our GUI code into any existing or future toolkit.

## Alternate Haskell Versions

Lest you develop the mistaken notion that typeclasses correspond exactly to instances of the Abstract Factory Pattern, we'll take a look at two other Haskell versions of the same program.

### Record Encoding

You might be thinking that my analogy between the `Widget` typeclass and the Abstract Factory Pattern is somewhat thin. After all, where's the actual **factory**? Consider an alternative, more literal Haskell translation of the Java version. Suppose that we had no `Widget` typeclass, and instead had a `WidgetFactory` record.

{% highlight Haskell %}
data WidgetFactory a =
  WidgetFactory
    { column :: [a] -> a
    , row :: [a] -> a
    , image :: FilePath -> a
    , text :: String -> a
    , heading :: String -> a
    }
{% endhighlight %}

In this encoding, we would explicitly pass a value of type `WidgetFactory a` to `table` and `aboutMe`.

{% highlight Haskell %}
table :: WidgetFactory a -> [(a, r -> a)] -> [r] -> a
table factory cols rows = ...

aboutMe :: WidgetFactory a -> a
aboutMe factory = ...
{% endhighlight %}

Instead of invoking a top-level `rows` to a list of widgets `childWidgets`, you'd extract the `rows` field from your factory and apply to the list of widgets, _vis a vis_ `(rows factory) childWidgets`. Instead of class instances, you'd write functions that return `WidgetFactory`s.

{% highlight Haskell %}
data Html = ...
htmlFactory :: WidgetFactory Html
htmlFactory =
  WidgetFactory
    { columns = \chl ->
        html "div" (fmap (style "display" "block") chl)
    , row = \chl ->
        html "div" (fmap (style "display" "inline-block") chl)
    , image = \path ->
        attr "source" path (html "image" [])
    , text = \str ->
        html "p" [Text str]
    , heading = \str ->
        html "h2" [Text str]
    }

data Latex = ...
latexWidgetFactory :: WidgetFactory Latex
latexWidgetFactory =
  WidgetFactory
    { columns = ...
    , ...
    }
{% endhighlight %}

Notice that in both the typeclass version and the record version, the type parameter `a` guards us from passing the wrong kind of widget into a factory. In the Haskell versions, the types `Html` and `Latex` don't need a common subtype the way they do in the Java version. In fact, the type parameter completely obviates the need for a type hierarchy, as the only reason we had to have a type hierarchy in the Java version was so that we could provide signatures for the `AbstractWidgetFactory` methods.

Another thing to notice is that the record version allows us to have multiple factories for the same type in scope. We could have two different values of type `WidgetFactory Html`, and we could choose between them for whatever various reasons. Sometimes this is what you want, and it's slightly annoying to pull off in the typeclass version, making the record encoding preferable in such cases. In typical cases, though, having multiple factories for the same type is decidedly **not** what we want. We usually want to ensure that there's only one factory of a given type in scope. The typeclass version ensures that this is the case, while the record version leaves this as a backdoor for potential bugs.[^nbd]

[^nbd]: It's usually not that big of a deal, to be honest.

### Algebraic Datatype Encoding

While the typeclass version and the record version are morally the same encoding (the record encoding follows the typeclass encoding in an entierly formulaic way[^sytc]), we can yet model the Abstract Factory Pattern in an altogether different way by inverting the priority of the principle players. By that, I mean we change our notion of which concepts are primitive and which concepts are composite.

[^sytc]: See the legendary blog post [Scrap Your Type Classes](https://www.haskellforall.com/2012/05/scrap-your-type-classes.html) by the incomperable Gabriella Gonzalez for details.

{% highlight haskell %}
data Widget
  = Column [Widget]
  | Row [Widget]
  | Image FilePath
  | TextW String -- the constructor name `Text` is already taken by `Html`
  | Heading String

table :: [(Widget, r -> Widget)] -> [r] -> Widget
table cols rows =
  Column (headingRow : dataRows)
  where
    headingRow = Row (fmap fst cols)
    dataRows = fmap makeDataRow rows
    makeDataRow r = Row (fmap ((S r) . snd) cols)

asHtml :: Widget -> Html
asHtml widget = case widget of
  Column chl ->
    html "div" (fmap (style "display" "block" . asHtml) chl)
  Row chl ->
    html "div" (fmap (style "display" "inline-block" . asHtml) chl)
  Image path ->
    attr "source" path (html "image" [])
  TextW str ->
    html "p" [Text str]
  Heading str ->
    html "h2" [Text str]

asLatex :: Widget -> Latex
asLatex widget = case widget of
  Column chl -> ...
  ...
{% endhighlight %}

In this encoding, the type `Widget` consists of _abstract syntax trees_ for our notions of what a widget should be and how we should be able to manipulate and combine widgets. We write our GUI code in terms of the abstract syntax. We then write the interpreters `asHtml` and `asLatex` that interpret the abstract syntax in terms of the respective concrete toolkits.

## Concluding Remarks

Notice that, at the end of the day, we end up writing what's basically all the same code in all three versions. The cases for `asHtml` end up having nearly identical code as the methods of the typeclass instance `Widget Html` and the fields of the record `htmlWidgetFactory :: WidgetFactory Html`. None of these versions results in significantly less code than either of the others.

All three encodings have their strengths and weaknesses. For example, a nice thing about the datatype encoding is that `Widget`s are now serializable. A nice thing about the record encoding is that you can have multiple factories in scope and select one or the other at runtime, if that's a thing you want to do. You could even construct new factories at runtime. Some nice things about the typeclass encoding are that it's very [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)---your `Widget` functions are top-level, so you neither need to keep pulling them out of some record nor need to keep passing said record down the call stack---and it ensures that there's only one factory in scope for any given type, if that's a thing you want to ensure.

I wanted to show you three different encodings because I don't want you to get the idea that typeclasses are necessarily some kind of manifestation of the Abstract Factory Pattern. Typeclasses are their own thing, and it just so happens that they provide a way to emulate the Abstract Factory Pattern. But so do records and so do algebraic datatypes, so there's no special connection between abstract factories and typeclasses. I also don't want you to think that typeclasses correspond to OOP interfaces (really, records are closer to OOP interfaces, but still not quite the same). In general, we shouldn't try to directly map OOP concepts onto Haskell concepts, because to do so is a form of the [XY Problem](https://en.wikipedia.org/wiki/XY_problem). Instead, we should always take a deep breath and go back to the root problem we're trying to solve. We may find a simple answer.

Recall that the basic problem the Abstract Factory Pattern solves is fundamentally about decoupling. The three above Haskell versions demonstrate two simple ideas. First, we have the record and typeclass encodings, which illustrate the utility of type parameters and callbacks for decoupling code. The record (respectively, the typeclass instance) is really just a way of passing in a bunch of callbacks to a function, when you think about it. Second, the datatype encoding illustrates the utility of _domain-specific languages_ (DSLs) and interpreters for decoupling code. Write your program in terms of a simple, declarative grammar with a few primitives and basic combination rules. Then, interpret your program by simply interpreting the primitives and basic rules, a form of induction/recursion.

## Summary

- Design patterns are templates of solutions that---for whatever reason---evade being codified into a reusable function, component, or library.

- Abstractions are open to interpretation. _Abstract_ stands in contrast to _concrete._

- Merely hiding concrete implementation details doesn't make something abstract; rather, it encapsulates (which is fine, encapsulation is appropriate at times).

- Abstraction decouples code. Encapsulation does not.

- An _Abstract Widget Factory_ is an **abstract** (widget factory), not an **(abstract widget)** factory. Explicitly, it's the factory that's abstract, not the widgets. An abstract factory is a placeholder for a concrete factory that will be given to your code at some future time.

- An OOP-style abstract factory can be modeled in Haskell using classes, records, or an algebraic datatype. None is clearly better than the others.

- You can use type parameters and callbacks to decouple code.

- You can use domain-specific languages and interpreters to decouple code.

- In general, try to avoid thinking about how to directly translate OOP solutions to Haskell. Instead, go back to the original problem.

- Keep it simple and don't overthink things. At the end of the day, it's all just passing arguments to functions.

---
