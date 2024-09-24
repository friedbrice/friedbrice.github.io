---
layout: post
title: "Functor the Ultimate Delegate"
date: 1999-12-31
permalink: /blog/functor-the-ultimate-delegate/
comments: true
tags:
  - math
  - category theory
  - haskell
---



<!-- break -->

Imagine your program needs a type called `PersonTroupe`, and imagine that this type's name is appropriately chosen.
This suggests that your program also has a type called `Person`, and perhaps that `PersonTroupe` is a data structure consisting of or referencing zero, one, or many `Person`s.
No doubt you've labored tirelessly implementing many operations on `Person`, such as `tapHeelLeft`, `tapHeelRight`, `tapBallLeft`, `tapBallRight`, `shiftHeelLeft`, `shiftHeelRight`, `shiftBallLeft`, `shiftBallRight`, and `jazzHands`.
Likely many more.

It makes sense, conceptually, for these operations to apply, in unison, to a `PersonTroupe`.
Naturally, you'd like your program to reflect that concept.
The way we do this is we _delegate._

## The Delegation Pattern

```java
interface Dancer {
  void tapHeelLeft();
  void tapHeelRight();
  void tapBallLeft();
  // one million other operations
  // and then finally
  void jazzHands();
  Integer height();
}

class Person implements Dancer {
  private Integer height;

  Integer height() {
    return this.height;
  }

  // every `...` represents _lots_ of tedious code.
  void tapHeelLeft() {...}
  void tapHeelRight() {...}
  void tapBallLeft() {...}
  // ...
  // one million more
  // ...
  void jazzHands() {...}
}

class PersonTroupe implements Dancer {
  private Person[] persons;

  void tapHeelLeft() {
    for (Person person : persons) {
      person.tapHeelLeft();
    }
  }

  void tapHeelRight() {
    for (Person person : persons) {
      person.tapHeelRight();
    }
  }

  void tapBallLeft() {
    for (Person person : persons) {
      person.tapBallLeft();
    }
  }

  // ...
  // one million times more
  // ...

  void jazzHands() {
    for (Person person : persons) {
      person.jazzHands();
    }
  }

  Integer height() {
    throw MethodNotSupportedException(PersonTroupe::height);
  }
}
```
