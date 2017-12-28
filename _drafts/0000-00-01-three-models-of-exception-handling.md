---
layout: post
title: "Three Models of Exception Handling"
date: 1999-12-31
permalink: /blog/1999-12-31-three-models-of-exception-handling
comments: true
tags:
- scala
- exception handling
- continuation
- monad
---

The hot-shot team member just pulled a miracle and, over the weekend, build the API our customers have been promised for the passed year, averting a crisis. Works like a charm, except of course when it doesn't. Now, it's up to us to productionalize the prototype by adding appropriate error handling.

<!--break-->

## The Project

This is an API over HTTP which, for simplicity's sake, only accepts POST requests. Users specify a resource in the path of their request, provide an auth token in their header, and the body of their request gets posted to the resource.

Of course, we still need to return an appropriate response whenever those assumptions are not met. The current prototype returns a response only when all preconditions are met, and halts otherwise. Here's the spec and the current implementation:

```scala

```
