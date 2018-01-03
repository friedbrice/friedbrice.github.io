---
layout: post
title: "Sangria Design Notes"
date: 1999-12-31
permalink: /blog/sangria-design-notes/
comments: true
tags:
  - code
  - scala
  - style
  - types
---

We're adopting [GraphQL][1] for our team's next project, and since our backend will be in Scala, we have the pleasure of using the incredible GraphQL library [Sangria][2]. There's a bit of a learning curve with any stack adoption, and this post serves as a crash course based on my two-week-long experience using Sangria to build a working prototype of our new GraphQL service.

<!-- break -->

Before we begin, let's get a few things out of the way.

First, it bears repeating that these notes represent about two week's worth of working with GraphQL and Sangria. I can't really claim any expertise here. If you notice something that I could be doing better, or something that I shouldn't be doing, or if this advice is total crap (for a reason that you can articulate), please let me know in the comments right away.

Second, this post assumes you've read the GraphQL [sales pitches][3] and that you're familiar with the basic concepts. We won't define terms like __resolver__ or explain GraphQL SDL syntax. We'll go a little bit into the basic system architecture we chose and we'll 

Third, we're building a read-only service with a single entry point, using GraphQL to aggregate multiple legacy and internal APIs. If you're building a different flavor of service, or if you're using Sangria on the front end, your mileage may vary.

# Overview

We split our application into four components:

  - **The Data Model:** How we model our domain data in Scala.
  - **Data Retrieval:** HTTP bindings to the existing APIs the application aggregates.
  - **Schema Definition:** Declare the GraphQL schema and resolvers in Sangria's vernacular.
  - **The Application Layer:** The actual HTTP serving.

We make ten recommendations, grouped by the relevant stack:

  - The Data Model
      - Use descriptive type aliases and keyword arguments
      - Make case classes reflect physical data sources
  - Data Retrieval
      - Make the DAO reflect physical data sources
      - Propogate needed arguments through case classes
      - Use `Future`s for external API calls
      - Use `TrieMap`s for global caching
  - Schema Definition
      - Use `Fetcher`s and `DeferredValue`s for batching, caching, and concurrency
      - Make a project spec schema and compare the implemented schema against it
  - Application Layer
      - Use `ExceptionHandler` to handle errors in resolvers
      - Use `Future.recover` for syntax and schema errors

# The Data Model

**Use Descriptive Type Aliases and Keyword Arguments**

HTTP APIs are stringly typed. A call involves splicing several strings together to for a request and then splitting strings out of the response.

**Make Case Classes Reflect Physical Data Sources**

# Data Retrieval

**Make the DAO Reflect Physical Data Sources**

**Propogate Needed Arguments Through Case Classes**

**Use Futures for External API Calls**

**Use TrieMaps for Caching**

# Schema Definition

**Use Fetcher and DeferredValue to Batch and Cache**

**Compare Implemented Schema to Project Spec Schema**

# Application Layer

**Use ExceptionHandler to Handle Errors In Resolvers**

**Recover on Syntax and Schema Errors**

  [1]: https://code.facebook.com/posts/1691455094417024/graphql-a-data-query-language/
  [2]: http://sangria-graphql.org/
  [3]: https://www.howtographql.com/
