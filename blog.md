---
layout: page
title: Blog
permalink: /blog/
---

The personal blog of Daniel Brice. These terrible, Orwellian, and
often  Neanderthal opinions are my own, and do not reflect on anyone I
am associated with.

<p class="rss-subscribe">Subscribe <a href="{{ "/feed.xml" | prepend: site.baseurl }}">via RSS</a>.</p>

<ul class="post-list">
  {% for post in site.posts %}
    <li>
      <span class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</span>
      <h2>
        <a class="post-link"
          href="{{ post.url | prepend: site.baseurl }}">
          {{ post.title }}
        </a>
      </h2>
      {{ post.excerpt }}
    </li>
  {% endfor %}
</ul>

