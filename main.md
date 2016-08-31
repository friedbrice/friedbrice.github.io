---
layout: default
permalink: /
---

The personal website of Daniel Brice.

Made with [Jekyll](http://jekyllrb.com/) and hosted on
[GitHub Pages](https://pages.github.com/).

I'll be using this space to muse about Math and coding and games,
mostly.

## Latest Blog Posts

<ul class="post-list">
  {% for post in site.posts  limit:5 %}
    <li>
      <span class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</span>
      <h2>
        <a class="post-link"
          href="{{ post.url | prepend: site.baseurl }}">
          {{ post.title }}
        </a>
      </h2>
      {% if post.content contains '<!--break-->' %}
        {{ post.content | split:'<!--break-->' | first }}
      {% else %}
        {{ post.excerpt }}
      {% endif %}
      <a href="{{ post.url }}">more...</a>
    </li>
  {% endfor %}
</ul>
