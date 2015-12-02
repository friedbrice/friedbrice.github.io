---
layout: default
---

The personal website of Daniel Brice, PhD.

Made with [Jekyll](http://jekyllrb.com/) and hosted on
[GitHub](https://github.com/).

I'll be using this space to muse about Math and coding and games,
mostly.

<div class="home">
  <h1 class="page-heading">Recent Posts</h1>
  <ul class="post-list">
    {% for post in site.posts %}
      <li>
        <span class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</span>

        <h2>
          <a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a>
        </h2>
      </li>
    {% endfor %}
  </ul>
  <p class="rss-subscribe">Subscribe <a href="{{ "/feed.xml" | prepend: site.baseurl }}">via RSS</a>.</p>
</div>
