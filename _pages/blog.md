---
layout: page
title: "Blog"
permalink: /blog/
date: 2016-08-30
topnav: true
---

<!-- This is such a silly hack :-p
We're having Jekyll generate Javascript that hard-codes the posts data.
So all the data will just be there in the page source.
That's fine, though; the JS will be re-generated every time I make a new post and push to Github.
-->
<script>
  let props = {
    baseurl: "{{ site.baseurl }}",
    posts: {},
    tags: {}
  };

  let state = {
    selectedTags: []
  };

  {% for post in site.posts %}
    (() => {
      let postKey = "{{ post.date }} {{ post.permalink }}";
      props.posts[postKey] = {
        postKey: postKey,
        date: new Date("{{ post.date }}"),
        permalink: "{{ post.permalink }}",
        url: "{{ post.url }}",
        title: "{{ post.title }}",
        tags: []
      };
      {% if post.tags %}
        {% for tag in post.tags %}
          (() => {
            let tag = "{{ tag }}";
            props.posts[postKey].tags.push(tag);
            if (!props.tags[tag]) {
              props.tags[tag] = [];
            }
            props.tags[tag].push(postKey);
          })();
        {% endfor %}
      {% endif %}
    })();
  {% endfor %}

  const sortedTags = Object.keys(props.tags)
    .map(t => {return {tag: t, count: props.tags[t].length}})
    .sort((l, r) => l.tag.localeCompare(r.tag))
    .sort((l, r) => r.count - l.count)
    .map(({tag}) => tag)

  function formatDate(date) {
    // sigh...
    let y = date.getFullYear();
    let d = date.getDate();
    let m = undefined;
    let a = undefined;

    switch (date.getMonth()) {
      case 0: m = 'Jan'; break;
      case 1: m = 'Feb'; break;
      case 2: m = 'Mar'; break;
      case 3: m = 'Apr'; break;
      case 4: m = 'May'; break;
      case 5: m = 'Jun'; break;
      case 6: m = 'Jul'; break;
      case 7: m = 'Aug'; break;
      case 8: m = 'Sep'; break;
      case 9: m = 'Oct'; break;
      case 10: m = 'Nov'; break;
      case 11: m = 'Dec'; break;
    }

    switch (date.getDay()) {
      case 0: a = 'Sun'; break;
      case 1: a = 'Mon'; break;
      case 2: a = 'Tue'; break;
      case 3: a = 'Wed'; break;
      case 4: a = 'Thu'; break;
      case 5: a = 'Fri'; break;
      case 6: a = 'Sat'; break;
    }

    return `${a}, ${m} ${d}, ${y}`;
  }

  function empty() {
    return document.createComment('empty');
  };

  function text(str) {
    return document.createTextNode(str);
  };

  function node(tag, attrs, elems) {
    let theNode = document.createElement(tag)
    attrs.forEach(attr => theNode.setAttribute(attr.name, attr.value))
    elems.forEach(elem => theNode.appendChild(elem))
    return theNode
  };

  function postItemId(prefix, postKey) {
    return `${prefix} post-item ${postKey}`
  }

  function tagButtonIdMenu(prefix, tag) {
    return `${prefix} tag-button-menu ${tag}`
  }

  function tagButtonIdPost(prefix, postKey, tag) {
    return `${postItemId(prefix, postKey)} tag-button-post ${tag}`
  }

  const tagClass = 'post-tag-button';
  const tagClassSelected = 'post-tag-button-selected';

  function tagList(tags, makeId) {
    return node('ul',
      [{name: 'class', value: 'post-tag-list'}],
      tags.map(tag => [
        node('li',
          [
            {name: 'class', value: tagClass},
            {name: 'id', value: makeId(tag)},
            {name: 'onclick', value: `update.selectTag('${tag}')`}
          ],
          [text(`[ ${tag} ]`)]
        ),
        text('\n')
      ]).flat()
    );
  }

  function view(prefix) {
    return node('div',
      [],
      [
        tagList(sortedTags, tag => tagButtonIdMenu(prefix, tag)),
        node('ul',
          [{name: 'class', value: 'post-list'}],
          Object.values(props.posts).map(post => node('li',
            [{name: 'id', value: postItemId(prefix, post.postKey)}],
            [
              node('span',
                [{name: 'class', value: 'post-meta'}],
                [text(formatDate(post.date))]
              ),
              node('h2',
                [],
                [
                  node('a',
                    [
                      {name: 'class', value: 'post-link'},
                      {name: 'href', value: `${props.baseurl}${post.url}`}
                    ],
                    [text(post.title)]
                  )
                ]
              ),
              post.tags?
                tagList(post.tags, tag => tagButtonIdPost(prefix, post.postKey, tag)):
                empty()
            ]
          ))
        )
      ]
    );
  };

  function makeUpdate(prefix) {
    return {
      selectTag: (tag) => {
        if (state.selectedTags.includes(tag)) {
          state.selectedTags = state.selectedTags.filter(t => t !== tag);

          document.getElementById(tagButtonIdMenu(prefix, tag)).className = tagClass;

          props.tags[tag].forEach(postKey => {
            document.getElementById(tagButtonIdPost(prefix, postKey, tag)).className = tagClass;
          });

          if (state.selectedTags.length === 0) {
            for (postKey in props.posts) {
              document.getElementById(postItemId(prefix, postKey)).style.display = 'block';
            }
          } else {
            props.tags[tag].forEach(postKey => {
              let keep = props.posts[postKey].tags.reduce((acc, t) => acc || state.selectedTags.includes(t), false);
              if (!keep) {
                document.getElementById(postItemId(prefix, postKey)).style.display = 'none';
              }
            });
          }
        } else {
          state.selectedTags.push(tag);

          document.getElementById(tagButtonIdMenu(prefix, tag)).className = tagClassSelected;

          props.tags[tag].forEach(postKey => {
            document.getElementById(tagButtonIdPost(prefix, postKey, tag)).className = tagClassSelected;
          });

          if (state.selectedTags.length === 1) {
            for (postKey in props.posts) {
              if (!props.posts[postKey].tags.includes(tag)) {
                let postItem = document.getElementById(postItemId(prefix, postKey));
                postItem.style.display = 'none';
              }
            }
          } else {
            props.tags[tag].forEach(postKey => {
              let postItem = document.getElementById(postItemId(prefix, postKey));
              postItem.style.display = 'block';
            });
          }
        }
      }
    };
  };

  function render(parent) {
    let prefix = `${parent} ${Math.random().toString().slice(2)}`;
    document.getElementById(parent).appendChild(view(prefix));
    return prefix;
  };
</script>

The personal blog of Daniel Brice. These half-baked, meandering, and often
incoherent opinions are my own, and do not reflect the opinions of
any one or group I am associated with.

<p class="rss-subscribe">Subscribe <a href="{{ "/feed.xml" | prepend: site.baseurl }}">via RSS</a>.</p>

<div class="home" id="blog-home">
  <script>
    const update = makeUpdate(render('blog-home'));
  </script>
  <noscript>
    <ul class="post-list">
      {% for post in site.posts %}
        <li>
          <span class="post-meta">{{ post.date | date: "%a, %b %-d, %Y" }}</span>

          <h2>
            <a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a>
          </h2>
          {% if post.tags %}
            <span class="post-meta">
              {% for tag in post.tags %}
                <span style="white-space: nowrap;">[ {{ tag }} ]</span>
              {% endfor %}
            </span>
          {% endif %}
        </li>
      {% endfor %}
    </ul>
  </noscript>
</div>

<p class="rss-subscribe">Subscribe <a href="{{ "/feed.xml" | prepend: site.baseurl }}">via RSS</a>.</p>
