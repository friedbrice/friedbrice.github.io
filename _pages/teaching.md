---
layout: page
title: "Teaching"
permalink: /teaching/
date: 2016-08-30
topnav: true
---

I was a Lecturer of Mathematics at [Cal State Bakersfield](http://csub.edu) during the 2015-2016 academic year. Here you'll find links to my [course pages](#courses) and [resources](#resources) that students might find helpful.

## Courses

_Here's an algorithmically-generated course list, but there's no content
yet. I'm using Blackboard (though wish I were
not, and all
the course content is there._

{% for page in site.pages reversed limit:6 %}
  {% if page.courselist %}
* [{{ page.title }}]({{ page.permalink }})  --  {{ page.coursetitle }}
  {% endif %}
{% endfor %}
<!--
* [Full List](about:blank), including courses taught at [Auburn University](http://auburn.edu)
-->

## Resources

* [Math | Khan Academy](http://www.khanacademy.org/math) - _Watch videos
  and practice your skills for almost any math subject._

* [Mathematics | MIT OpenCourseWare](http://ocw.mit.edu/courses/mathematics/)
  _Various MIT faculty are openly sharing these resources as a service
  to OCW users. The resources include calculus textbooks by Professors
  Gilbert Strang and Daniel Kleitman._

* [opencalculus](http://opencalculus.wordpress.com) - _Devoted to free
  calculus resources for students, free and open source materials for
  instructors, and active engagement for all._

* [PatrickJMT](http://patrickjmt.com/) - _Just Math Tutorials: making
  FREE and hopefully useful math videos for the world!_

* [Pauls Online Math Notes](http://tutorial.math.lamar.edu) - _The
  intent of this site is to provide a complete set of free online
  (and downloadable) notes and/or tutorials for classes that \[the
  author] teach\[es] at
  Lamar University._

* [Wolfram|Alpha](http://www.wolframalpha.com) - _Computational
  knowledge engine._ Basically an online graphing calculator and
  computer algebra system.
