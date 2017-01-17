---
layout: post
title: "Opening `.url` Files Like a Pro"
date: 2015-12-14
permalink: /blog/2015-12-14/
redirect_from: [ /blog/12/ ]
comments: true
tags:
- bash
- code
- linux
---

I go back and forth between Linux, Windows, and OS X, and I end up getting a lot of proprietary files in my Dropbox.
It's been bugging me for a long time that there's no xdg handler for Windows `.url` files in Linux, so I finally strung one together.

<!--break-->

A `.url` files in windows looks something like this:

{% highlight ini %}
; ~/example.url
[InternetShortcut]
URL=http://www.example.com
{% endhighlight %}

Simple, elegant, clean.

In order to pull the url out of a file like that, we just have to find the line that begins with `URL=`, cut that line at the `=`, and take the second piece.
That's a one-liner in Bash:

{% highlight bash %}
cat "${MY_FILE}" | grep "URL=" | cut -d = -f 2
{% endhighlight %}

Once we have the URL, we can feed it to `xdg-open` to open it in the default browser.
Here's a Bash script that does that with a few embellishments.

{% highlight bash %}
#!/bin/bash
# ~/bin/openurl
# A script for opening windows `.url` files.

URL=$(cat "$1" | grep "URL=" | cut -d= -f2)
echo -e "Opening\n$URL\nin default browser..."
xdg-open "$URL" & sleep 3
{% endhighlight %}

Remember to make it executable:

{% highlight text %}
$ chmod +x ~/bin/openurl
{% endhighlight %}

The `$1` refers to the first argument, so if we invoke as `openurl example.url`, then `$1` will expand to `example.url`.
We can try this from the command line, and you should see that it works as intended.

{% highlight text %}
$ openurl example.url
Opening
http://www.example.com
in default browser...
Created new window in existing browser session.
$
{% endhighlight %}

The last line of output is created by `chromium-browser`, my default browser.

Nice, but not enough.
We ultimately want to be able to just double click on `example.url` in our file manager, and then have a browser tab open with the correct URL.
The program that makes all of that work is called `xdg-open`, so we need `xdg-open example.url` to invoke `~/bin/openurl example.url`.
For this, we need to create a `.desktop` file for `~/bin/openurl`.

We need to know the MIME-type of a `.url` files, so we check real quick:

{% highlight text %}
$ xdg-mime query filetype example.url
application/x-mswinurl
{% endhighlight %}

And now we can write our `.desktop` file.

{% highlight ini %}
; ~/.local/share/applications/openurl.desktop
[Desktop Entry]
Version=1.0
Name=openurl
Comment=A script for opening windows `.url` files.
Exec=<FULL PATH TO EXECUTABLE, eg /home/me/bin/openurl>
Type=Application
MimeType=application/x-mswinurl
{% endhighlight %}

Finally, we just need to tell xdg to open `application/x-mswinurl` types with `openurl.desktop`.

{% highlight text %}
$ xdg-mime default openurl.desktop application/x-mswinurl
{% endhighlight %}

Now, we can open `.url` files in Linux by double clicking them.
Not sure how I'm going to do all this wiring in OS X, though.

Here are a few pages that I referenced to get this working:

1. [xdg-open - ArchWiki](http://wiki.archlinux.org/index.php/Xdg-open)
2. [unity - Creating a .desktop file for a new application - Ask Ubuntu](http://askubuntu.com/questions/281293/creating-a-desktop-file-for-a-new-application)
