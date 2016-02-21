---
layout: post
title: "Local Hoogle Daemon"
date: 2016-02-20
permalink: /blog/13/
tags:
- code
- haskell
- linux
- systemd
- xdg
---

Has it really been two months?! It's like the quarter starts and I get
sucked into a black hole, never to be heard from again. I'm starting to
pick up Real World Haskell again, and I needed reliable documentation
without relying on an internet connection, so I spun up a local Hoogle
server as a systemd service.

<!--break-->

Running Hoogle locally is nothing revolutionary. I followed some great
sources. However, I did need more than a few sources, especially to get
the systemd stuff worked out, so I'm writing everything down here so
it'll be in one place.

-   Install `hoogle` and `luakit`.

    Pretty obvious, except we have a choice. We can install with
    `cabal-install`, but I prefer to use `apt-get`.

    `luakit` is not exactly required. You can just use your default
    browser (mine is Chromium), but I like using luakit in this context
    because I like having an icon and window for Hoogle separate from my
    main browser.

-   Create `~/.local/share/applications/hoogle.desktop`

    The `hoogle.desktop` file is a launcher for Linux GUIs that adhere
    to the Freedesktop.org standard, so you'll have an icon in your
    applications menu and on your dock (where applicable).

{% highlight ini %}
; hoogle.desktop
[Desktop Entry]
Type=Application
Version=1.0
Name=Hoogle
Comment=A Haskell API search engine
Path=/usr/bin
Exec=/usr/bin/luakit http://localhost:1080
Icon=haskell
Terminal=false
Categories=Development;
{% endhighlight %}

-   Create `~/.icon` tree

    This part is just tedious. To make your life easier, I'll provide a
    [zip archive][1]. Just unpack it and move the `.icon` directory to
    your home directory.

    This archive provides something of a minimal example for creating
    your own custom icons for Freedesktop.org-compliant environments,
    so you might peek inside to get an idea for how things work.

  [1]: {{ site.baseurl }}/assets/hoogle-icons.zip

-   Create `/etc/systemd/system/hoogle.service`

    `hoogle.service` defines a system daemon that will run Hoogle in the
    background. We can enable it to run at boot, or we can manually
    start and stop the service as needed.

{% highlight ini %}
; hoogle.service
[Unit]
Description=A Haskell API search engine.
Documentation=man:hoogle(1)

[Service]
User=hoogle
ExecStart=/usr/bin/hoogle server --local --port 1080
WorkingDirectory=/var/log/hoogle

[Install]
WantedBy=multi-user.target
{% endhighlight %}

-   Create a system user called `hoogle`

    We don't want our Hoogle server running as root, so we'll create a
    special non-login user.

    {% highlight text %}
    # useradd -r hoogle
    {% endhighlight %}

-   Create `/var/log/hoogle` and set ownership and mod rules

{% highlight text %}
# mkdir /var/log/hoogle
# chown hoogle:hoogle /var/log/hoogle
# chmod 775 /var/log/hoogle
{% endhighlight %}

-   Start and/or enable `hoogle.service`

    If you want the Hoogle daemon to run at system boot, replace
    `<command>` with `enable`. Using `disable` will undo this. If you
    want to manually start (res. stop) the Hoogle daemon, replace
    `<command>` with `start` (res. `stop`).

{% highlight text %}
# systemctl <command> hoogle.service
{% endhighlight %}

And, that's it. Once Hoogle is running as a daemon, you can use
then find "Hoogle" in your GUI application launcher and launch it like
any other app.
