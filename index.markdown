---
layout: default
title: Argon
---

Argon is a declarative tiling window manager.
It was designed to do [what I want](principles.html).

Feel free to [email me](mailto:ktvoelker@gmail.com) your thoughts or questions
about Argon.

# Features #

* Reusable tile layouts
* A floating window layer above every tiled workspace that can be shown
  and hidden on command
* Powerful commands for changing the focus and moving windows
* A history of recently-focused tiles
* Pattern-matching on new windows to put them where you want them
* Keybinding groups which can be enabled and disabled independently
  of each other
* User-defined event-handling hooks

# Download #

There are two options:

* Get the pre-built binary package with
[lzma](http://github.com/downloads/ktvoelker/argon/argon-0.2.0-binary.tar.xz)
(916K)
or
[gzip](http://github.com/downloads/ktvoelker/argon/argon-0.2.0-binary.tar.gz)
(1360K)
compression
* Get the
[source package](http://github.com/downloads/ktvoelker/argon/argon-0.2.0.tar.gz) (40K),
which can be built with [`cabal`](http://www.haskell.org/cabal/).

Argon is licensed under the
[GNU General Public License v3.0](http://www.gnu.org/licenses/gpl.html).
The full text of the license is included in all download packages in the
file `LICENSE`.

# Installation #

There are two options:

* Manually put the binary, `dist/build/argon/argon`, somewhere on your
  execution path.
* Run `cabal install`. Note that the directory to which `cabal` installs
  Argon may not be on your execution path.

If you don't have [GHC](http://www.haskell.org/ghc/), the Haskell compiler,
you may need to install [the GMP library](http://gmplib.org/). You almost
certainly have all [the other dependencies](deps.html) already.

# Configuration #

You need a [configuration file](config.html) to run Argon. An example
configuration is included in both download packages at `src/argon.ini`. The
example was written for a 1920x1200 display and a Dvorak keyboard, so you may
not find it directly useful, but it may help you understand how to write your
own.

# Use #

If you start X by running `startx`, then the script `$HOME/.xinitrc` is
responsible for starting your window manager. But if you use `startx`, you
probably already knew that.

If you use a display manager to start X, like GDM, KDM, or XDM, then you may
be able to configure a "custom session" by editing the script
`$HOME/.xsession`. I'm not an expert at this, but if you're having trouble,
[ask anyway](mailto:ktvoelker@gmail.com).

How to use Argon itself depends almost entirely on your
[configuration file](config.html), but there are a few
[features](use.html) that are not exposed by the configuration.

# Bugs #

To report a bug, please [send me an email](mailto:ktvoelker@gmail.com).

It would be helpful for you to include the [runtime log](debug.html) of Argon
with your bug report.

# Development #

You can clone the latest sources from
[GitHub](http://github.com/ktvoelker/argon) by running:

    git clone git://github.com/ktvoelker/argon.git

You can send patches to [ktvoelker@gmail.com](mailto:ktvoelker@gmail.com)
or send me a pull request on [GitHub](http://github.com/ktvoelker/argon).

