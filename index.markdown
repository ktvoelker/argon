---
layout: default
title: Argon
---

Argon is a declarative tiling window manager.
It was designed to do [what I want](principles.html).

Feel free to [email me](mailto:ktvoelker@gmail.com) your thoughts or questions
about Argon.

Download
--------

I am going to be making a release very soon. I just have to finish this
documentation and fix one known bug. In the meantime, you can clone the
sources and check out the `0.2.0` branch. See below for instructions on
cloning.

<!--
There are two options:

* Get the pre-built [binary package](http://github.com/downloads/ktvoelker/argon/argon-0.2.0-binary.tar.bz2/qr_code).
* Get the [source package](http://github.com/downloads/ktvoelker/argon/argon-0.2.0.tar.bz2/qr_code),
which can be built with [`cabal`](http://www.haskell.org/cabal/).

Argon is licensed under the
[GNU General Public License v3.0](http://www.gnu.org/licenses/gpl.html).
The full text of the license is included in both download packages in the
file `LICENSE`.
-->

Installation
------------

There are two options:

* Manually put the binary, `dist/build/argon/argon`, somewhere on your
  execution path.
* Run `cabal install`. Note that the directory to which `cabal` installs
  Argon may not be on your execution path.

Then, use `argon` as your [window manager](wm-install.html).

Use
---

You need a [configuration file](config.html) to run Argon. An example
configuration is included in both download packages at `src/argon.ini`. The
example was written for a 1920x1200 display and a Dvorak keyboard, so you may
not find it directly useful, but it may help you understand how to write your
own.

Bugs
----

To report a bug, please [send me an email](mailto:ktvoelker@gmail.com).

It would be helpful for you to include the [runtime log](debug.html) of Argon
with your bug report.

Development
-----------

You can clone the latest sources from
[GitHub](http://github.com/ktvoelker/argon) by running:

    git clone git://github.com/ktvoelker/argon.git

You can send patches to [ktvoelker@gmail.com](mailto:ktvoelker@gmail.com)
or send me a pull request on [GitHub](http://github.com/ktvoelker/argon).

