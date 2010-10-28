---
layout: default
title: Installing Argon as Your Window Manager
---

If you start X by running `startx`, then the script `$HOME/.xinitrc` is
responsible for starting your window manager. But if you use `startx`, you
probably already knew that.

If you use a display manager to start X, like GDM, KDM, or XDM, then you may
be able to configure a "custom session" by editing the script
`$HOME/.xsession`. I'm not an expert at this, but if you're having trouble,
[ask anyway](mailto:ktvoelker@gmail.com).

