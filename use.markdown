---
layout: default
title: Non-Configurable Behaviors
---

This is a list of everything about using Argon that you wouldn't know if you
only read about writing your [configuration](config.html).

These features could be made configurable.

# Moving and Resizing Floating Windows #

To move a floating window, hold down the Mod1 key (probably Alt), left-click
anywhere in the window, and drag. The current implementation is implemented
rather naively, and as a result, the window will probably not move as fast as
your mouse. I will fix this eventually.

To resize a floating window, use the same procedure, but with the right mouse
button.

# Directional Tile Queries #

When you use a [directional tile query](config.html#tile_queries)
(like `dir-left`), there may be multiple tiles which touch the left border
of the current tile. How is one chosen?

* The most recently-focused tile is chosen, if any of the candidates
  appear in your history.
* Otherwise, the candidate which has the longest border with the current one
  is chosen.

A tile that only touches at a corner is never a candidate.

