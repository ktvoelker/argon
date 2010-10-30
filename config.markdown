---
layout: default
title: Configuring Argon
---

## Table of Contents ##

* Table of contents
{:toc}

# Introduction #

By default, Argon looks for a configuration file called `$HOME/argon.ini`,
but you can specify an alternate configuration file as the first command-line
argument.

The high-level structure of the configuration is the
[INI format](http://en.wikipedia.org/wiki/INI_file). An INI file is a
set of sections, where each section contains a set of key-value pairs:

    [some section]
    foo = bar
    baz = quux

Some keys and values have additional syntactic structure which is understood
by Argon, as you will see below. Unfortunately, the current parser is a bit
slapdash, so these rules are often rather rigid. I intend to eventually
replace the parser with a better one which will offer improved error messages
and a less rigid syntax. I'm not going to guarantee that future formats will
be backward-compatible, but they will be similar.


# Tables, Layouts, and Spaces #

Tables, layouts, and spaces are used to define your tiles.

## Tables ##

A table is a grid of columns and rows, where the height of each column and the
width of each row are given in pixels.

A table looks like this:

    [table foo]
    cols = 320 320
    rows = 240 240

## Layouts ##

A layout is a way of arranging tiles on a table. Each tile is given by the
column and row of the upper-left table cell it covers as well as the number
of table cells it spans across and down.

A layout looks like this:

    [layout bar]
    table = foo
    tile0 = 0 0 1 1
    tile1 = 1 0 1 2
    tile2 = 0 1 1 1

The four integers which describe a tile are the column, row, column span,
and row span, respectively.

## Spaces ##

A space (short for "workspace" and also known as a "virtual desktop") is an
instantiation of a layout. If two spaces use the same layout, they will have
identically-arranged tiles, but each space nonetheless has a distinct set of
tiles. Each space also defines which tile should have the focus initially.

Each table, layout, tile, and space has a name, and their namespaces are
distinct. Furthermore, the tiles of each space are in distinct namespaces.

A space looks like this:

    [space baz]
    layout = bar
    start = tile2

Each space also has a layer of floating windows above its tiles. These layers
are generally treated like tiles.

# Commands #

Commands may be executed in two ways: by pressing some key or combination of
keys bound to a command, or by the occurrence of an event for which a
trigger is defined.

## Tile Queries ##

A tile query is a description of a set of tiles. Tile queries may depend on
the runtime state of Argon, so they are evaluated every time they are used.
Certain commands require tile queries as arguments.

Syntax        | Description
--------------|-------------------
`hist-back`   | The previously-focused tile
`hist-fwd`    | The tile from which the focus previously went back
`dir-up`      | The tile above
`dir-down`    | The tile below
`dir-left`    | The tile to the left
`dir-right`   | The tile to the right
`foo/bar`     | Tile `bar` on space `foo` (see below)
`Q R`         | Union of results of queries `Q` and `R`
`Q - R`       | Set difference of results of queries `Q` and `R`
`emptiest: Q` | The results of query `Q` sorted by emptiness

The `foo/bar` syntax for referencing a particular tile also supports
certain wildcards:

Wildcard | As a space | As a tile
:-------:|------------|---------------------
`.`      | Current    | Current
`*`      | All        | All
`+`      |            | All except the floating "tile"
`^`      |            | The floating "tile"

## List of Commands ##

First, the syntax of the syntax:

Metasyntax | Meaning
-----------|-----------------
`TQ`       | A tile query
`(A B)`    | A sequence of arguments
`[A B]`    | A sequence of optional arguments
`{A B}`    | A set of Boolean flags which may be given in any order
`A ...`    | A sequence of any number of `A` arguments

And here are the actual commands:

Syntax                        | Meaning
------------------------------|---------------------------
`move [TQ] TQ {broad deep}`   | Move windows (see below)
`focus TQ`                    | Move the focus to the first query result
`exec PROGRAM ARG ...`        | Execute a program with arguments
`seq COMMAND (; COMMAND) ...` | Execute a sequence of commands (see below)
`enable_keys KEYS ...`        | Enable some keybinding groups (see below)
`disable_keys KEYS ...`       | Disable some keybinding groups (see below)
`show_float`                  | Show the floating layer of the current space
`hide_float`                  | Hide the floating layer of the current space
`quit`                        | Quit Argon
`kill`                        | Kill the client owning the focused window
`delete`                      | Delete (close) the focused window
`next_win`                    | Focus the next window in the same tile
`prev_win`                    | Focus the previous window in the same tile
`space_menu`                  | Display a menu of all spaces (see below)

### The `move` Command ###

The `move` command moves windows. It is complex and powerful, but intended
to be simple in the most common cases. For example, `move dir-right` moves
the currently-focused window one tile to the right. Read on if you want a
complete specification of the behavior of `move`.

The default for the first tile query, which identifies the source, is `./.`
(the current tile). The second tile query identifies the destination.

If the `broad` flag is given, then windows are moved from all tiles selected
by the first query. Otherwise, windows are only moved from the first tile
thus selected.

If the `deep` flag is given, then all windows in the selected tiles are moved.
Otherwise, only the top window from each selected tile is moved.

### The `seq` Command ###

Each semicolon which separates arguments to `seq` must be surrounded on both
sides with whitespace.

### The `space_menu` Command ###

The `space_menu` command pops up a menu of all your spaces, and then switches
to the space you select. You must have
[dmenu](http://tools.suckless.org/dmenu/) on your execution path to use this
command.

I intend to eliminate this command in the future once I have implemented
certain other features that will make it redundant. It exists for now because
I need it.

# Keybindings #

Keybindings are organized into groups. Each group may be enabled and
disabled independently of the others using the `enable_keys` and
`disable_keys` commands described above.

A group of keybindings is described by a section in the configuration like
this:

    [keys foo]
    C 1 x = delete
    F2 = next_win

A key to bind is described by listing the modifier keys which must be held
down, followed by the key to hit. The supported modifiers are:

Modifier | Meaning
:-------:|-------------
`C`      | Control
`S`      | Shift
`1`      | Mod1 (typically Alt)
`4`      | Mod4 (typically Windows/Super)

If you aren't sure what keys are mapped to these modifiers, run `xmodmap` to
get a listing.

The final key of each binding is parsed by the X server in the standard way.
A list of these keys can probably be found in
[`/usr/include/X11/keysymdef.h`](keysymdef.html). You can also determine
information about a key by running `xev` from the console, pressing the key,
and looking at the console output.

# The `global` Section #

Every configuration must have a `global` section which specifies two
important defaults:

    [global]
    start = some_space
    start_keys = some groups of keys

# Triggers #

A trigger is a binding of an event that may occur at runtime to a command.
A keybinding could be seen as a particular kind of trigger, although the
implementation doesn't regard them that way just yet. Only a few triggers
are currently supported.

## `on ready` ##

The `on ready` trigger goes in the `global` section. It executes once after
Argon is ready to manage windows.

    [global]
    on ready = exec /home/karl/.argon.ready

## `on enter` ##

The `on enter` trigger goes in a `space` section. It has two variants: one
executes when that space gains the focus from some other space. The other
specifies a particular tile and executes whenever that tile gains the focus.

    [space foo]
    on enter = enable_keys foo_keys
    on enter some_tile = exec xterm

# Attraction #

An attraction causes new windows matching a certain description to be placed
in a tile defined by the attraction rather than the current tile.

Each attraction has its own section. The key-value pairs in that section
specify the description to match against. The `tile` key is special: it
specifies a tile query that says where the new window should go.

Attractions may, like keybindings, be seen as specialized triggers, although
the implementation does not yet reflect this.

Here is an example:

    [attract t]
    name = foo
    class = bar
    role = baz
    transient = no
    tile = emptiest: any/where but/here

Now, a more rigorous description of each key:

Key         | Value
------------|----------------
`name`      | An exact match against the WM_NAME property
`class`     | An exact match against the WM_CLASS property
`role`      | An exact match against the WM_WINDOW_ROLE property
`transient` | `yes` or `no`, matching whether the window is a transient
`tile`      | A tile query that says where the windows go

