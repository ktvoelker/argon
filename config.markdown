---
layout: default
title: Configuring Argon
---

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


## Tables, Layouts, and Spaces ##

Tables, layouts, and spaces are used to define your tiles.

### Tables ###

A table is a grid of columns and rows, where the height of each column and the
width of each row are given in pixels.

A table looks like this:

    [table foo]
    cols = 320 320
    rows = 240 240

### Layouts ###

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

### Spaces ###

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

## Commands ##

Commands may be executed in two ways: by pressing some key or combination of
keys bound to a command, or by the occurrence of an event for which a
trigger is defined.

### Tile Queries ###

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

### List of Commands ###

TODO

## Keybindings ##

Keybindings are organized into groups. Each group may be enabled and
disabled independently of the others.

TODO

