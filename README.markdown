
Argon
=====

This is Argon, a declarative window manager. Its purpose is to do what I want.
If you want what I want, then you might want Argon.

What do I want?
---------------
* I want a tiling window manager that allows floating windows above the tiles.
* I want to configure my tile layouts once and never change them again.
* I want every window to always be in exactly one tile until I tell it to be
  somewhere else, whether or not that window is visible.
* I want certain kinds of windows to always be placed in particular tiles.

In summary, I want to minimize the work I have to do, both with the keyboard
and with my mind. For me, minimizing mental work requires maximizing the
predictability of the window manager's behavior.

Building Argon
--------------
The official method of building Argon is with `cabal` tool which is included
in the `cabal-install` package available from
http://www.haskell.org/cabal/download.html. To build, run:

    cabal configure
    cabal build

The executable will be found at `dist/build/argon/argon`.

Running Argon
-------------
Argon needs two pieces of information to get started: the X display to connect
to and the configuration file to read. There are defaults for both.

To specify a particular X display, set the `DISPLAY` variable in the
environment in which you execute `argon`. The default display is `:0.0`.

To specify a particular configuration file, pass its name as the first
command-line argument to `argon`. The default configuration file is `.argonrc`
in the directory given by the `HOME` environment variable.

The Configuration File
----------------------
The Argon configuration file is roughly a traditional INI file. For more
detail about the low-level syntax of the file, you can read about the library
which is used to parse it at http://hackage.haskell.org/package/ConfigFile.

Take a look at the example configuration in the repository, `src/argon.ini`,
for more detail about what is expected in the file.

