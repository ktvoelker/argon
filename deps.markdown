---
layout: default
title: Dependencies
---

The Haskell dependencies of Argon will be installed automatically by `cabal`,
but if you really want to know, you can look inside `argon.cabal` for a list.

Once you have the `argon` binary, you don't need any Haskell stuff to run it.
Here is the list of binary dependencies as reported by `ldd` on my Arch Linux
system (which is where the binary package is built):

	   linux-gate.so.1 =>  (0xb770a000)
	   libXinerama.so.1 => /usr/lib/libXinerama.so.1 (0xb76e5000)
	   libXext.so.6 => /usr/lib/libXext.so.6 (0xb76d7000)
	   libX11.so.6 => /usr/lib/libX11.so.6 (0xb75bd000)
	   librt.so.1 => /lib/librt.so.1 (0xb75b4000)
	   libutil.so.1 => /lib/libutil.so.1 (0xb75af000)
	   libdl.so.2 => /lib/libdl.so.2 (0xb75ab000)
	   libgmp.so.10 => /usr/lib/libgmp.so.10 (0xb754d000)
	   libm.so.6 => /lib/libm.so.6 (0xb7528000)
	   libc.so.6 => /lib/libc.so.6 (0xb73dd000)
	   libxcb.so.1 => /usr/lib/libxcb.so.1 (0xb73c5000)
	   libpthread.so.0 => /lib/libpthread.so.0 (0xb73aa000)
	   /lib/ld-linux.so.2 (0xb770b000)
	   libXau.so.6 => /usr/lib/libXau.so.6 (0xb73a7000)
	   libXdmcp.so.6 => /usr/lib/libXdmcp.so.6 (0xb73a2000)

Everything on this list, other than `libgmp`, looks like a standard C, Linux,
or X11 library. Please [email me](mailto:ktvoelker@gmail.com) if you have
any trouble with Argon's dependencies.

