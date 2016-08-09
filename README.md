Pipes Filesystem
----------------

An experimental library that provides basic functions for efficient streaming over directory trees, compatible with the [Haskell Pipes](https://github.com/Gabriel439/Haskell-Pipes-Library) ecosystem.

Please note that this library is still experimental and in development. Using it may delete all the files on your hard disk or even burn down your house (although it probably won't).

Functionality
-------------

Use [Pipes.FileSystem.children](https://github.com/jonathanknowles/pipes-filesystem/blob/master/source/Pipes/FileSystem.hs) to iterate (non-recursively) over all children of given directory.

Use [Pipes.FileSystem.descendants](https://github.com/jonathanknowles/pipes-filesystem/blob/master/source/Pipes/FileSystem.hs) to iterate (recursively) over all descendants of a given directory.

Traversal order can be specified using [Pipes.FileSystem.TraversalOrder](https://github.com/jonathanknowles/pipes-filesystem/blob/master/source/Pipes/FileSystem.hs).

Compatibility
-------------

This library uses Linux-specific system functions to reduce the number of [file status retrieval calls](http://man7.org/linux/man-pages/man2/stat.2.html) to a bare minumium. Under most circumstances this should yield better performance than would be obtained by relying on [POSIX-compliant calls](http://pubs.opengroup.org/onlinepubs/009695399/functions/stat.html).

Currently there isn't support for Windows or other UNIX-like operating systems, but this could be added if enough people want it. Feel free to contribute a patch! (I might add support for this in a later version.)

Building
--------

Requires a patched version of the [Haskell Unix package](https://github.com/jonathanknowles/unix/), which provides Linux-specific file system functions.

This may change in future.
