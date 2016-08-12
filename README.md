Pipes Filesystem
----------------

An experimental library that provides basic functions for efficient streaming over directory trees, compatible with the [Haskell Pipes](https://github.com/Gabriel439/Haskell-Pipes-Library) ecosystem.

Please note that this library is still **experimental** and in development. Using it may delete all the files on your hard disk or even burn down your house (although it probably won't).

Functionality
-------------

Use [`Pipes.FileSystem.children`](https://github.com/jonathanknowles/pipes-filesystem/blob/master/source/Pipes/FileSystem.hs) to iterate (non-recursively) over all children of given directory.

Use [`Pipes.FileSystem.descendants`](https://github.com/jonathanknowles/pipes-filesystem/blob/master/source/Pipes/FileSystem.hs) to iterate (recursively) over all descendants of a given directory.

Traversal order can be specified using [`Pipes.FileSystem.TraversalOrder`](https://github.com/jonathanknowles/pipes-filesystem/blob/master/source/Pipes/FileSystem.hs).

Performance
-----------

In order to stream the contents of directory trees as efficiently as possible, this library uses the [Linux `readdir` function](http://man7.org/linux/man-pages/man3/readdir.3.html) which provides extended type information about each directory entry. (This is in contrast to the [POSIX-compliant version of `readdir`](http://pubs.opengroup.org/onlinepubs/009695399/functions/readdir_r.html), which only provides the *name* of each directory entry, and *no* type information.) This type information is made available to downstream consumers through the [`FileInfo`](https://github.com/jonathanknowles/pipes-filesystem/blob/master/source/Pipes/FileSystem.hs) type, thus making it possible for consumers to avoid the expense of calling [`stat`](http://man7.org/linux/man-pages/man2/stat.2.html) in many situations.

Compatibility
-------------

Currently this library only supports Linux. Support for other operating systems such as Windows or other UNIX-like operating systems could be added if enough people want it. Feel free to contribute a patch!

Building
--------

Requires a [patched version](https://github.com/jonathanknowles/unix/) of the [Haskell Unix package](https://github.com/haskell/unix/), which provides Linux-specific file system functions.

This may change in future.
