Pipes Filesystem
----------------

An experimental library that provides functions for efficient streaming over directory trees, compatible with the [Haskell Pipes](https://github.com/Gabriel439/Haskell-Pipes-Library) ecosystem.

Please note that this library is still experimental and in development. Using it may delete all the files on your hard disk or even burn down your house (although it probably won't).

Compatibility
-------------

This library uses Linux-specific system functions to reduce the number of file status retrieval calls to a bare minumium. Under most circumstances this should yield better performance than would be obtained by relying on POSIX-compliant calls.

Currently there isn't support for Windows or other UNIX-like operating systems, but this could be added if enough people want it. Feel free to contribute a patch! (I might add support for this in a later version.)

Building
--------

Requires a patched version of the [Haskell Unix package](https://github.com/jonathanknowles/unix/), which provides Linux-specific file system functions.

This may change in future.
