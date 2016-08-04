# Rdiff FS

`rdifffs` is a program providing a virtual filesytem view onto a backup
created using [rdiff-backup](http://www.nongnu.org/rdiff-backup/).

## Introduction

`rdiff-backup` backs up one directory (a) to another (b). The destination
(b) contains a full copy of the current (a), plus the required bits and
pieces to recreate previous versions of (a). `rdifffs` exposes the previous
versions in a filesystem.

At the moment, `rdifffs` should be considered alpha quality.

## Example of use

    $ ./rdifffs ./real/dest ./mnt
    $ ls mnt
    drwxr-xr-x 4 jon jon 4096 Feb  7  2010 2010-02-04T18:18:15Z
    drwxr-xr-x 4 jon jon 4096 Feb  7  2010 2010-02-04T18:18:17Z
    drwxr-xr-x 4 jon jon 4096 Feb  7  2010 2010-02-04T18:18:18Z
    drwxr-xr-x 4 jon jon 4096 Feb  7  2010 2010-02-06T12:28:31Z
    drwxr-xr-x 4 jon jon 4096 Feb  7  2010 2010-02-07T13:04:04Z
    drwxr-xr-x 4 jon jon 4096 Feb  7  2010 2010-09-04T16:38:10+01:00
    lr-xr-xr-x 2 jon jon   11 Jan  1  1970 current -> 2010-09-04T16:38:10+01:00
    $ cat mnt/*/a
    Thu Feb  4 18:18:10 GMT 2010
    Thu Feb  4 18:18:17 GMT 2010
    Sat Sep  4 16:38:02 BST 2010
    Sat Sep  4 16:38:02 BST 2010
    Thu Feb  4 18:18:18 GMT 2010
    Sat Sep  4 16:38:02 BST 2010
    Sat Sep  4 16:38:02 BST 2010

### Building/Installing rdifffs

`rdifffs` is written in Haskell and requires a Haskell compiler to build
and a runtime to run. It has been developed with ghc6 version 6.12.1.

`rdifffs` makes use of (at least) the following additional libraries:

 * HFuse, <http://code.haskell.org/hfuse>
 * MissingH, <http://software.complete.org/missingh>
 * parsec (version 2.1.0.1), <http://www.cs.uu.nl/~daan/parsec.html>
 * zlib, <http://hackage.haskell.org/package/zlib/>

Install these via [Cabal](https://wiki.haskell.org/Cabal-Install),
or `apt` on a [Debian](http://debian.org) system.

Consult the supplied [Makefile](Makefile) to build `rdifffs`.

### Running rdifffs

Pass the underlying `rdiff-backup` directory as the first argument. All
the other arguments are handled by FUSE. Supply at least a mount point.

#### Troubleshooting

You might find the fuse argument `-d` useful: this causes `rdifffs` not to
fork and relinquish the terminal. It also prints out a lot of debugging
information (which you could direct at `/dev/null`). During initial development
I found `rdifffs` more stable in this configuration (possibly due to bugs in
HFuse which may no longer exist).

## Author, Copyright

Copyright © 2010-2016 Jonathan Dowland <jon+rdifffs@dow.land>

`rdifffs` is distributed under the terms of the BSD 3-clause license.
See [LICENSE](LICENSE) for copyright information.
