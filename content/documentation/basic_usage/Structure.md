---
draft: false
toc: true
type: docs

title: Code structure
linktitle: Structure
weight: 20
menu:
  documentation:
    parent: Basic usage
---

When obtained, the EPOCH codes all have a similar structure. If the
tarred and gzipped archive (commonly referred to as a tarball) is
downloaded and unpacked into the user's `$HOME` directory, then the
extracted contents will consist of a directory named
"`$HOME/epoch-4.12.0`" (with "4.12.0" substituted by the current version
number) and the subdirectories and files listed below.

Alternatively, if the code is checked out from the GitHub git repository
with the command

`git clone --recursive https://github.com/Warwick-Plasma/epoch.git`

then the directory will be "`$HOME/epoch`".

Once the code has been obtained, the top-level directory will contain
the following 4 directories and several files

-   epoch1d - Source code and other files required for the 1D version of
    EPOCH.
-   epoch2d - Source code and other files required for the 2D version of
    EPOCH.
-   epoch3d - Source code and other files required for the 3D version of
    EPOCH.
-   SDF - Source code for the SDF file format which is used to generate
    output for EPOCH runs. This directory also includes various tools
    and readers for working with SDF files.
-   CHANGELOG - A brief overview of the change history for each released
    version of EPOCH.
-   CODING_STYLE - This document contains the conventions which must be
    used for any code being submitted for inclusion in the EPOCH
    project.
-   LICENSE - A copy of the GPLv3 license which is used by the EPOCH
    project.
-   README.md - A brief overview of obtaining and using the EPOCH code.
-   make_tarball.sh - This is a shell script which is used for creating
    the tarred and gzipped archives of EPOCH which are posted to the
    GitHub server each time a new release is made.
-   test_all.sh - A regression test script used when testing the code.

The three EPOCH subdirectories all have a similar structure. Inside each
of the epoch{1,2,3}d directories, there are 3 sub-directories:

-   src - The EPOCH source code.
-   example_decks - A sample data directory containing example input
    deck files.
-   Data - This is an empty directory to use for running simulations.

there are also 3 files:

-   Makefile - A standard makefile.
-   Start.pro - An IDL script which starts the IDL visualisation
    routines. Execute it using "idl Start".
-   unpack_source_from_restart - Restart dumps can be written to
    contain a copy of the input decks and source code used to generate
    them. This script can be used to unpack that information from a
    given restart dump. It is run from the command line and must be
    passed the name of the restart dump file.
