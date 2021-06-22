---
draft: false
toc: true
type: docs

title: Running EPOCH and basic control of EPOCH
linktitle: Running
weight: 60
menu:
  documentation:
    parent: Basic usage
    weight: 60
---

When the code is run, the output is

            d########P  d########b        .######b          d#######  d##P      d##P
           d########P  d###########    d###########     .##########  d##P      d##P
          ----        ----     ----  -----     ----   -----         ----      -- P
         d########P  d####,,,####P ####.      .#### d###P          d############P
        d########P  d#########P   ####       .###P ####.          d############P
       d##P        d##P           ####     d####   ####.         d##P      d##P
      d########P  d##P            ###########P     ##########P  d##P      d##P
     d########P  d##P              d######P          #######P  d##P      d##P

    The code was compiled with no compile time options

    Welcome to EPOCH2D version 4.12.0   (commit v4.12.0-0-gfd74a464-clean)

    Code is running on 1 processing elements

    Specify output directory

At which point the end user should simply type in the name of the
directory where the code output is to be placed. This directory must
also include the file which controls the code setup, specifies how to
set the initial conditions and controls the I/O. Writing an input deck
for EPOCH is fairly time consuming and so the code is supplied with some
example input decks which include all the necessary sections for the
code to run. Alternately, the code checks for the Data directory in a
file named "USE_DATA_DIRECTORY" before prompting at the command-line.
This allows the code to be run without waiting for input at the
command-line.
