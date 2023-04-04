---
draft: false
toc: true
type: docs

title: EPOCH Makefile
linktitle: Makefile
weight: 270
menu:
  developer:
    parent: Overview 
    weight: 20
---

The makefile supplied with EPOCH is a standard GNU make makefile, which must
be user modified to allow a developer to add new files to the code. EPOCH's
makefile is quite large, so an explanation of how to add new files and new
directories is given below.

## Adding a new file to be compiled with EPOCH

There are three things that must be done to cause EPOCH to compile a new
file and link it into the final code. Assume that you're adding a file called
"newfile.F90". First, find the line which sets the environment variable
`SRCFILES` and add a new parameter which reads "newfile.F90". This tells the 
makefile to compile the final code using your new file, the
next thing to do is to add a line which tells the code about the dependencies
for your file. Lower down in the makefile, you'll find a section with lines
which look like:

 ```perl
balance.o: balance.F90 boundary.o mpi_subtype_control.o partlist.o
 ```

Add a new line for describing all the FILES (NOT modules) which are used by
your new file. If you USE shared_data, mpi_subtype_control and stack in
your file then the line would look like:
 ```perl
newfile.o: newfile.F90 shared_data.o mpi_subtype_control.o stack.o
 ```
Note the structure of the line with ONLY the source file for the new file
specified, all other used files specify the intermediate .o files. The
remaining element of the makefile which needs to be modified is to add your
new file as a dependency to all the files which USE modules contained in your
new file. This is achieved very simply by adding "newfile.o" to the dependency
list for those files which USE your modules. For example if you've written new
boundary conditions and USE your modules in boundary.f90, you'd just change
the line for boundary.f90 from:
 ```perl
boundary.o: boundary.f90 deck_io_block.o particle_temperature.o partlist.o
 ```
to
 ```perl
boundary.o: boundary.f90 deck_io_block.o particle_temperature.o partlist.o \
  newfile.o
 ```

Note that the backslash characters are line continuation marks in makefiles.

## Adding new directories to EPOCH's makefile
If you want to add an entire new directory to the EPOCH compile path then
you need to add it to the definition of the variable `VPATH`.
Remember to use the variable `$(SRCDIR)` rather than hard-coding
`src` into the path.