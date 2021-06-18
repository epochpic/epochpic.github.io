---
draft: false
toc: true
type: docs

title: Compiling EPOCH
linktitle: Compiling
weight: 40
menu:
  documentation:
    parent: Basic usage
    weight: 40
---

To compile EPOCH in the supplied state, you must first change to the
correct working directory. As explained in , the root directory for
EPOCH contains several subdirectories, including separate directories
for each of the 1D, 2D and 3D versions of the code. To compile the 2D
version of the code, you first switch to the "epoch2d" directory using
the command
**`cd $HOME/epoch/epoch2d`**
and then type
**`make`**
and the code will compile. There are certain options within the code
which are controlled by compiler preprocessors and are described in the
next section. When the code is compiled, it creates a new directory
called "bin" containing the compiled binary which will be called
`epoch1d`, `epoch2d` or `epoch3d`. To run the code, just execute the
binary file by typing:
**`./bin/epoch2d`**
or whatever the correct binary is for the dimensionality of the code
that you have. You should be given a screen which begins with the EPOCH
logo, and then reads:

    The code was compiled with no compile time options

    Welcome to EPOCH2D version 4.12.0   (commit v4.12.0-0-gfd74a464-clean)

    Code is running on 1 processing elements

    Specify output directory

At this point, the user simply types in the name of the (already
existing) output directory and the code will read the input deck files
inside the specified directory and start running. To run the code in
parallel, just use the normal mpirun or mpiexec scripts supplied by your
MPI implementation. If you want the code to run unattended, then you
will need to pipe in the output directory name to be used. The method
for doing this varies between MPI implementations. For many MPI
implementations (such as recent versions of OpenMPI) this can be
achieved with the following:
**`echo Data | mpirun -np 2 ./bin/epoch2d`**
Some cluster setups accept the following instead:
**`mpirun -np 2 ./bin/epoch2d < deck.file`**
where "deck.file" is a file containing the name of the output directory.
Some cluster queueing systems do not allow the use of input pipes to
mpirun. In this case, there is usually a "-stdin" command line option to
specify an input file. See your cluster documentation for more details.

As of version 4.2.12, EPOCH now checks for the existence of a file named
"USE_DATA_DIRECTORY" in the current working directory before it
prompts the user for a Data directory. If such a file exists, it reads
it to obtain the name of the data directory to use and does not prompt
the user. If no such file exists, it prompts for a data directory name
as before. This is useful for cluster setups in which it is difficult or
impossible to pipe in the directory name using a job script.

The "Makefile" contains configurations for fort, gfortran, pgi, g95,
hector/archer and ibm (the compiler suite used on IBM's BlueGene
machines). In order to compile using one of the listed configurations,
add the "`COMPILER=`" option to the "`make`" command. For example
**`make COMPILER=gfortran`**
will compile the code using the gfortran compiler and appropriate
compiler flags. The options are

-   COMPILER=gfortran - GNU Fortran
-   COMPILER=intel - Intel ifort
-   COMPILER=pgi - Portland group compiler
-   COMPILER=g95 - G95 compiler
-   COMPILER=ibm - IBM AIX Fortran compiler for BlueGene
-   COMPILER=hector - Cray compiler as used on hector and archer

As of version 4.11, it is now possible for the build system to
automatically detect the correct compiler to use. Typing
`make COMPILER=auto` will cause the build system to guess which compiler
is in use. Note that this might not always work, so it is better to use
the correct value for `COMPILER` if it is already known.

You can also compile the code with debugging flags by adding
"`MODE=debug`" and can compile using more than one processor by using
"`-j<n>`", where "`<n>`" is the number of processors to use. Note that
this is just to speed up the compilation process; the resulting binary
can be run on any number of processors.
