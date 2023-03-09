---
draft: false
toc: true
type: docs

title: Quick-start
linktitle: Quick-start
weight: 350
menu:
  documentation:
    parent: Quick start
    weight: 20
---

# Quick-start guide {#ultra_simple_getting_epoch_guide}

## Pre-requisites

EPOCH is mostly a self-contained code. The code is intended to be run on a Linux-based operating system, and this section will be written for Linux machines. Experienced programmers may be able to adapt these instructions for Windows and Mac, or may choose to run EPOCH from a virtual Linux machine if they don't have access to Linux.

To compile and run EPOCH, the system requires:
- A fortran compiler (e.g. GNU Fortran)
- An MPI library (e.g. Open MPI)

These packages can easily be installed by entering the commands

```
sudo apt-get install gfortran
sudo apt-get install openmpi-bin openmpi-common libopenmpi-dev libgtk2.0-dev
```

into a Linux terminal.

## Installing EPOCH

These instructions should work in your host institute if you have git. To install EPOCH, open a linux terminal and type into a command line:
```
git clone --recursive https://github.com/Warwick-Plasma/epoch.git
```

You will now have a directory called 'epoch'. Inside this directory will
be three EPOCH sub-directories `epoch1d`, `epoch2d` and `epoch3d`, an `SDF`
directory and a few other files. EPOCH is split into 3 codes, one for each dimension - these must be built separately.

If you are unable to access git, you may download a compressed tar-file containing the code from the ['Releases'](https://github.com/Warwick-Plasma/epoch/releases)
section on the EPOCH GitHub webpage. Note that this will not include any bug-fixes since 
the last release, so it is strongly recommended to use the git installation.

## Building the code

To build one of the EPOCH codes, navigate to either the `epoch1d`, `epoch2d` or `epoch3d` directories within a Linux terminal, and run:

```
make COMPILER=gfortran -j4
```

Here we assume you are compiling using mpif90, other compiler options are available. The `-j4` key splits the compilation between 4 processors for a speed-up, the 4 can be replaced with a higher number if your system permits.

This command installs a basic version of EPOCH - some additional features like bremsstrahlung radiation or particle-ID require adjustments to the Makefile. The optional compilation flags are discussed [here][Compiler flags].

## Running the code {#running_the_codes}

EPOCH simulations are specified using an input file called `input.deck`, which is read by the code. Example input decks are present in the `example_decks` sub-directory in the `epoch1d`, `epoch2d` and `epoch3d` directories. Whichever dimension you chose, `ionisation.deck` provides a small example simulation which can be run using the default EPOCH build.

Once you have a file called `input.deck`
(either copied and renamed from `ionisation.deck`, or something you have written yourself), then you may run the code.

To run any EPOCH code, you must first navigate to the sub-directory of the code in a Linux terminal, such that the `ls` command shows the `bin` sub-directory. EPOCH expects you to be here when looking up physics tables. For example, the 2d code can be run by entering `epoch2d` and running:

```
mpirun -np 4 ./bin/epoch2d
```

You will then be prompted to enter the path to the directory containing your `input.deck` file. This step may be skipped by piping in the path directly into the run command, using:

```
mpirun -np 4 ./bin/epoch2d <<< /path/to/input/
```

where /path/to/input is the path to the directory containing the `input.deck`.


# Visualising the data {#visualising_the_data}

The code generates data in the SDF format. The data is
written to a self-describing file format called SDF. This has been
developed for use by several codes maintained at the University of
Warwick. There are routines for reading the data from within IDL, VisIt,
MatLab and Python.

More complete documentation on visualisation routines is available
[here][Visualisation]

As a quick summary:

- MATLAB: To read data from the SDF file "0007.sdf", run the command: `data = GetDataSDF('0007.sdf');`. The `GetDataSDF` function is present in the folder `SDF/MATLAB`.

- Python: Data may be extracted and plotted using the `sdf_helper` library, which can be built using the Makefile. Data can then be read using:

```python 
import sdf_helper as sh
data = sh.getdata(7)
```

- IDL/GDL: The following commands are useful for listing the variables and reading the data: `list_variables,7,'Data'`, `data = getstruct(7,/varname)`, `help,data,/struct`, where `/varname` is the name of a variable output from the list. More detailed instructions are present [here][Visualising_SDF_files_with_LLNL_VisIt]

- VisIt: Consult the running instructions at this URL: [
here][Visualising_SDF_files_with_LLNL_VisIt]

# Further examples {#other_laser_plasma_example_decks}

Now the basics have been covered, you're free to start running EPOCH. Documentation on writing input deck files is given [here][input_decks]. There are also further examples from previous workshops we have run, which can be viewed in these pages: 

[Basic examples][Basic_examples]

[Workshop][Workshop_examples_continued]



<!-- ########################  Cross references  ######################## -->

[Basic_examples]: /documentation/examples/Basic_examples
[Compiler flags]: /documentation/basic_usage/Compiler_Flags
[Input_deck_output_block]: /documentation/input_deck/input_deck_output_block
[input_decks]: /documentation/input_deck
[Landing_Page]: /documentation
[Visualisation]: /documentation/visualising_output
[Visualising_SDF_files_with_LLNL_VisIt]: /documentation/visualising_output/visualising_sdf_files_with_llnl_visit
[Workshop_examples_continued]: /documentation/examples/workshop_examples_continued
