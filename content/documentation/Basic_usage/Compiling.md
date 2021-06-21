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



<!-- ########################  Cross references  ######################## -->


[Acknowledging_EPOCH]: /tutorial/acknowledging_epoch
[Basic_examples]: /tutorial/basic_examples
[Basic_examples__focussing_a_gaussian_beam]: /tutorial/basic_examples/#focussing_a_gaussian_beam
[Binary_files]: /tutorial/binary_files
[Calculable_particle_properties]: /tutorial/calculable_particle_properties
[Compiler_Flags]: /tutorial/compiler_flags
[Compiling]: /tutorial/compiling
[FAQ]: /tutorial/faq
[FAQ__how_do_i_obtain_the_code]: /tutorial/faq/#how_do_i_obtain_the_code
[Input_deck]: /tutorial/input_deck
[Input_deck_adf]: /tutorial/input_deck_adf
[Input_deck_boundaries]: /tutorial/input_deck_boundaries
[Input_deck_boundaries__cpml_boundary_conditions]: /tutorial/input_deck_boundaries/#cpml_boundary_conditions
[Input_deck_boundaries__thermal_boundary_conditions]: /tutorial/input_deck_boundaries/#thermal_boundary_conditions
[Input_deck_collisions]: /tutorial/input_deck_collisions
[Input_deck_constant]: /tutorial/input_deck_constant
[Input_deck_control]: /tutorial/input_deck_control
[Input_deck_control__basics]: /tutorial/input_deck_control/#basics
[Input_deck_control__maxwell_solvers]: /tutorial/input_deck_control/#maxwell_solvers
[Input_deck_control__requesting_output_dumps_at_run_time]: /tutorial/input_deck_control/#requesting_output_dumps_at_run_time
[Input_deck_control__stencil_block]: /tutorial/input_deck_control/#stencil_block
[Input_deck_control__strided_current_filtering]: /tutorial/input_deck_control/#strided_current_filtering
[Input_deck_dist_fn]: /tutorial/input_deck_dist_fn
[Input_deck_fields]: /tutorial/input_deck_fields
[Input_deck_injector]: /tutorial/input_deck_injector
[Input_deck_injector__keys]: /tutorial/input_deck_injector/#keys
[Input_deck_laser]: /tutorial/input_deck_laser
[Input_deck_operator]: /tutorial/input_deck_operator
[Input_deck_output__directives]: /tutorial/input_deck_output/#directives
[Input_deck_output_block]: /tutorial/input_deck_output_block
[Input_deck_output_block__derived_variables]: /tutorial/input_deck_output_block/#derived_variables
[Input_deck_output_block__directives]: /tutorial/input_deck_output_block/#directives
[Input_deck_output_block__dumpmask]: /tutorial/input_deck_output_block/#dumpmask
[Input_deck_output_block__multiple_output_blocks]: /tutorial/input_deck_output_block/#multiple_output_blocks
[Input_deck_output_block__particle_variables]: /tutorial/input_deck_output_block/#particle_variables
[Input_deck_output_block__single-precision_output]: /tutorial/input_deck_output_block/#single-precision_output
[Input_deck_output_global]: /tutorial/input_deck_output_global
[Input_deck_particle_file]: /tutorial/input_deck_particle_file
[Input_deck_probe]: /tutorial/input_deck_probe
[Input_deck_qed]: /tutorial/input_deck_qed
[Input_deck_species]: /tutorial/input_deck_species
[Input_deck_species__arbitrary_distribution_functions]: /tutorial/input_deck_species/#arbitrary_distribution_functions
[Input_deck_species__ionisation]: /tutorial/input_deck_species/#ionisation
[Input_deck_species__maxwell_juttner_distributions]: /tutorial/input_deck_species/#maxwell_juttner_distributions
[Input_deck_species__particle_migration_between_species]: /tutorial/input_deck_species/#particle_migration_between_species
[Input_deck_species__species_boundary_conditions]: /tutorial/input_deck_species/#species_boundary_conditions
[Input_deck_subset]: /tutorial/input_deck_subset
[Input_deck_window]: /tutorial/input_deck_window
[Landing]: /tutorial/landing
[Landing_Page]: /tutorial/landing_page
[Libraries]: /tutorial/libraries
[Links]: /tutorial/links
[Maths_parser__functions]: /tutorial/maths_parser/#functions
[Non-thermal_initial_conditions]: /tutorial/non-thermal_initial_conditions
[Previous_versions]: /tutorial/previous_versions
[Python]: /tutorial/python
[Running]: /tutorial/running
[SDF_Landing_Page]: /tutorial/sdf_landing_page
[Structure]: /tutorial/structure
[Using_EPOCH_in_practice]: /tutorial/using_epoch_in_practice
[Using_EPOCH_in_practice__manually_overriding_particle_parameters_set_by_the_autoloader]: /tutorial/using_epoch_in_practice/#manually_overriding_particle_parameters_set_by_the_autoloader
[Using_EPOCH_in_practice__parameterising_input_decks]: /tutorial/using_epoch_in_practice/#parameterising_input_decks
[Using_delta_f]: /tutorial/using_delta_f
[Visualising_SDF_files_with_IDL_or_GDL]: /tutorial/visualising_sdf_files_with_idl_or_gdl
[Visualising_SDF_files_with_LLNL_VisIt]: /tutorial/visualising_sdf_files_with_llnl_visit
[Workshop_examples]: /tutorial/workshop_examples
[Workshop_examples__a_2d_laser]: /tutorial/workshop_examples/#a_2d_laser
[Workshop_examples__a_basic_em-field_simulation]: /tutorial/workshop_examples/#a_basic_em-field_simulation
[Workshop_examples__getting_the_example_decks_for_this_workshop]: /tutorial/workshop_examples/#getting_the_example_decks_for_this_workshop
[Workshop_examples__specifying_particle_species]: /tutorial/workshop_examples/#specifying_particle_species
[Workshop_examples_continued]: /tutorial/workshop_examples_continued
