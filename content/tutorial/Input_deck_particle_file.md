+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "Input deck particle file"
[menu.tutorial]
  parent = "Content"
  weight = 370
+++

This block contains information about the block used to load particles
from file. See [EPOCH input deck][Input_deck] for more
information on the input deck.

The particles_from_file block is similar in function to the fields
block, it allows the loading of custom particle data from raw binary
data files. An example usage of the block is shown below

```perl
begin:particles_from_file 
   species = "electron" 

   # Load mandatory data for 3D simulation
   x_data = "xdata.dat"
   y_data = "ydata.dat"
   z_data = "ydata.dat"
   w_data = "ydata.dat"

   # Load particle ids in 4 byte int format,
   # ignoring first 8 bytes of file 
   #offset = 8
   #id4_data = "iddata.dat"
end:particles_from_file
```

Specifying a particles_from_file block for a species causes EPOCH to
load the per-particle data from the specified files. Data files are
assumed to be in order such that the first variable in each file will be
attributed to one particle, the second variable in each file to a second
electron, and so on. A target species to load to, as well as particle
position and weight data (unless has been set) must be supplied. With
the exception of particle ID, any optional parameters which are left
unspecified will be initialised to zero.\
If the code has been compiled with or then particle IDs may be loaded
from a raw binary file of integers of either size 4 or size 8 regardless
of the compile time flag choice. If no particle ID data is supplied, IDs
will be generated sequentially from 1.\
All other data should be in the form of floating point numbers of the
same precision as in the core code.\
A particles_from_file block accepts the following parameters:\
- `species` - Name of the species to which the particles will
be loaded. This is a mandatory parameter and the corresponding species
block must be defined.\
- `{x,y,z}_data` - File containing particle position data in
$m$. This data must be supplied, up to the dimensionality of the
simulation.\
- `w_data` - File containing pseudoparticle weight, this is
the number of real particles the pseudoparticle represents. This data
must be supplied.

-   `{px,py,pz}_data` - File containing particle momentum
    data in $kg\,ms^{-1}$. The default value is zero.\
-   `id{4,8}_data` - File containing particle IDs in either
    4 or 8 byte unsigned integer representation.\
-   `offset` - File offset. Number of bytes at the head of
    the file to be ignored, may be specified multiple times. see for
    more details of behaviour.\

# Next section {#next_section}

[The window block][Input_deck_window]


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
