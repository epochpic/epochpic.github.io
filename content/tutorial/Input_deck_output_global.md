+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "Input deck output global"
[menu.tutorial]
  parent = "Content"
  weight = 360
+++

This block contains parameters which should be applied to all output
blocks. See [EPOCH input deck][Input_deck] for more
information on the input deck.

With the introduction of multiple output blocks, there are now a few
parameters that only make sense to be applied globally across all output
blocks. To accommodate this, a new block named "output_global" has been
added. Most of the parameters accepted by this block have the same
meaning as those in the "output" block except that they are applied to
all "output" blocks.

The parameters that can be specified in the "output_global" block are
as follows:\
- `force_first_to_be_restartable` - Logical flag which
determines whether the file written at time zero is a restart dump. The
default value is "F".\
- `force_last_to_be_restartable` - Force the code to
override other output settings and make the last output dump it writes
be a restart dump. Any internal condition which causes the code to
terminate will make the code write a restart dump, but code crashes or
scheduler terminations will not cause the code to write a restart dump.
"force_final_to_be_restartable" is accepted as a synonym. The
default value is "T".\
- `dump_first` - Logical flag which determines whether to
write an output file immediately after initialising the simulation. The
default is "F".\
- `dump_last` - Logical flag which determines whether to
write an output file just before ending the simulation. The default is
"T" if an output block exists in the input deck and "F" otherwise.
"dump_final" is accepted as a synonym.\
- `time_start` - Floating point parameter which specifies
the simulation time at which to start considering output for all output
blocks. Note that if "dump_first" or "dump_last" are set to true for
any block then dumps will occur at the first or last timestep regardless
of the value of this parameter. This also applies to the three following
parameters. The default value is 0.\
- `time_stop` - Floating point parameter which specifies the
simulation time at which to stop considering output for all output
blocks. The default value is the largest possible float.\
- `nstep_start` - Integer parameter which specifies the step
number at which to start considering output for the block. The default
value is 0.\
- `nstep_stop` - Integer parameter which specifies the step
number at which to stop considering output for the block. The default
value is the largest possible integer.\
- Floating point parameter which specifies the elapsed walltime in
seconds at which to start considering output for all output blocks. Note
that if **dump_first** or **dump_last** are set to true for any blocks
then dumps will occur at the first or last timestep regardless of the
value of the **walltime_start** parameter. The default value is 0.\
- Floating point parameter which specifies the elapsed walltime in
seconds at which to stop considering output all output blocks. The
default value is the largest possible float.\
- `sdf_buffer_size` - Integer parameter. When writing
particle data to an SDF file, the data is first transferred into an
output buffer. The size of this buffer can have a big impact on the
overall speed of writing dump files. This parameter allows the size of
the buffer to be specified in bytes. The default value is 67108864 (64
MB).\
- `filesystem` - String parameter. Some filesystems can be
unreliable when performing parallel I/O. Often this is fixable by
prefixing the filename with 'ufs' or 'nfs'. This parameter supplies the
prefix to be used. The default value is an empty string.\
- `use_offset_grid` - When using moving windows some
visualisation programs (notably VisIt) show the motion of the window by
moving the visualisation window rather than by changing the x-axis.
Setting this option to "T" causes the code to write another grid which
always gives the offset relative to the left hand edge of the window
rather than the true origin. Performs no function when not using the
moving window. The default value is "F".\
- ` dump_first_after_restart` - Logical flag to enable a
dump to occur immediately after restart. In the past, a `dump_first`
flag in the output block would cause an output dump immediately after
restarting. Since this is rarely the desired behaviour, the flag is now
ignored when restarting. To force a dump to occur immediately after
restart, set `dump_first_after_restart = T` in the output block. The
default value is "F".

# Next section {#next_section}

[The dist_fn block][Input_deck_dist_fn]


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
