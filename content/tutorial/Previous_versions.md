---
draft: false
toc: true
type: docs

title: Previous versions
linktitle: Previous versions
weight: 300
menu:
  tutorial:
    parent: Code details
    weight: 50
---

Changes between version 3.1 and 4.0 {#changes_between_version_3.1_and_4.0}
===================================

# Changes to the Makefile {#changes_to_the_makefile}

Some changes have been made to the Makefile. These are documented
[here][Compiler_Flags].
The following compile-time defines have been added to the Makefile:

-   NO_IO
-   PARTICLE_ID
-   PARTICLE_ID4
-   COLLISIONS_TEST
-   PHOTONS
-   TRIDENT_PHOTONS
-   PREFETCH

The following compile-time defines have been removed from the Makefile:

-   COLLISIONS
-   SPLIT_PARTICLES_AFTER_PUSH
-   PARTICLE_IONISE

# Major features and new blocks added to the input deck {#major_features_and_new_blocks_added_to_the_input_deck}

-   [CPML boundary
    conditions][Input_deck_boundaries__cpml_boundary_conditions]
-   [Thermal boundary
    conditions][Input_deck_boundaries__thermal_boundary_conditions]
-   [Collisions][Input_deck_collisions]
-   [QED][Input_deck_qed]
-   [Subsets][Input_deck_subset]
-   [Ionisation][Input_deck_species__ionisation]
-   [Single-precision
    output][Input_deck_output_block__single-precision_output]
-   [Multiple output
    blocks][Input_deck_output_block__multiple_output_blocks]
-   [Particle
    migration][Input_deck_species__particle_migration_between_species]

# Additional output block parameters {#additional_output_block_parameters}

The following parameters have also been added to the "output" block (see
[here][Input_deck_output__directives]):

-   dump_first
-   dump_last
-   force_first_to_be_restartable
-   ejected_particles
-   absorption
-   id
-   name
-   restartable

# Other additions to the input deck {#other_additions_to_the_input_deck}

-   [npart_per_cell][Input_deck_species]
-   [dir_{xy,yz,zx}_angle][Input_deck_dist_fn]
-   [particle_tstart][Input_deck_control]
-   [identify][Input_deck_species]

Finally, the input deck now has a method for writing continuation lines.
If the deck contains a "\" character then the rest of the line is
ignored and the next line becomes a continuation of the current one.

Changes between version 4.0 and 4.3 {#changes_between_version_4.0_and_4.3}
===================================

# Changes to the Makefile {#changes_to_the_makefile_1}

Some changes have been made to the Makefile. These are documented
[here][Compiler_Flags] .
The following compile-time define has been added to the Makefile:

-   MPI_DEBUG

The following compile-time define has been removed from the Makefile:

-   FIELD_DEBUG

# Additions to the input deck {#additions_to_the_input_deck}

The following parameters have been added to the
["control"][Input_deck_control] block of the input
deck:

-   nproc{x,y,z}
-   smooth_currents
-   field_ionisation
-   use_exact_restart
-   allow_cpu_reduce
-   check_stop_file_frequency
-   stop_at_walltime
-   stop_at_walltime_file
-   simplify_deck
-   print_constants
-   The "restart_snapshot" parameter now accepts filenames

The following parameters have been added to the
["output"][Input_deck_output_block] block of the input
deck:

-   disabled
-   time_start
-   time_stop
-   nstep_start
-   nstep_stop
-   dump_at_times
-   dump_at_nsteps
-   dump_cycle
-   dump_cycle_first_index
-   filesystem
-   file_prefix
-   rolling_restart
-   particle_energy
-   relativistic_mass
-   gamma
-   total_energy_sum
-   optical_depth
-   qed_energy
-   trident_optical_depth
-   The default value of "dump_first" is now "T"

The following parameter has been added to the
["collisions"][Input_deck_collisions] block of the
input deck:

-   collisional_ionisation

The following parameter has been added to the
["qed"][Input_deck_qed] block of the input deck:

-   use_radiation_reaction

The following parameter has been added to the
["species"][Input_deck_species] block of the input
deck:

-   immobile

The following parameters were changed in the
["laser"][Input_deck_laser] block of the input deck:

-   The "phase" parameter can now be time varying
-   The "profile" parameter can now be time varying

The following parameters have been added to the list of pre-defined
[constants][maths_parser__constants].

-   nproc_{x,y,z}
-   nsteps
-   t_end
-   cc

There has also been a new
["output_global"][Input_deck_output_global] block
added to the input deck.

# Changes in behaviour which are not due to changes in the input deck {#changes_in_behaviour_which_are_not_due_to_changes_in_the_input_deck}

-   The species "drift" property is now applied to particles whilst the
    moving window model is active. In previous versions of the code,
    this property was ignored once the moving window began.
-   Ionisation species now inherit their "dumpmask". See
    [here][Input_deck_species__ionisation] for details.
-   Default values for ignorable directions were added. This change
    allows submitting 3D or 2D input decks to a 1D version of and 3D
    input decks to a 2D version of . Any references to y/z will be set
    equal to zero unless overridden by a deck constant. Other y/z values
    also assume sensible defaults, eg. 1 grid cell, 1 metre thick, etc.
-   Automatic byte swapping is carried out by the SDF library. The
    library now checks the endianness of the SDF file and byte-swaps the
    data if required.
-   "qed" blocks may now be present even if the code was not compiled
    using the "-DPHOTONS" flag. The code will only halt if "use_qed=T"
    inside the "qed" block.
-   The code now checks for the Data directory in a file named
    "USE_DATA_DIRECTORY" before prompting at the command-line. This
    allows the code to be run without waiting for input at the
    command-line.
-   The field and particle grids are now automatically written to SDF
    output files if they are needed.
-   The Data directory may now contain a '`/`' character.

Changes between version 4.3 and 4.8 {#changes_between_version_4.3_and_4.8}
===================================

# Changes to the Makefile {#changes_to_the_makefile_2}

Some changes have been made to the Makefile. These are documented
[here][Compiler_Flags].
The following compile-time define has been added to the Makefile:

-   PER_SPECIES_WEIGHT
-   NO_TRACER_PARTICLES
-   NO_PARTICLE_PROBES
-   PARSER_CHECKING

The following compile-time define has been removed from the Makefile:

-   PER_PARTICLE_WEIGHT
-   TRACER_PARTICLES
-   PARTICLE_PROBES

# Additions to the input deck {#additions_to_the_input_deck_1}

The following parameters have been added to the
["control"][Input_deck_control] block of the input
deck:

-   allow_missing_restart
-   print_eta_string
-   n_zeros

The following parameters have been added to the
["output"][Input_deck_output_block] block of the input
deck):

-   weight (synonym for particle_weight)

The following parameters have been added to the
["output_global"][Input_deck_output_global] block of
the input deck:

-   dump_first_after_restart

The following parameters have been added to the
["subset"][Input_deck_subset] block of the input deck:

-   skip, skip_`x,y,z`

Changes between version 4.8 and 4.9 {#changes_between_version_4.8_and_4.9}
===================================

# New capabilities {#new_capabilities}

Version 4.9 adds significant new capabilities as follows:

-   delta-f version: particle distributions can be expressed as
    $f_0 + f_1$ where $f_0$ is a specified background plasma and all
    simulation particles are used to describe the $f_1$ component,
    documented in .
-   selectable field solvers: 3 new solvers have been added for fields,
    fully documented in .

# Changes to the Makefile {#changes_to_the_makefile_3}

Some changes have been made to the Makefile. These are documented in .
The following compile-time define has been added to the Makefile:

-   DELTAF_METHOD
-   DELTAF_DEBUG
-   USE_ISATTY

# Additions to the input deck {#additions_to_the_input_deck_2}

The following alterations were made to the input deck:

-   ioniz\* (with a "z") aliases have been added for ionis\* keywords.
-   y and z parameters can now appear in the input deck in EPOCH 1D and
    2D.

A new deck block has been added. The particles_from_file block allows
loading of custom particle data from raw binary data files. See for
details. This block accepts the following parameters:

-   species
-   {xyz}_data
-   w_data
-   {xyz}_data
-   id{4,8}_data
-   offset

The following parameters have been added to the "control" block of the
input deck (see ):

-   maxwell_solver
-   use_current_correction

The following parameters have been added to the "species" block of the
input deck (see ):

-   maxwell_solver
-   number_density_back
-   drift_{x,y,z}_back
-   temp_{x,y,z}_back
-   temp_{x,y,z}_back_ev
-   temp_back
-   temp_back_ev

The following parameters have been added to the "dist_fn' block of the
input deck (see ):

-   dir may now take the value mod_p
-   restrict_mod_p

# Changes not resulting from changes to the deck {#changes_not_resulting_from_changes_to_the_deck}

-   Lasers can be specified with time-varying frequency profile.
-   The existing subset blocks can now be applied to field and derived
    grid variables. If spatial restrictions are used, subsections will
    be output, along with a corresponding grid. Note that these are not
    compatible with the "skip" parameter to subset blocks.
-   The dist_fn block "range" keyword is now respected for spatial
    directions, allowing a spatial subset of the distribution function
    to be output directly.
-   Some corrections were applied to calculation of thermal boundary
    conditions for particles.
-   The load balancer may now be disabled by setting a 0 or negative
    threshold.

Changes between version 4.9 and 4.10 {#changes_between_version_4.9_and_4.10}
====================================

# New capabilities {#new_capabilities_1}

Version 4.10 adds the following new capabilities:

-   Time varying particle injectors. See [
    here][Input_deck_injector]
-   Per-species particle boundaries. You can now specify bc_x_min and
    bc_x_max to a species block. This overrides the global boundaries
    for that species. See
    [here][Input_deck_species__species_boundary_conditions]
-   Added "particles_per_cell" output diagnostic. See
    [here][Input_deck_output_block__derived_variables]

Changes between version 4.10 and 4.11 {#changes_between_version_4.10_and_4.11}
=====================================

# New capabilities {#new_capabilities_2}

Version 4.11 adds the following new capabilities:

-   Added time dependent moving window. No new input deck parameters
    have been added, but it is now possible to specify "window_v_x" to
    be a function that varies in time. See
    [here][Input_deck_window]
-   If "print_constants=T" in the control block (see
    [here][Input_deck_control]) deck constants are now
    output to a separate file named "const.status". This allows for
    easier post-processing.
-   Added COMPILER=auto option to automatically detect compiler. See
    [here][Compiling]

The following correction has been made:

-   Fractional numbers of particles-per-cell now function as expected
    when used in conjunction with the moving window.

Changes between version 4.11 and 4.12 {#changes_between_version_4.11_and_4.12}
=====================================

# New capabilities {#new_capabilities_3}

Version 4.12 adds the following new capabilities:

-   Added "average_weight" output diagnostic. See
    [here][Input_deck_output_block__derived_variables]
-   Removed the "PARTICLE_COUNT_UPDATE" Makefile flag and replaced it
    with a "use_particle_count_update" parameter in the control
    block. See [here][Input_deck_control__basics]
-   Added "use_flux_maxwellian" option to the "injector" block. See
    [here][Input_deck_injector__keys]
-   Added "lehe_{x,y,z}" flags to the "maxwell_solver" option in the
    control block. See
    [here][Input_deck_control__maxwell_solvers]
-   Added "use_accurate_n_zeros" control block parameter. See
    [here][Input_deck_control]
-   Added "custom" flag to the "maxwell_solver" option in the control
    block. See [here][Input_deck_control] and
    [here][Input_deck_control__stencil_block]
-   Added the "WORK_DONE_INTEGRATED" Makefile flag and corresponding
    dumpmask directives "work_{x,y,z}" and "work_{x,y,z}_total".
    These add a diagnostic for the work done on a particle by the
    electric field. See [here][Compiler_Flags] and
    [here][Input_deck_output_block__particle_variables].

Changes between version 4.12 and 4.14 {#changes_between_version_4.12_and_4.14}
=====================================

# New capabilities {#new_capabilities_4}

Version 4.14 adds the following new capabilities:

-   Added the "reset_walltime" flag to the control block. See
    [here][Input_deck_control]
-   Changed the default value of "print_eta_string" to "T" in the
    control block.
-   Added the ability to request an output dump at run time. See
    [here][Input_deck_control__requesting_output_dumps_at_run_time]
-   Added the "window_stop_time" parameter to the window block. See
    [here][Input_deck_window]
-   Added the "atan2" function to the maths parser. See
    [here][Maths_parser__functions]
-   Added "dlb_maximum_interval" parameter to the control block. See
    [here][Input_deck_control]
-   Added "dlb_force_interval" parameter to the control block. See
    [here][Input_deck_control]
-   Added "balance_first" parameter to the control block. See
    [here][Input_deck_control]
-   Added y and z versions of the "bc_x_min_after_move" and
    "bc_x_max_after_move" parameters to the window block. See
    [here][Input_deck_window]
-   Added a "dump_at_walltimes" parameter to the output block. See
    [here][Input_deck_output_block__multiple_output_blocks]
-   Added "walltime_start" and "walltime_stop" parameters to the
    output block and output_global block. See
    [here][Input_deck_output_block__directives] and
    [here][Input_deck_output_global]
-   Added "walltime_interval" parameter to the output block. See
    [here][Input_deck_output_block__multiple_output_blocks]
-   Added the Higuera-Cary particle push. This can be enabled using the
    "HC_PUSH" Makefile flag. See
    [here][Compiler_Flags].

Changes between version 4.14 and 4.15 {#changes_between_version_4.14_and_4.15}
=====================================

-   Added averaging of "poynt_flux" and "ekflux" variables.
-   The initial problem setup can now be load-balanced before any
    particles are loaded. This enables some heavily imbalanced setups to
    be run that were not previously possible.
    -   Added the "use_pre_balance" flag to the control block. See
        [here][Input_deck_control]
-   Allow the load balancer to adjust the processor topology
    -   Added the "use_optimal_layout" flag to the control block. See
        [here][Input_deck_control]
-   Added control block option "use_more_setup_memory" for
    controlling the way that species are setup. See
    [here][Input_deck_control]
-   Added strided multipass digital current filtering (See
    [here][Input_deck_control__strided_current_filtering]).
    This adds the following flags to the control block.
    -   smooth_iterations
    -   smooth_compensation
    -   smooth_strides
-   Added persistent subsets. See
    [here][Input_deck_subset]. This adds the following
    flags to the subset block
    -   persist_start_time
    -   persist_start_step
-   Added loading of relativistic particle species using the
    Maxwell-Jüttner distribution. See
    [here][Input_deck_species__maxwell_juttner_distributions].
    This adds the following flags to the species block
    -   use_maxwell_juttner
    -   fractional_tail_cutoff
-   Added loading of particle species using an arbitrary distribution
    function for sampling the momentum components. See
    [here][Input_deck_species__arbitrary_distribution_functions].
    This adds the following flags to the species block
    -   dist_fn
    -   dist_fn_p{x,y,z}_range
-   Added "temperature_{x,y,z}" derived output variables to the output
    block. See
    [here][Input_deck_output_block__derived_variables]

Changes between version 4.15 and 4.16 {#changes_between_version_4.15_and_4.16}
=====================================

-   Added "number_density" aliases for "density" in the species and
    injector blocks (see [here][Input_deck_species] and
    [here][Input_deck_injector]).

    :   These aliases include:

    -   number_density for density
    -   promote_number_density for promote_density
    -   demote_number_density for demote_density
    -   number_density_min for density_min
    -   number_density_max for density_max
    -   number_density_back for density_back

-   Replaced "USE_ISATTY" Makefile flag with "NO_USE_ISATTY". See
    [here][Compiler_Flags].
-   Added "NO_MPI3" Makefile flag. See
    [here][Compiler_Flags].
-   Added a "zero_current" alias for "tracer" in the species blocks.
    See [here][Input_deck_species]. The use of "tracer"
    has now been deprecated and will be removed in version 5.0. At that
    time, the compiler flag will also be renamed.


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
