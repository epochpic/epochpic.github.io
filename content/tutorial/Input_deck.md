---
draft: false
toc: true
type: docs

title: The EPOCH input deck
linktitle: Input deck
weight: 80
menu:
  tutorial:
    name: Input deck
    weight: 20
---

Most of the control of EPOCH is through a text file called `input.deck`.
The input deck file must be in the output directory which is passed to
the code at runtime and contains all the basic information which is
needed to set up the code, including the size and subdivision of the
domain, the boundary conditions, the species of particles to simulate
and the output settings for the code. For most users this will be
capable of specifying all the initial conditions and output options they
need. More complicated initial conditions will be handled in later
sections.

The input deck is a structured file which is split into separate blocks,
with each block containing several "parameter" = "value" pairs. The
pairs can be present in any order, and not all possible pairs must be
present in any given input deck. If a required pair is missing the code
will exit with an error message. The blocks themselves can also appear
in any order. The input deck is case sensitive, so true is always "T",
false is always "F" and the names of the parameters are always lower
case. Parameter values are evaluated using a maths parser which is
described in [EPOCH maths parser][maths_parser].\
If the deck contains a "`\`" character then the rest of the line is
ignored and the next line becomes a continuation of the current one.
Also, the comment character is "`#`"; if the "`#`" character is used
anywhere on a line then the remainder of that line is ignored.\
There are three *`input deck directive`* commands, which are:

-   begin:*`block`* - Begin the block named
    *`block`*.
-   end:*`block`* - Ends the block named
    *`block`*.
-   import:*`filename`* - Includes another file (called
    *`filename`*) into the input deck at the point where the
    directive is encountered. The input deck parser reads the included
    file exactly as if the contents of the included file were pasted
    directly at the position of the import directive.

Each block must be surrounded by valid *`begin:`* and
*`end:`* directives or the input deck will fail. There are
currently fourteen valid blocks hard coded into the input deck reader,
but it is possible for end users to extend the input deck. The fourteen
built in blocks are:

-   control - Contains information about the general code setup. See
    [here][Input_deck_control]
-   boundaries - Contains information about the boundary conditions for
    this run. See [here][Input_deck_boundaries]
-   species - Contains information about the species of particles which
    are used in the code. Also details of how these are initialised. See
    [here][Input_deck_species]
-   laser - Contains information about laser boundary sources. See
    [here][Input_deck_laser].
-   fields - Contains information about the EM fields specified at the
    start of the simulation. See
    [here][Input_deck_fields].
-   particles_from_file - Contains information about files used to
    load particle data. See
    [here][Input_deck_particle_file].
-   window - Contains information about the moving window if the code is
    used in that fashion. See
    [here][Input_deck_window].
-   output - Contains information about when and how to dump output
    files. See [here][Input_deck_output_block].
-   output_global - Contains parameters which should be applied to all
    output blocks. See
    [here][Input_deck_output_global].
-   dist_fn - Contains information about distribution functions that
    should be calculated for output. See
    [here][Input_deck_dist_fn].
-   probe - Contains information about particle probes used for output.
    See [here][Input_deck_probe].
-   collisions - Contains information about particle collisions. See
    [here][Input_deck_collisions].
-   qed - Contains information about QED pair production. See
    [here][Input_deck_qed].
-   subset - Contains configuration for filters which can be used to
    modify the data to be output. See
    [here][Input_deck_subset].
-   constant - Contains information about user defined constants and
    expressions. These are designed to simplify the initial condition
    setup. See [here][Input_deck_constant].

# Next section {#next_section}

[The control block][Input_deck_control]


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
