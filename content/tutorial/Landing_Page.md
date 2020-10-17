+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "Landing Page"
[menu.tutorial]
  parent = "Content"
  weight = 450
+++

# How to use these pages {#how_to_use_these_pages}

If you are new to EPOCH, start with the [FAQ][FAQ] and
the [ introductory information.](#basic_usage) Then read [the
basic examples][Basic_examples]. There's quite a lot to
learn in order to get started, so you should plan to read through all of
this section. You will also need to refer to [the input deck
pages][input_deck]. Next, look at the code and have a
play with some test problems. After that re-read the
[FAQ][FAQ]. This should be enough for testing simple
problems. See [below](#visualising_epoch_output) for more
information on visualising the output files.

For specific information, see the
[index](#epoch_manual_index) below or use the search
function. Alternately, start with the [FAQ][FAQ] and
read through the pages in order by following the "Next section" links.

# Basic usage {#basic_usage}

-   [The EPOCH FAQ list][FAQ]\
-   [Getting the code][FAQ__how_do_i_obtain_the_code]\
-   [The structure of the EPOCH codes][Structure]\
-   [Library requirements for the EPOCH
    codes][Libraries]\
-   [ Compiling EPOCH][Compiling]\
-   [Compiler flags and preprocessor
    defines][Compiler_Flags]\
-   [Running EPOCH and basic control of
    EPOCH][Running]\

# The input deck {#the_input_deck}

-   [The EPOCH input deck][input_deck]\
    \*[The control block][input_deck_control]\
    \*[The boundaries block][input_deck_boundaries]\
    \*[The species block][input_deck_species]\
    \*[The laser block][input_deck_laser]\
    \*[The fields block][input_deck_fields]\
    \*[The window block][input_deck_window]\
    \*[The output block][input_deck_output_block]\
    \*[The output_global
    block][input_deck_output_global]\
    \*[The dist_fn block][input_deck_dist_fn]\
    \*[The probe block][input_deck_probe]\
    \*[The collisions block][input_deck_collisions]\
    \*[The qed block][input_deck_qed]\
    \*[The subset block][input_deck_subset]\
    \*[The constant block][input_deck_constant]\

-   [The injector block][Input_deck_injector]\

# Code details {#code_details}

-   [The EPOCH maths parser][maths_parser]\
-   [EPOCH use in practice][Using_EPOCH_in_practice]\
-   [Using EPOCH in delta_f form][Using_delta_f]\
-   [Basic examples of using EPOCH][Basic_examples]\
-   [Changes from previous versions of
    EPOCH][Previous_versions]\

# Visualising EPOCH output {#visualising_epoch_output}

[Visualising SDF files using Harris Geospatial Solutions IDL or GNU Data
Language (GDL)][Visualising_SDF_files_with_IDL_or_GDL]\
[Visualising SDF files using LLNL
VisIt][Visualising_SDF_files_with_LLNL_VisIt]\
[Visualising SDF files using Python][Python]\

# Examples with EPOCH {#examples_with_epoch}

Example decks and output are available in a few places:

-   [Basic examples of using EPOCH][Basic_examples]
    from the manual
-   [User submitted examples][Landing] (tbc)

A link to submit your own examples will be provided soon

### The EPOCH workshop {#the_epoch_workshop}

The examples from the EPOCH workshop are in two parts: [(part
1)][Workshop_examples] [(part
2)][Workshop_examples_continued]

# Helpful information {#helpful_information}

[Links and references][Links]\
[Acknowledging EPOCH][Acknowledging_EPOCH]\
The EPOCH [Developer
Manual](https://cfsa-pmw.warwick.ac.uk/EPOCH/epoch/wikis/Downloads) is
quite out of date at this point, so it contains some information which
is no longer correct. However, the fundamental algorithms have not
changed so it still contains plenty of useful and relevant information.\


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
