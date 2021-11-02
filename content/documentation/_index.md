---
title: Overview

date: 2018-09-09T00:00:00
# lastmod: 2018-09-09T00:00:00

draft: false
toc: true
type: docs

menu:
  documentation:
    name: Overview
    weight: 1

---

# How to use these pages {#how_to_use_these_pages}

If you are new to EPOCH, start with the [FAQ][FAQ] and
the [ introductory information.](#basic_usage) Then read [the
basic examples][Basic_examples]. There's quite a lot to
learn in order to get started, so you should plan to read through all of
this section. You will also need to refer to [the input deck
pages][Input_deck]. Next, look at the code and have a
play with some test problems. After that re-read the
[FAQ][FAQ]. This should be enough for testing simple
problems. See [below](#visualising_epoch_output) for more
information on visualising the output files.

For specific information, see the
[index](#epoch_manual_index) below or use the search
function. Alternately, start with the [FAQ][FAQ] and
read through the pages in order by following the "Next section" links.

# Basic usage {#basic_usage}

-   [The EPOCH FAQ list][FAQ]
-   [Getting the code][FAQ__how_do_i_obtain_the_code]
-   [The structure of the EPOCH codes][Structure]
-   [Library requirements for the EPOCH codes][Libraries]
-   [Compiling EPOCH][Compiling]
-   [Compiler flags and preprocessor defines][Compiler_Flags]
-   [Running EPOCH and basic control of EPOCH][Running]

# The input deck {#the_input_deck}

-   [The EPOCH input deck][Input_deck]
    * [The control block][Input_deck_control]
    * [The boundaries block][Input_deck_boundaries]
    * [The species block][Input_deck_species]
    * [The laser block][Input_deck_laser]
    * [The fields block][Input_deck_fields]
    * [The window block][Input_deck_window]
    * [The output block][Input_deck_output_block]
    * [The output_global block][Input_deck_output_global]
    * [The dist_fn block][Input_deck_dist_fn]
    * [The probe block][Input_deck_probe]
    * [The collisions block][Input_deck_collisions]
    * [The qed block][Input_deck_qed]
    * [The subset block][Input_deck_subset]
    * [The constant block][Input_deck_constant]
    * [The injector block][Input_deck_injector]

# Code details {#code_details}

-   [The EPOCH maths parser][Maths_parser]
-   [EPOCH use in practice][Using_EPOCH_in_practice]
-   [Using EPOCH in delta_f form][Using_delta_f]
-   [Basic examples of using EPOCH][Basic_examples]
-   [Changes from previous versions of EPOCH][Previous_versions]

# Visualising EPOCH output {#visualising_epoch_output}

- [Visualising SDF files using IDL or GNU Data
  Language (GDL)][Visualising_SDF_files_with_IDL_or_GDL]
- [Visualising SDF files using LLNL VisIt][Visualising_SDF_files_with_LLNL_VisIt]
- [Visualising SDF files using Python][Python]

# Examples with EPOCH {#examples_with_epoch}

Example decks and output are available here:

-   [Basic examples of using EPOCH][Basic_examples]
    from the manual

A link to submit your own examples will be provided soon

### The EPOCH workshop {#the_epoch_workshop}

The examples from the EPOCH workshop are in two parts: [(part
1)][Workshop_examples] [(part
2)][Workshop_examples_continued]

# Helpful information {#helpful_information}

[Acknowledging EPOCH][Acknowledging_EPOCH]
The EPOCH [Developer
Manual](https://github.com/Warwick-Plasma/EPOCH_manuals/releases) is
quite out of date at this point, so it contains some information which
is no longer correct. However, the fundamental algorithms have not
changed so it still contains plenty of useful and relevant information.



<!-- ########################  Cross references  ######################## -->


[Acknowledging_EPOCH]: /documentation/basic_usage/acknowledging_epoch
[Basic_examples]: /documentation/examples/basic_examples
[Compiler_Flags]: /documentation/basic_usage/compiler_flags
[Compiling]: /documentation/basic_usage/compiling
[FAQ]: /documentation/basic_usage/faq
[FAQ__how_do_i_obtain_the_code]: /documentation/basic_usage/faq#how_do_i_obtain_the_code
[Input_deck]: /documentation/input_deck/input_deck
[Input_deck_boundaries]: /documentation/input_deck/input_deck_boundaries
[Input_deck_collisions]: /documentation/input_deck/input_deck_collisions
[Input_deck_constant]: /documentation/input_deck/input_deck_constant
[Input_deck_control]: /documentation/input_deck/input_deck_control
[Input_deck_dist_fn]: /documentation/input_deck/input_deck_dist_fn
[Input_deck_fields]: /documentation/input_deck/input_deck_fields
[Input_deck_injector]: /documentation/input_deck/input_deck_injector
[Input_deck_laser]: /documentation/input_deck/input_deck_laser
[Input_deck_output_block]: /documentation/input_deck/input_deck_output_block
[Input_deck_output_global]: /documentation/input_deck/input_deck_output_global
[Input_deck_probe]: /documentation/input_deck/input_deck_probe
[Input_deck_qed]: /documentation/input_deck/input_deck_qed
[Input_deck_species]: /documentation/input_deck/input_deck_species
[Input_deck_subset]: /documentation/input_deck/input_deck_subset
[Input_deck_window]: /documentation/input_deck/input_deck_window
[Libraries]: /documentation/basic_usage/libraries
[Maths_parser]: /documentation/code_details/maths_parser
[Previous_versions]: /documentation/code_details/previous_versions
[Python]: /documentation/visualising_output/python
[Running]: /documentation/basic_usage/running
[Structure]: /documentation/basic_usage/structure
[Using_EPOCH_in_practice]: /documentation/code_details/using_epoch_in_practice
[Using_delta_f]: /documentation/code_details/using_delta_f
[Visualising_SDF_files_with_IDL_or_GDL]: /documentation/visualising_output/visualising_sdf_files_with_idl_or_gdl
[Visualising_SDF_files_with_LLNL_VisIt]: /documentation/visualising_output/visualising_sdf_files_with_llnl_visit
[Workshop_examples]: /documentation/examples/workshop_examples
[Workshop_examples_continued]: /documentation/examples/workshop_examples_continued
