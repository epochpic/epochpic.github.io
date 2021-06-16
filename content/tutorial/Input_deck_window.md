---
draft: false
toc: true
type: docs

title: window block
linktitle: window block
weight: 140
menu:
  tutorial:
    parent: Input deck
    weight: 70
---

This block contains information about the moving window if the code is
used in that fashion. See [EPOCH input
deck][Input_deck] for more information on the input
deck.

EPOCH can include an optional block which causes the simulation domain
to operate as a moving window. At present, it is only possible to have
the window moving at a speed parallel to the x direction, although the
window does not have to start moving at t = 0. When the window moves,
the code removes particles from the left hand edge of the domain and
introduces new particles at the right hand edge. The new particles are
placed by re-evaluating the species density, temperature and drift using
the new time and spatial coordinates. The block looks like:

```perl
begin:window
   move_window = T
   window_v_x = 3.0e8
   window_start_time = 7.0e-13
   bc_x_min_after_move = simple_outflow
   bc_x_max_after_move = simple_outflow
end:window
```

-   `move_window` - Logical flag determining whether or not
    to move the window. If the window block is absent then this is the
    same as setting move_window to "F".
-   `window_v_x` - The speed in m/s of the window.
-   `window_start_time` - The time in seconds at which the
    window should start moving.

\- The time in seconds at which the window should stop moving.
- `bc_x_min_after_move` - The boundary condition which
should apply to the left boundary after the window has started moving.
This is to allow the swapping of a laser boundary to a simple outflow
boundary. Boundary codes are the same as when just specifying normal
boundaries. If a boundary value isn't specified then it is assumed that
the boundary isn't changed when the window starts moving.
"xbc_left_after_move" is accepted as a synonym.
- `bc_x_max_after_move` - The boundary condition which
should apply to the right boundary after the window has started moving.
"xbc_right_after_move" is accepted as a synonym. - "y" and "z"
versions of the previous two parameters. **ybc_down_after_move**,
**ybc_up_after_move**, **zbc_back_after_move** and
**zbc_front_after_move** are accepted as synonyms.

# Compatibility

Because of how the moving window must work, there are some compatibility
issues with certain features. In particular:

-   lasers attached to an X boundary which remain in place after the
    window moves, or attached to Y or Z boundaries:
    -   The laser will behave as though it is attached to the window
        itself: for Y or Z boundaries with spatial variations this may
        not give the expected result
    -   For X boundaries, the moving emitter will result in a form of
        numerical Doppler shifting. In addition to this the boundary
        used to drive the field will shift discontinuously, yielding
        noisy and erratic changes in the electromagnetic field.
-   Injectors attached to an X boundary will not work. Those on a Y or Z
    boundary may appear to work, but the rates will be incorrect.
-   CPML boundary conditions:
    -   in X these cannot work as they rely on time-history which is
        simply missing.
    -   On Y or Z boundaries they will approximately work, but the
        history will be truncated and so they will generally require
        more tuning. We can't help with this in general.
-   Load of particles from file is not supported since it can't be made
    to work in general.


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
