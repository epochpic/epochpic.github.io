---
draft: false
toc: true
type: docs

linktitle: Input deck probe
menu:
  tutorial:
    parent: Content
    weight: 390
---

This block contains information about particle probes used for output.
See [EPOCH input deck][Input_deck] for more information
on the input deck.

Sometimes it is useful to consider all the properties of particle which
pass through a point/line/plane (depending on dimension) in the
simulation. To allow this, it is possible to specify one or more
*`Particle Probe`* blocks in the input deck. These record
copies of all particles which cross a point/line/plane in a given
direction which meet minimum and maximum kinetic energy criteria and
output the particle properties into the normal output files. Particle
probes record the positions, momenta and weight of all particles passing
through the plane. To use particle probes, the code must not have been
compiled with the **`-DNO_PARTICLE_PROBES`** compiler option. This is a
fairly heavyweight diagnostic since each particle position must be
tested from within the particle push. The code will run faster if it is
not compiled in.\
The probe is specified in terms of a point in the plane and the normal
vector to the plane which is to be monitored. Particles are only
recorded if they cross the plane in the direction given by the normal
vector. If you want to record particles travelling in both directions
then use two particle probes, one with an opposite signed normal vector
to the other.

```perl
begin:probe
   name = electron_back_probe

   point = (50.0e-6, -50.0e-6)
   normal = (1.0, 0.0)

   ek_min = 0.0
   ek_max = -1.0
   include_species : s1

   dumpmask = always
end:probe
```

-   `name` - The name that the probe should have in output
    dumps. Output variables are then named this as a prefix. For
    example, the block shown above will result in the name
    **electron_back_probe_px** for the x momentum. The particle
    positions would just be called **electron_back_probe**.\
-   `point` - An arbitrary point in the plane of the probe.\
-   `normal` - A vector normal to the plane of the probe, in
    the direction of crossings you wish to monitor.\
-   `include_species` - The species to which this probe
    should be applied. To probe several species, use several probe
    blocks in the input deck. "probe_species" is accepted as a
    synonym.\
-   `ek_min` - The minimum kinetic energy of particles to
    store information about. Set to 0 for no minimum kinetic energy.\
-   `ek_max` - The maximum kinetic energy of particles to
    store information about. Set to -1 for no maximum kinetic energy.\
-   `dumpmask` - The dump code for this particle probe. This
    is the same as that for the main output controls in **input.deck**.
    Note that the code has to store copies of particles which pass
    through the probe until a dump occurs. This means that the code's
    memory requirements can increase drastically if this code only dumps
    probe information infrequently. If this is set to **never** then the
    code effectively never uses the probe.\

# Next section {#next_section}

[The collisions block][Input_deck_collisions]


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
