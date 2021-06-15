---
draft: false
toc: true
type: docs

title: injector block
linktitle: injector block
weight: 230
menu:
  tutorial:
    parent: Input deck
    weight: 150
---

The *injector* block specifies a particle source to be introduced
through a simulation boundary. Each injector block specifies a source of
a single species of particle defined by a density, centre of mass drift
momentum, temperature and number of simulation particles per cell. The
current version of the injectors is incompatible with the
PER_SPECIES_WEIGHT compiler flag, and attempting to use an injector
with a version of EPOCH compiled with this flag will fail.

# Concepts

EPOCH can inject particles through any of the simulation boundaries.
This plasma is either a drifting Maxwellian corresponding to a
collisionally thermalized beam or a "flux Maxwellian" corresponding to
a Maxwellian source accelerated by an electrostatic accelerator. It can
have any temporal or transverse spatial profile of density, temperature
or drift that you wish to specify.

EPOCH does not automatically make any assumption about the plasma that
you wish to inject and does not correct for currents injected into the
domain. Current due to an injected beam will be smoothly created as the
particles enter the domain. If you wish to inject a neutral beam, you
will have to use multiple injectors to inject electrons and ions so as
to produce a neutral beam. Great care must be taken when introducing
relativistic beams since the current due to a highly relativistic beam
will not be the current due to the centre of mass velocity since EPOCH
does not use the Maxwell-Jüttner distribution for loading particles.

# Boundary conditions {#boundary_conditions}

The injectors only work properly with certain boundary conditions. For
most purposes the "open" boundary condition is the only one that makes
sense with injectors since particles are flowing freely through the
boundary. Remember that in any version of EPOCH that supports injectors
you can also use per species boundary conditions to allow you to have
different boundary conditions for injected and bulk particles.

# Moving window {#moving_window}

Injectors and moving windows can be tricky to work with, so the default
behaviour of EPOCH is to stop all injectors when the window starts to
move. If you wish to override this behaviour then simply explicitly set
t_end in the injector block to a value after the window starts to move.
Setting

```perl
 t_end = t_end 
```

will cause the injectors to continue running until the end of the
simulation even with the moving window. You must take great care when
specifying injectors for a moving window because you will likely get
gaps or bunches in particles injected through the x boundary and there
will probably be some shearing of particles introduced through y and z
boundaries. It is in general recommended that you specify a velocity
profile for the moving window that stops at times when particles are to
be injected and then starts again once the injection is complete.

# Keys

-   boundary - specifies which boundary to attach the particle source
    too. Same specification as the
    [laser block][Input_deck_laser], so
    permitted values are x_min, x_max, y_min, y_max, z_min and
    z_max
-   species - specifies which species should be injected through the
    boundary. Just specify the name of the species required.
-   t_start - Time at which to start the injector
-   t_end - Time at which to end the injector
-   npart_per_cell - target pseudo-particle density for the injector.
    Average number of particles injected will be this value or slightly
    higher if very few particles are specified
-   number_density - Number density of the particle source in $m^{-3}$.
    Can be space varying along the boundary to which the injector is
    attached and time varying
-   number_density_min - Minimum number density in $m^{-3}$ below
    which pseudo particles are not loaded. Use if the density has a
    profile to avoid injecting low weight particles in low density
    regions
-   temp_x - Temperature in x direction (K)
-   temp_y - Temperature in y direction (K)
-   temp_z - Temperature in z direction (K)
-   temp - Temperature in all directions (K)
-   drift_x - Momentum drift in x direction in $kgm/s$
-   drift_y - Momentum drift in x direction in $kgm/s$
-   drift_z - Momentum drift in x direction in $kgm/s$

\- Logical flag to determine whether to use an accelerated flux
Maxwellian rather than a drifting Maxwellian. This calculates the flux
due to passing a Maxwellian source into an electrostatic accelerator
instead of a drifting Maxwellian. If your particle source is a lab
accelerator then you may want to set this to true.

# Example Deck {#example_deck}

```perl
begin:injector
   boundary = x_min
   species = Electron
   number_density = dens
   temp_x = temp
   drift_x = drift_p
   npart_per_cell = 32
end:injector
```

# Warnings

Currently injectors are a beta feature of EPOCH. We believe them to work
correctly, but unusual results must be considered suspect. If you get
unexpected results, please contact the EPOCH development team.


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
