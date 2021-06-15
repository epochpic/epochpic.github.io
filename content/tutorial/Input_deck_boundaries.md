---
draft: false
toc: true
type: docs

title: boundaries block
linktitle: boundaries block
weight: 100
menu:
  tutorial:
    parent: Input deck
    weight: 30
---

This block contains information about the boundary conditions for this
run. See [EPOCH input deck][Input_deck] for more
information on the input deck.

# Basics

The *`boundaries`* block sets the boundary conditions of each
boundary of the domain. Some types of boundaries allow EM wave sources
(lasers) to be attached to a boundary. Lasers are attached at the
initial conditions stage.

An example boundary block for EPOCH2D is as follows:

```perl
begin:boundaries
   bc_x_min = simple_laser
   bc_x_max_field = simple_outflow
   bc_x_max_particle = simple_outflow
   bc_y_min = periodic
   bc_y_max = periodic
end:boundaries
```

The `boundaries` block accepts the following parameters:\
- `bc_{x,y,z}_min` - The condition for the lower boundary
for both fields and particles. "xbc_left", "ybc_down" and "zbc_back"
are accepted as a synonyms.\
- `bc_{x,y,z}_min_{field,particle}` - The condition for
the lower boundary for {fields,particles}.
"xbc_left_{field,particle}", "ybc_down_{field,particle}" and
"zbc_back_{field,particle}" are accepted as a synonyms.\
- `bc_{x,y,z}_max` - The condition for the upper boundary
for both fields and particles. "xbc_right", "ybc_up" and "zbc_front"
are accepted as a synonyms.\
- `bc_{x,y,z}_max_{field,particle}` - The condition for
the upper boundary for {fields,particles}.
"xbc_right_{field,particle}", "ybc_up_{field,particle}" and
"zbc_front_{field,particle}" are accepted as a synonyms.\
- `cpml_thickness` - The thickness of the
[CPML](#cpml_boundary_conditions) boundary in terms of the
number of grid cells. The default value is 6.\
- `cpml_kappa_max` - A tunable
[CPML](#cpml_boundary_conditions) parameter.\
- `cpml_a_max` - A tunable
[CPML](#cpml_boundary_conditions) parameter.\
- `cpml_sigma_max` - A tunable
[CPML](#cpml_boundary_conditions) parameter.\
There are ten boundary types in EPOCH and each boundary of the domain
can have one and only one of these boundaries attached to it. These
boundary types are:\
- `periodic` - A simple periodic boundary condition. Fields
and/or particles reaching one edge of the domain are wrapped round to
the opposite boundary. If either boundary condition is set to periodic
then the boundary condition on the matching boundary at the other side
of the box is also assumed periodic.\
- `simple_laser` - A characteristic based boundary condition
to which one or more EM wave sources can be attached. EM waves impinging
on a *simple_laser* boundary are transmitted with as little reflection
as possible. Particles are fully transmitted. The field boundary
condition works by allowing outflowing characteristics to propagate
through the boundary while using the attached lasers to specify the
inflowing characteristics. The particles are simply removed from the
simulation when they reach the boundary. See [ laser
blocks][Input_deck_laser] for details.\
- `simple_outflow` - A simplified version of *simple_laser*
which has the same properties of transmitting incident waves and
particles, but which cannot have EM wave sources attached to it. These
boundaries are about 5% more computationally efficient than
*simple_laser boundaries* with no attached sources. This boundary
condition again allows outflowing characteristics to flow unchanged, but
this time the inflowing characteristics are set to zero. The particles
are again simply removed from the simulation when they reach the
boundary.\
- `reflect` - This applies reflecting boundary conditions to
particles. When specified for fields, all field components are clamped
to zero.\
- `conduct` - This applies perfectly conducting boundary
conditions to the field. When specified for particles, the particles are
reflected.\
- `open` - When applied to fields, EM waves outflowing
characteristics propagate through the boundary. Particles are
transmitted through the boundary and removed from the system.\
- `cpml_laser` - See [\#CPML boundary
conditions](#cpml_boundary_conditions).\
- `cpml_outflow` - See [\#CPML boundary
conditions](#cpml_boundary_conditions).\
- `thermal` - See [\#Thermal
boundaries](#thermal_boundaries).\
- `**NOTE: If simple_laser, simple_outflow, cpml_laser,
cpml_outflow or open are specified on one or more boundaries then the
code will no longer necessarily conserve mass.**`\
<span style="color:#ff0000">Note also that it is possible for the user
to specify contradictory, unphysical boundary conditions. It is the
users responsibility that these flags are set correctly.`

# CPML boundary conditions {#cpml_boundary_conditions}

There are now Convolutional Perfectly Matched Layer boundary conditions
in EPOCH. The implementation closely follows that outlined in the book
"Computational Electrodynamics: The Finite-Difference Time-Domain
Method" by Taflove and Hagness[^1]. See also Roden and Gedney[^2].

CPML boundaries are specified in the input deck by specifying either
"cpml_outflow" or "cpml_laser" in the boundaries block.
"cpml_outflow" specifies an absorbing boundary condition whereas
"cpml_laser" is used to attach a laser to an otherwise absorbing
boundary condition.

There are also four configurable parameters:\
- `cpml_thickness` - The thickness of the CPML boundary in
terms of the number of grid cells. The default value is 6.\
- `cpml_kappa_max`, `cpml_a_max`,
`cpml_sigma_max` - These are tunable parameters which
affect the behaviour of the absorbing media. The notation follows that
used in the two references quoted above. Note that the
"cpml_sigma_max" parameter is normalised by $\sigma_{\rm opt}$ which
is taken to be 3.2/dx (see Taflove and Hagness[^3] for details). These
are real valued parameters which take the following default values:
cpml_kappa_max=20, cpml_a_max=0.15, cpml_sigma_max=0.7\
An example usage is as follows:

    begin:boundaries
       cpml_thickness = 16
       cpml_kappa_max = 20
       cpml_a_max = 0.2
       cpml_sigma_max = 0.7
       bc_x_min = cpml_laser
       bc_x_max = cpml_outflow
       bc_y_min = cpml_outflow
       bc_y_max = cpml_outflow
    end:boundaries

# Thermal boundaries {#thermal_boundaries}

Thermal boundary conditions have been added to the "boundaries" block.
These simulate the existence of a "thermal bath" of particles in the
domain adjacent to the boundary. When a particle leaves the simulation
it is replace with an incoming particle sampled from a Maxwellian of a
temperature corresponding to that of the initial conditions. It is
requested using the keyword "thermal". For example:

    begin:boundaries
       bc_x_min = laser
       bc_x_max = thermal
    end:boundaries

# Next section {#next_section}

[The species block][Input_deck_species]

# References

<references />

[^1]: A. Taflove and S. C. Hagness, Computational Electrodynamics: The
    Finite-Difference Time-Domain Method. Artech House,
    2000.[1](https://www.researchgate.net/publication/202924435_Computational_Electrodynamics_The_Finite-Difference_Time-Domain_Method)

[^2]: J. Roden and S. Gedney, "Convolution pml (cpml): An efficient fdtd
    implementation of the cfs-pml for arbitrary media," Microw. Opt.
    Technol. Lett.,
    2000.[2](https://www.researchgate.net/publication/228078114_Convolutional_PML_CPML_an_efficient_FDTD_implementation_of_the_CFS-PML_for_arbitrary_media)

[^3]: 


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
