---
draft: false
toc: true
type: docs

title: collisions block
linktitle: collisions block
weight: 190
menu:
  tutorial:
    parent: Input deck
    weight: 120
---

This block contains information about particle collisions. See [EPOCH
input deck][Input_deck] for more information on the
input deck.

EPOCH has a particle collision routine with scattering algorithms based
on the model presented by Sentoku and Kemp[^1] or the model presented by
Pérez et al [^2], which in turn was based on the work of Nanbu and
Yonemura[^3]. This adds a new output block named "collisions" which
accepts the following four parameters.\
- `use_collisions` - This is a logical flag which determines
whether or not to call the collision routine. If omitted, the default is
"T" if any of the frequency factors are non-zero (see below) and "F"
otherwise.\
- This logical flag determines whether the scattering angle of
Pérez/Nanbu will be used. The default is "T". If "F", the
Sentoku-Kemp algorithm will be used.

-   `coulomb_log` - This may either be set to a real value,
    specifying the Coulomb logarithm to use when scattering the
    particles or to the special value "auto". If "auto" is used then the
    routine will calculate a value based on the local temperature and
    density of the particle species being scattered, along with the two
    particle charges. If omitted, the default value is "auto".\
-   `collide` - This sets up a symmetric square matrix of
    size $nspecies\,\times\,nspecies$ containing the collision frequency
    factors to use between particle species. The element (s1,s2) gives
    the frequency factor used when colliding species s1 with species s2.
    If the factor is less than zero, no collisions are performed. If it
    is equal to one, collisions are performed normally. For any value
    between zero and one, the collisions are performed using a frequency
    multiplied by the given factor.\

If "collide" has a value of "all" then all elements of the matrix are
set to one. If it has a value of "none" then all elements are set to
minus one.\
If the syntax "species1 species2 <value>" is used, then the
(species1,species2) element of the matrix is set to the factor
"<value>". This may either be a real number, or the special value "on"
or "off". The "collide" parameter may be used multiple times.\
The default value is "all" (ie. all elements of the matrix are set to
one).\
- `collisional_ionisation` - If this logical flag is set to
"T" then the collisional ionisation model is enabled. This process is
independent of *field_ionisation* (see
[here][Input_deck_species__ionisation]). However, in
order to set up *collisional_ionisation* you must also specify
ionisation energies and electrons in a *species* block (see
[here][Input_deck_species__ionisation]). The default
value is "F".\
For example:

```perl
begin:collisions
   use_collisions = T
   use_nanbu = T
   coulomb_log = auto
   collide = all
   collide = spec1 spec2 off
   collide = spec2 spec3 0.1
end:collisions
```

With this block, collisions are turned on, the Nanbu-Pérez scattering
algorithm is used and the Coulomb logarithm is automatically calculated.
All values of the frequency array are set to one except (spec1,spec2) is
set to minus one (and also (spec2,spec1)) and (spec2,spec3) is set to
0.1

# References

<references />

[^1]: Y. Sentoku and A. J. Kemp, "Numerical methods for particle
    simulations at extreme densities and temperatures: Weighted
    particles, relativistic collisions and reduced currents," J. Comput.
    Phys., 2008.
    [1](http://www.sciencedirect.com/science/article/pii/S0021999108001988)

[^2]: F. Pérez et al, "Improved modeling of relativistic collisions and
    collisional ionization in particle-in-cell codes ," Physics of
    Plasmas, 2012. [2](https://doi.org/10.1063/1.4742167)

[^3]: K. Nanbu and S. Yonemura, "Weighted Particles in Coulomb Collision
    Simulations Based on the Theory of a Cumulative Scattering Angle,"
    J. Comput. Phys., 1998. [3](https://doi.org/10.1006/jcph.1998.6049)


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
