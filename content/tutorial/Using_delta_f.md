---
draft: false
toc: true
type: docs

title: Using delta f
linktitle: Using delta f
weight: 290
menu:
  tutorial:
    parent: Code details
    weight: 30
---

To help reduce the impact of numerical noise in certain simulations, the
delta-f method may be used in for specified particle species. The
delta-f method effectively subtracts a background distribution $f_0$
when calculating currents due to the motion of markers; when the
particle distribution function $f$ is close to $f_0$, this substantially
reduces the statistical noise in the currents. Only current deposition
is implemented differently, and the equations of motion of the markers
are not modified in this delta-f approach.

The component of the currents associated with the background $f_0$ may
be in principle be calculated analytically, but the delta-f
implementation in EPOCH assumes (but does not check) that the total
background current is zero.

In order to use the delta-f method EPOCH must be compiled with the
\#DELTAF_METHOD precompiler flag enabled. Standard input simulations
are not affected by switching on this flag, but the user may then choose
to treat certain species in the plasma using the delta-f method. To
enable delta-f calculation for a species the background distribution
function $f_0$ must be defined in the input block. $f_0$ is specified
using variables similar to those used to specify $f$. The current
implementation of delta-f allows only a spatially uniform drifting
Maxwellian $f_0$, with temperatures $T_x$, $T_y$ and $T_z$ allowed to
differ from each other. In 3D, for the case where the temperature in
each direction is equal, we have
$f_0 = n_0 (2 \pi T)^{-3/2} \exp\left(\frac{m (\mathbf{v} - \mathbf{v_d})^2}{2 k_B T } \right).$

The parameters number_density_back, temp(x,y,z)_back and
drift(x,y,z)_back in each species specification in the input deck set
$f_0$. number_density_back=0 is the default and is equivalent to not
using the delta-f method.

For example, the electron species component of an input deck solved
using delta-f might be written:

```perl
begin:species 
   name = electron 
   charge = -1.0 
   mass = 1.0 
   frac = 0.3 
   temp = 1e8 
   temp_back = 1e8 
   number_density = 1e20 
   number_density_back = 1e20 
end:species
```

Additional distribution function diagnostic options are supplied for the
Delta-f version. Standard diagnostics work as usual based on the total
distribution function $f$ but is is also possible to output the Delta-f
component of the distribution functions by adding output_deltaf = T in
dist_fn components of the input deck.

An example input deck is supplied in the 1D version as
twostream_deltaf.deck. This uses the delta-f method to solve the
weak-beam two stream instability. The bulk plasma species is solved
using the delta-f method, since this evolve very little, and mostly
supports the Lagnmuir waves that the weak beam interacts with. The
relative change to the beam species is large, and the standard PIC
method, rather than delta-f is used to model this species. A comparison
of the electric field diagnostics between standard and delta-f
simulations shows a substantial decrease in noise.

Further details of the method are provided [ in this
pdf](Media:talk_deltaf.pdf "wikilink").


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
