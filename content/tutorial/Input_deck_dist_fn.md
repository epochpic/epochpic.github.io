---
draft: false
toc: true
type: docs

title: dist_fn block
linktitle: dist_fn block
weight: 170
menu:
  tutorial:
    parent: Input deck
    weight: 100
---

This block contains information about distribution functions that should
be calculated for output. See [EPOCH input
deck][Input_deck] for more information on the input
deck.

Sometimes it is useful to reconstruct part of the full phase space for
one or more particle species. This functionality is provided through a
*dist_fn* block. The distribution function is integrated over all
dimensions which are not axes of the distribution function.

Calculating distribution functions requires some degree of integration
of data leading to various possible ways of normalising the resulting
distribution function. In EPOCH, distribution functions are normalised
so that the value at every point of the distribution function is the
number of particles within that cell of the distribution function,
ignoring all phase space directions which are not considered as an axis
of the distribution function. Summing the distribution function should
give the total number of real particles (as opposed to computational
pseudoparticles) in the simulation.

An example *dist_fn* block is given below:

```perl
begin:dist_fn
   name = x_px
   ndims = 2
   dumpmask = always

   direction1 = dir_x
   direction2 = dir_px

   # Range is ignored for spatial coordinates
   range1 = (1, 1)
   range2 = (-50.0e-20, 50.0e-20)

   # Resolution is ignored for spatial coordinates
   resolution1 = 1
   resolution2 = 5000

   restrict_py = (-3.0e-20, 3.0e-20)

   include_species:Electron
   include_species:Carbon
end:dist_fn
```

-   `name` - The name of the distribution function when it is
    output. This name is appended with the name of each species for
    which the data is output and so, for example, when applied to a
    species named carbon the output is called *x_px_Carbon*. The
    Cartesian grid which describes the axes of the distribution function
    would then be called *grid_x_px_Carbon*.\
-   `ndims` - The number of dimensions in this phase space
    reconstruction. Due to difficulties in visualising data in more than
    three dimensions, this is restricted to being 1, 2 or 3.\
-   `dumpmask` - Determines which output dumps will include
    this distribution function. The dumpmask has the same semantics as
    those used by variables in the "output" block, described
    [here][Input_deck_output_block]. The dumpmask from
    "distribution_functions" in the output block is applied first and
    then this one is applied afterwards. For example, if the dist_fn
    block contains "dumpmask = full" and the output block contains
    "distribution_functions = always" then this distribution function
    will be only be dumped at full dumps. The default dumpmask is
    "always".\
-   `direction**n**` - This is the phase space to sample
    along axis . This can be any one of: dir_x, dir_y, dir_z,
    dir_px, dir_py, dir_pz, dir_en, dir_gamma_m1, dir_xy_angle,
    dir_yz_angle, dir_zx_angle with spatial codes only being
    available in dimensionalities of the code which have that direction.
    Therefore dir_z does not exist in EPOCH1D or EPOCH2D and dir_y
    does not exist in EPOCH1D.

The flags "dir_xy_angle", "dir_yz_angle" and "dir_zx_angle"
calculate the distribution of particle momentum directions in the X-Y,
Y-Z and Z-X planes.\
- `range**n**` - The range between which this axis should
run. This is in the form of (minimum, maximum). Any particle which
exceeds the range is ignored. For momentum directions this parameter is
specified in $kg\ ms^{-1}$. If the range of a momentum direction is set
so that the maximum and the minimum are equal then the code will
automatically set the range to exactly span the range of particle
momenta at the point of writing the dump.\
- `resolution**n**` - The number of gridpoints in a given
direction. This is ignored for spatial dimensions where the resolution
is always the same as the resolution of the underlying simulation.\
- `include_species` - Specifies a species which should be
included in the output. This is useful since it is rare that momentum
limits are appropriate for both electrons and ions, so usually for a
given dist_fn block only electrons or ions are considered. It is
possible to have two dist_fn blocks with the same name but different
ranges and different include_species settings produce the effect of a
single diagnostic for all species in the output file.\
- `output_deltaf` - If set to "T", the particle weights used
in calculating the distribution function is adjusted by subtracting the
Delta-f distribution function for the particle species. The default
value is "F".\
- `restrict_{x,y,z,px,py,pz}` - Restrictions are specified
in the same way as ranges, but have a subtly different behaviour. Ranges
specify the range of a visible axis on the resulting distribution
function, whereas restrictions allow you to specify minimum and maximum
values for each spatial and momentum direction and use only particles
which fall within this range when calculating the distribution function.
Restrictions can be specified even for properties which are not being
used as axes. It is possible to set a restriction that is more
restrictive than the range applied. This is not trapped as an error and
such parts of the distribution function are guaranteed to be empty. The
available spatial restrictions depend on the dimensionality of the code.
Therefore, attempting to set restrict_z in EPOCH1D will produce a
warning.\
At present, the code to calculate the distribution functions has one
limitation: it ignores particle shape functions when calculating
properties on the spatial axis, meaning that the result is less smooth
than normal properties from the code.


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
