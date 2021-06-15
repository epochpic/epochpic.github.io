---
draft: false
toc: true
type: docs

title: subset block
linktitle: subset block
weight: 210
menu:
  tutorial:
    parent: Input deck
    weight: 130
---

This block contains configuration for filters which can be used to
modify the data to be output. See [EPOCH input
deck][Input_deck] for more information on the input
deck.

It is possible to restrict the number of particles written to file
according to various criteria. For example, you can now output the
momentum of all particles which have a gamma lower than 1.8 or the
positions of a randomly chosen subset of a given species.

A new input deck block named "subset" is defined which accepts the
following parameters:\
- `name` - The name given to this subset. This is used to
identify the subset in the output block and is also used when labelling
the data in the SDF files.\
- `include_species` - Add the given particle species to the
set of particles that this subset applies to. By default, no particle
species are included.\
- `dumpmask` - The dumpmask to use when considering this
subset in an output block. This takes the same form as the output block
dumpmask. The default value is "always".\
- `random_fraction` - Select a random percentage of the
particle species. This is a real value between zero and one. If 0 is
specified, no particles are selected. If 1 is specified, all the
particles are selected. If 0.2 is specified, 20% of the particles are
selected.\
- `{px,py,pz,weight,charge,mass,gamma}_min` - Select only
the particles with momentum, weight, charge, mass or gamma which is
greater than the given value.\
- `{px,py,pz,weight,charge,mass,gamma}_max` - Select only
the particles with momentum, weight, charge, mass or gamma which is less
than the given value.\
- `{x,y,z}_min` - Select only the particles whose position
lies above the given value.\
- `{x,y,z}_max` - Select only the particles whose position
lies below the given value.\
- `id_min,max` - Select only the particles whose "id" is
greater than or less than the given values. The "id" field is explained
below.\
- `skip,skip_{x,y,z}` - Integer parameter for subsampling
output. If set to a positive integer then all grid-based variables using
the subset restriction will be reduced when being written to file. This
is achieved by skipping by the specified number of cells in each of the
specified directions. The "skip" parameter provides a quick method for
setting the same number of cells to skip in all directions. This
currently only applies to grid-based variables and is ignored for data
averages. The default value is "0".

Once a subset has been defined, the subset name can then be used in
place of (or in addition to) the dumpmask in an "output" block (see also
[here][Input_deck_output_block__dumpmask]). For example:

```perl
begin:subset
   name = background
   random_fraction = 0.1
   include_species:electron
   include_species:proton
end:subset

begin:subset
   name = high_gamma
   gamma_min = 1.3
   include_species:electron
end:subset

begin:output
   particles = background + high_gamma + always
   px = background + high_gamma
   py = background
   pz = always
end:output
```

In this example, three "px" blocks will be written:
"Particles/background/electron/Px", "Particles/background/proton/Px" and
"Particles/high_gamma/electron/Px". The "background" blocks will
contain 10% of the each species, randomly selected. The "high_gamma"
block will contain all the electrons with a gamma greater than 1.3.

There will also be "Particles/background/electron/Py" and
"Particles/background/proton/Py" block containing y-momentum for the
same 10% random subset of particles. Finally, the
"Particles/All/electron/Pz" and "Particles/All/proton/Pz" will contain
the z-momentum for all particles.

The final selection criteria given in the list above is "id_min" and
"id_max". As of EPOCH version 4.0, the code can now assign a unique ID
field to every particle in the simulation. This can be useful for
tracking specific particles as they move through a simulation. As this
field adds extra memory requirements to the particles, it is disabled by
default and must be compiled in using the **`-DPARTICLE_ID`** compiler
flag.

Particle IDs can be written to file using the "id" variable name in the
["output"][Input_deck_output_block] block. Eg.

```perl
begin:output
   particles = always
   id = always
end:output
```

### Subsets of fields {#subsets_of_fields}

Subset blocks can be applied to per-species variables such as current
and temperature. Only particles within the given momentum ranges and of
the selected species are included in the calculations. In addition,
subset blocks can now be applied to field or grid variables. This allows
you to output spatial sections using the `{x,y,z}_max,min`
restrictions. The output data will be trimmed to the selected ranges and
a corresponding restricted grid included in the output. Note that
specifying an empty range will lead to output of the entire domain. For
example, the following snippet will output an ex_c_centre variable
restricted to the centre 1/3rd of the domain with a corresponding grid
grid_centre:

```perl
begin:subset
   name = centre
   x_min = x_min + (x_max - x_min) / 3.0
   x_max = x_min + 2.0 * (x_max - x_min) / 3.0
end:subset

begin:output 
   ...
   ex = always + centre
end:output
```

### Persistent subsets {#persistent_subsets}

Persistent subsets are subsets that capture a set of particles once,
given a specified set of parameters, and then track those particles
permanently. Persistent subsets use the same blocks as normal subsets
and take the same parameters as normal subsets (except the skip
parameters which only apply to fields). Subsets are marked as persistent
by setting either

-   `persist_start_time` - Time at which to record the list
    of particles to be tracked. Throughout the rest of the simulation
    this recorded list will be used whenever requesting output for this
    subset. "persist_after_time" is accepted as an alias. Set to 0
    to record from the start of the simulation.\
-   `persist_start_step` - Similar to persist_start_time
    except this specifies a simulation step number to use instead of
    time. "persist_after_step" is accepted as an alias.\

If the input deck is edited on restart to add a new persistent subset
then it must be added **after** existing persistent subsets or problems
may occur on restart.


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
