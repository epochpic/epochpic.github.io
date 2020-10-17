+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "Input deck pipeline"
[menu.tutorial]
  parent = "Content"
  weight = 380
+++

The new, more flexible output system in the Epoch 5.0 series is around
the concept of a 'pipeline'. These are designed to allow as much
in-code data reduction as possible. A pipeline for outputting particle
data lets you go through a series of selection steps so that you output
only the particles you're actually interested in. As in the older
system, you can select by species or momentum, make a random selection,
or track only those particles that have left the system. Unlike the
older system, you can also produce much more complex selection operators
such as tracking all those particles that started the simulation in a
particular grid cell. Additionally, distributions functions have been
made much more flexible, adding more axes and allowing you to directly
output e.g. energy as a function of angle.

Pipelines start with a source of particles (usually the name of a
species or probe), apply a series of operators to select some subset of
these, and then produce a series of outputs, either raw particle data or
derived variables such as number density on a grid. Grid based or
distribution function outputs can be further processed to restrict in
space, average over time, etc.

Everything that was possible under the old system remains possible, and
decks should continue to work as before. More complex decks may benefit
from rewriting in the new system.

# Simple outputs in old and new systems {#simple_outputs_in_old_and_new_systems}

Before going through the details of the new pipelines, we show a few
examples compared to the old system to illustrate the general structure
and benefits.

A typical output using the old system is something like this, taken from
the two-stream deck (tests/twostream/input.deck)

```perl
begin:output
   ... # Omitting dumptime specs etc

   # Properties at particle positions
   particles = always
   px = always

   # Properties on grid
   grid = always
   ex = always
   number_density = always + species
end:output
```

We can reproduce this with pipelines

```perl
begin:pipeline
   name = particles # Pipeline name, for use in output block where timings are set
   species : Left, Right # Do sum of Left and Right
   species : Left # Also output Left only
   species : Right # Also output Right only
   particle_variable : dir_px  # Output px
   derived_variable : number_density # Built in number density output
   field_variable : io_ex # Output ex also
end:pipeline
```

Doing more complex selection in the old system used the concept of
subsets, such as

```perl
begin:subset
   name = mysub
   random_fraction = 0.1
   include_species : Left
   include_species : Right
end:subset

begin:output
   ... # Omitting dumptime specs etc

   # Properties on grid
   grid = always
   number_density = always + species + mysub
end:output
```

which now becomes

```perl
begin:pipeline
   name = particles
   species : Left
   species : Right
   particle_operator : random_fraction(0.1)
   derived_variable : number_density # Built in number density output
end:pipeline
```

# A more complex output {#a_more_complex_output}

The real power of the pipelines is to shorten and simplify relatively
complex output specs using multiple selectors, which would previously
have required multiple subsets. For instance, if we have multiple
ionisation states, but the high-numbered ones contain very few
particles, so we want to sum their number densities, we would have used
something like

```perl
begin:subset
   name = base
   random_fraction = 0.1
   include_species : Carbon
end:subset
begin:subset
   name = one
   random_fraction = 0.1
   include_species : Carbon1
end:subset
begin:subset
   name = two
   random_fraction = 0.1
   include_species : Carbon2
end:subset
begin:subset
   name = high
   random_fraction = 0.1
   include_species : Carbon3
   include_species : Carbon4
   include_species : Carbon5
end:subset

begin:output
   number_density = always + base + one + two + high + sum
end:output
```

where with pipelines we can specify the species directly, as

```perl
begin:pipeline
   name = particles
   species: Carbon
   species : Carbon1
   species : Carbon2
   species : Carbon3, Carbon4, Carbon5
   particle_operator : random_fraction(0.1)
   derived_variable : number_density
end:pipeline
```

and it's now far easier to add more selection criteria, such as a
momentum lower bound etc.

# Block details {#block_details}

A typical new output pipeline might look something like

```perl
begin:pipeline
   name = pipe1
   species : electron
   species : electron, proton
   ejected_species : electron
   probe : probe1
   particle_operator : restriction_op
   particle_operator : random_fraction(0.01)
   particle_variable : dir_px
   particle_variable : dir_vz
   derived_variable : number_density # Built in number density output
   derived_variable : x_px_df # User specified distribution function specified by name
   field_variable : io_ex
end:pipeline
```

This will create 4 sets of output, for electrons, the sum of electrons
and protons, ejected electrons and for probe1. An unspecified
restriction is applied, and a random fraction of 1% of the particles are
used. Particle x momentum and z velocity are included in the output. It
also outputs the number density for each species, a custom distribution
function (see below) and the ex field.

The keys are described below. Note that most keys are optional, but:

-   Name is a required key
-   If you don't specify any \[particle,derived,field\]_variable keys
    nothing will be output
-   Operators are applied in the order specified. This should never
    change the output, but may have minor effects on memory and time
    taken

### Name

All pipelines must be named using the name key. This is to allow them to
be referred to in output blocks

### Particle sources {#particle_sources}

Particle sources are specified using one of the following keys

-   species - Named species or list of species to output. For each new
    species key a new output pipeline for just that species is created.
    If a comma separated list of species is specified then the output is
    calculated summed over that species
-   ejected_species - Named species to output. This outputs particle
    data for all particles that have left the domain going through any
    of the boundaries. Particles that remain in the domain after
    interacting with the boundary (such as through thermal or periodic
    boundaries) are not considered ejected. Particles that leave due to
    the moving window are not considered ejected.
-   probe - Named particle probe. All species specified in the probe are
    summed over for output

For all outputs except for field_variable outputs require at lest one
particle source. No output will be generated if no particle sources are
specified

### Particle operators {#particle_operators}

Particle operators are specified with the particle_operator keyword and
may be either the name of a [named operator
block][Input_deck_operator] or a constructor function
for a particle operator (such as random_fraction)

### Particle variable output {#particle_variable_output}

You can specify any [calculable particle
property][Calculable_particle_properties] known to
EPOCH as a particle output variable. Simply specify the
particle_variable key, followed by the named calculable particle
property

### Derived variables {#derived_variables}

A derived variable is any output that is ultimately derived from a
particle source. This is either a user specified [advanced distribution
function][Input_deck_adf], specified by name, or a
built in named derived variable. All of the built in variables are
defined on the main simulation grid. The build in derived variables are

-   number_density - Output particle number density
-   temperature - Particle temperature
-   ekbar - Particle mean kinetic energy
-   mass_density - Mass density
-   charge_density - Charge density
-   particles_per_cell - Particles per cell diagnostic
-   px_bar - Mean x momentum in each cell
-   py_bar - Mean y momentum in each cell
-   pz_bar - Mean z momentum in each cell
-   modp_bar - Mean total momentum in each cell
-   vx_bar - Mean x velocity in each cell
-   vy_bar - Mean y velocity in each cell
-   vz_bar - Mean z velocity in each cell
-   modv_bar - Mean total velocity in each cell

### Field variables {#field_variables}

A field variable is any output on a grid that is not derived from a
particle source and are specified by name. The possible names are

-   poynting_x - Poynting flux in X
-   poynting_y - Poynting flux in Y
-   poynting_z - Poynting flux in Z
-   io_ex - Ex field
-   io_ey - Ey field
-   io_ez - Ez field
-   io_bx - Bx field
-   io_by - By field
-   io_bz - Bz field
-   io_jx - Jx field
-   io_jy - Jy field
-   io_jy - Jz field


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
