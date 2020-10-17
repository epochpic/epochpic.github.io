+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "Numerical heating"
[menu.tutorial]
  parent = "Content"
  weight = 500
+++

# PIC Noise {#pic_noise}

PIC codes effectively work by modelling packets of physical particles as
"pseudo particles" having a single velocity and a fixed spatial
extent. The number of real particles in these pseudoparticles is termed
the weight. These pseudoparticles then move as rigid objects with all of
the particles contained in them moving at that single velocity. So long
as the physics of interest in the problem are well resolved on the
spatial scale of these pseudoparticles then this model works well, but
it does inevitably average across much of the small scale physics that
takes place within the pseudoparticles. In particular the thermal motion
of the particles within the pseudoparticles are suppressed in favour of
a thermal distribution of the velocities of the entire pseudoparticles.
An ideal, non-drifting, non-relativistic Maxwellian distribution carries
zero current but even in reality since there are a finite number of
particles there will be deviations from this perfectly symmetric
distribution. Combining particles together into pseudoparticles
amplifies this effect by a factor of $w^\frac{1}{2}$ where $w$ is the
weight. So if your pseudoparticles represent 1,000,000 real particles
then the deviation from Maxwellian will be 1,000 times the level in the
real physical system. In real PIC simulations particle weights many
orders of magnitude higher than this are common. This artificial
deviation is termed PIC noise and means that you get electric and
magnetic fields purely due to deviations from the smooth Maxwellian that
are much larger than those in the real plasma.

Since these pseudoparticles are combinations of particles with the same
charge and mass, the charge to mass ratio of the pseudoparticle is
always exactly the same as the real particles inside them and so their
trajectory in an applied field is always the same regardless of their
weight. The weight comes into the calculations through the calculation
of the current which directly involves the number of particles that are
moving.

# Self Heating {#self_heating}

In real thermal plasmas at equilibrium energy is freely moving back and
forth between thermal and wave energy of the plasma and EM energy. Since
energy is conserved (ignoring loss processes) the total energy summed
over these forms is constant and so the plasma will sit quietly in
equilibrium. In an energy conserving PIC code the same process would
happen, although PIC noise would mean that the excursions of the
different forms of energy would be larger. PIC codes that do not
conserve energy however exhibit the same exchange of energy back and
forth but since conservation of energy is not enforced the plasma will
tend to heat over time. EPOCH is a momentum conserving code rather than
an energy conserving code.

The simplest class of PIC codes, those using nearest grid point
weighting (assuming that all of the current due to a particle is
attributed to the grid point nearest that particle) exhibit very rapid
self heating unless the Debye length is resolved. This is detailed in
Langdon 1970. <https://doi.org/10.1016/0021-9991(70)90024-0>. EPOCH does
not use nearest grid point weighting even at lowest order and has much
lower levels of self heating, especially when using spline interpolation
(see Arber et al.
<http://iopscience.iop.org/article/10.1088/0741-3335/57/11/113001/meta#ppcfaa013as5>).
It is essential that simulations are performed at a resolution, particle
number and interpolation method that ensures that self heating is much
lower than all processes of physical interest since self heating is
entirely numerical.

# Weight Based Stochastic Heating (WBSH) {#weight_based_stochastic_heating_wbsh}

This process is important where you have particles with different
weights in the same simulation (and tracer particles which effectively
have zero weight in this sense). WBSH is caused by a mixture of real and
numerical effects. In a real physical system a system of charged
particles in a stochastic electric field will heat up since the "random
kicks" to their velocity from the stochastic field causes them to
undergo a random walk in phase space, on average expanding outwards as
$t^{\frac{1}{2}}$. This process will continue until the current
contributed by the particles is large enough to modify the stochastic
field, at which point self consistent effects will cause the heating to
stop.

In a PIC simulation with all particles having equal weight, this process
is generally uninteresting. While the motion of the particles creates a
stochastic electric field since this field is created by the current of
the particles that current is also, by construction, sufficient to
induce the self-consistent limiting of the effect. In the default mode
of operation EPOCH does not ensure that all particles have equal weights
(if you compile EPOCH with the PER_SPECIES_WEIGHT compiler flag then
all particles in a single species do have equal weight but there are
fewer simulation particles in low density regions. Even then WBSH can
occur between species with different peak densities). The number of
simulation particles per cell is set first and then the particle weight
is modified to match the requested real particle number density, which
means that particles can have weights which differ by orders of
magnitude. This can then lead to WBSH effects if the particles of high
weight and particles of low weight share the same cell (or are within a
particle shape function length of each other).

Imagine two populations of physically identical particles modelled in a
PIC code: population 1 comes from a high density region and contains
high weight particles and population 2 comes from a low density region
and contains low weight particles. Assuming that both populations start
with the same temperature the magnitude of the stochastic field is
dominated by the behaviour of the population 1 since these particles are
of higher weight. This effect is entirely numerical since all of the
underlying physical particles in both populations are the same, but by
combining more particles together in population 1 the PIC noise is
higher than it would have been if only population 2 was present.
Population 1 particles then self consistently interact with that field
and do not heat. Population 2 particles react to that field, but do not
deposit enough current to induce their self-consistent limiting of the
heating rate and so will heat up. This heating of population 2 will
continue until they contribute broadly as much current as the particles
in population 1. This will roughly be when $wT = \mathrm{const}$ where
$w$ is the weight of the population and $T$ is the temperature of the
population. For particles with orders of magnitude difference in weight
this can be very significant.

### Tracer particles {#tracer_particles}

Tracer particles are passive test particles that move in the EM fields,
but do not deposit current. As a consequence they are always affected by
WBSH since they never contribute a thermal current at all and so cannot
reach equilibrium with the currents due to other species. They are
intended to be used in place of particles with low intrinsic weights
(such as particles far into the tails of distributions) which deposit
negligible current and would suffer from WBSH anyway. They should not be
used to attempt to mimic the behaviour of particles which would deposit
substantial current were they not tracer particles without substantial
care to ensure that WBSH is not affecting your solution.

------------------------------------------------------------------------

### When tracer particles fail {#when_tracer_particles_fail}

While it might feel like a natural use of tracer particles one of the
cases where they will fail is when you use them to track a subset of
particles for output purposes. So it would be tempting to use a deck
like

```perl
begin:species
   name = Electron
   charge = -1
   mass = 1
   npart_per_cell = 1000
   number_density = 1.e4
   dumpmask = never
end:species

begin:species
   name = Tracer
   charge = -1
   mass = 1
   npart_per_cell = 10
  number_density = density(Electron)
  tracer = T
end:species
```

to produce a "1%" species that you could dump and process rather than
having to handle the large number of particles in your electron species.
This will lead to uncontrolled heating of the tracer species due to
WBSH. You can achieve the intended effect easily by

------------------------------------------------------------------------

#### Using real particles {#using_real_particles}

Simply replace your tracer species with real particles and adjust the
densities of the two species to add to your requested overall density.
both species will have the same weight and experience the same
stochastic heating

```perl
begin:species
   name = Electron
   charge = -1
   mass = 1
   npart_per_cell = 1000
   number_density = 1.e4 * 0.99
   dumpmask = never
end:species

begin:species
   name = Tracer
   charge = -1
   mass = 1
   npart_per_cell = 10
  number_density = 1e4*0.01
end:species
```

------------------------------------------------------------------------

#### Using subsets {#using_subsets}

If you just want a representative 1% of the particles to be dumped at
each output then you can use
[subsets][Input_deck_subset]. The below example will
produce output of a random 1% sample on each output dump. The selected
1% is different on every output.

```perl
begin:species
   name = Electron
   charge = -1
   mass = 1
   npart_per_cell = 1000
   number_density = 1.e4
end:species

begin:subset
   name = background
   random_fraction = 0.01
   include_species:Electron
end:subset

begin:output
   particles = background
   px = background
   py = background
   pz = background
end:output
```

------------------------------------------------------------------------

#### Using persistent subsets {#using_persistent_subsets}

If you need to always track the same 1% of the particles then you can
use [ persistent subsets][Input_deck_subset]. They are
almost identical to subsets but you have to tell EPOCH at which time to
"lock" the particles to follow. The below example will lock in a
random 1% of the particles at the start of the simulation and always
output that 1% at all subsequent dumps. Persistent subsets do increase
EPOCH's memory footprint by about 10% of the number of particles that
you request it to store, in this case about 0.1% of the total memory
footprint.

```perl

begin:species
   name = Electron
   charge = -1
   mass = 1
   npart_per_cell = 1000
   number_density = 1.e4
end:species

begin:subset
   name = background
   persist_after = 0.0
   random_fraction = 0.01
   include_species:Electron
end:subset

begin:output
   particles = background
   px = background
   py = background
   pz = background
end:output
```

### When tracer particles work {#when_tracer_particles_work}

Tracer particles are intended for a situation where you believe that you
have a small number of particles that you believe will not contribute
meaningfully to the current and don't want to waste time calculating
the current for those particles. A classical example of this is when you
want to simulate particles well into the tail of a distribution function
that you wish to trace the behaviour of in the self consistent fields
from the bulk plasma.

```perl
begin:constant
  temperature = 1e6
  pte = sqrt(me * kb * temperature) #Non relativistic thermal momentum
end:constant

begin:species
   name = Electron
   charge = -1
   mass = 1
   npart_per_cell = 10
   number_density = 1.e4
   temp = temperature
end:species

begin:species
   name = Tail
   charge = -1
   mass = 1
   npart_per_cell = 10
   number_density = 1 #Don't actually care about absolute number density so normalise to 1
   drift = 6 * pte #Put these particles 6 thermal velocities out
   tracer = T
end:species
```

This allows you to have many particles in the tail of the distribution
while having comparatively few in the core (you would need 10s of
millions of particles per cell to get 10 particles per cell at 6 thermal
velocities in a single species). The advantage of using tracer particles
rather than real particles are

1.  You can arbitrarily normalise the density without it having any
    effect on the simulation. This is helpful if it would be onerous to
    calculate the density in the wings of your distribution function.
2.  The particle pusher is about 10% faster when not calculating
    currents so if many of your particles are in your tail species EPOCH
    can be usefully faster

While the particles in your Tail species will experience WBSH, real
particles in the tail would need very low weights to get the correct
density while retailing the requested particle per cell numbers. Since
the current due to the tail particles is very small the effect of
completely ignoring it is also very small so the benefits in performance
and simplicity of setup are useful.

### When WBSH is important {#when_wbsh_is_important}

WBSH is generally not a major concern in PIC codes, but it can be
important for certain classes of problem, especially when particles with
very different weights are present at the same spatial location for long
periods before they interact with the physics of interest in your
simulation.

1.  Putting a very low density positron population inside a higher
    density plasma as an initial condition unless you also adjust the
    number of particles per cell so that the weight is the same for both
    positrons and plasma electrons. This might well require many
    particles per cell for your plasma electrons if the number of
    positrons is very low.
2.  Putting in a "patch" of space with a large number of particles per
    cell to try and resolve distribution functions locally
3.  Putting tracer particles in with real particles to monitor a section
    of space
4.  At very large jumps in density (not smooth ramps, but substantial
    discontinuities)

There is no guarantee that a particular simulation having these
properties will show substantial changes to the output as a result of
WBSH, but care must be taken to ensure that it does not. As always,
convergence testing your answer will help to show up statistical
problems in your simulation. In particular you should increase the
number of psuedo-particles in the higher weight species to reduce the
weight discrepancy.


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
