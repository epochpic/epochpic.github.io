+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "Non-thermal initial conditions"
[menu.tutorial]
  parent = "Content"
  weight = 490
+++

# THIS SECTION SUPERSEDED {#this_section_superseded}

As of EPOCH 4.15 arbitrary distribution functions can be specified in
the deck (see
[here][Input_deck_species__arbitrary_distribution_functions]).
The approach in this section is no longer needed.

# Background

Before version 4.15 EPOCH's input deck is only set up to allow you to
specify Maxwellian particle distributions. You do this by putting the
relevant keys into the `species` block for each species. You can specify
an isotropic Maxwellian by specifying the `temp` key in Kelvin. You can
specify an anisotropic Maxwellian by specifying some or all of the
`temp_x`, `temp_y` and `temp_z` which sets the distribution function in
each of these directions You can extend this to include distributions
consisting of multiple Maxwellians by combining multiple species each
with an individual temperature.

Sometimes though you want to use a truly arbitrary distribution
function, and at present to do that you have to modify the EPOCH code
itself. There is a specific routine `manual_load` in the file
`src/user_interaction/ic_module.90` which is the intended place for
users to control EPOCH from within the EPOCH source code. This routine
is not as simple to use as the input deck, as it requires some knowledge
of the internal design of EPOCH, and some familiarity with modern
Fortran, but for most uses you will not have to worry about things like
the parallelism or the detailed internal EPOCH data structures. It
should all just work. When the routine is first called the particles are
in the state that is specified in the input deck, with the associated
density and temperature. You only need to modify the things that are
different to the state specified in the input deck.

# EPOCH internals {#epoch_internals}

There are three elements of EPOCH's internals that you do have to
interact with, the particle_species objects, particle_list objects and
the particle objects themselves. All of them are implemented as Fortran
`TYPE` and are prototyped in the `src/shared_data.F90` file. For this
problem, the only bits that you will need to use are

```fortran
  TYPE particle_species
    CHARACTER(string_length) :: name
    TYPE(particle_list) :: attached_list
  END TYPE particle_species
```

The `name` parameter is self explanatory and is the name of the species
as specified in the input deck. The `attached_list` parameter is a
particle_list object which holds the particles belonging to this
species.

You will need only one element of the `particle_list` object, called
`head`

```fortran
  TYPE particle_list
    TYPE(particle), POINTER :: head
  END TYPE particle_list
```

`head` is a pointer to the first particle in the list. EPOCH stores the
particles using a [linked list](https://en.wikipedia.org/wiki/Linked_list)
format, so that each particle stores a pointer to the next particle.
`head` Gives you access to the start of the list of particles.

You might need to interact with two elements of the particle type

```fortran
  TYPE particle
    REAL(num), DIMENSION(3) :: part_p
    REAL(num) :: part_pos ! In EPOCH 1D
    REAL(num), DIMENSION(2) :: part_pos ! In EPOCH 2D
    REAL(num), DIMENSION(3) :: part_pos ! In EPOCH 3D
  END TYPE particle
```

The `part_pos` parameter is the position of the particle. It is a single
REAL type value in EPOCH1D, a 2 element array in EPOCH2D and a 3 element
array in EPOCH3D. You might need to read these values if you want to
have spatially varying distribution functions. It is possible to change
the position of a particle in this routine, but it is not recommended
and most uses of this are beyond the scope of this article. The `part_p`
parameter is the particle moment in SI units. It is always a 3 element
array, with the elements ordered as `[px,py,pz]`. This is the array that
you must change to specify a non-Maxwellian distribution. It is up to
you how to specify the momentum of the particles, but the most general
algorithm is
[rejection sampling](https://en.wikipedia.org/wiki/Rejection_sampling)
which is detailed in an example below.

# Examples

As an example, code will be shown that modifies EPOCH so that the
`two_stream.deck` example deck produces one species with a power law
distribution in $p_x$ and $p_y$ with a spectral index of -2

$f(p_x,p_y) = A |p|^{-2}$.

There are three related problems in setting up this distribution :
looping over all of the particle species until the one that you want is
found, looping over all of the particles in that species, and finally
sampling the distribution that you want. Finding the particle species is
easy. There is an array called `species_list` which runs from 1 to
`n_species` which returns a `particle_species` type. One can then simply
test for the species wanted by comparing

```fortran
TRIM(species_list(ispecies)%name)
```

to the name of the required species as specified in the input deck. The
comparison is case sensitive. To loop over the particles in the species
the current particle pointer `current` is pointed to the head of the
particle list attached to the species `species_list(ispecies)`. The main
loop over the particles looks slightly strange because of the linked
list. The pointer `current` points to the current particle in the linked
list. It is set to be the start of the list in the line

```fortran
current=> species_list(ispecies)%attached_list%head
```

After each loop the pointer is reassigned to point to the `next` element
of the current particle which advances through the linked list. The loop
terminates when the current pointer points to an unassigned (NULL)
value. The minimum loop to just iterate over the particles is

```fortran
MODULE ic_module

  USE shared_data
  USE helper

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: manual_load

CONTAINS

  !This manual_load file does nothing other than loop over
  !the particles in a species specified by name
  SUBROUTINE manual_load

    INTEGER :: ispecies
    TYPE(particle), POINTER :: current

    DO ispecies = 1, n_species

      IF (TRIM(species_list(ispecies)%name) == 'Right') THEN
        !Have found the species that I want, so grab the start of the
        !linked list
        current=> species_list(ispecies)%attached_list%head

        !Now loop until I reach the end of the linked list
        DO WHILE(ASSOCIATED(current))
          !Advanced the linked list
          current=>current%next
        END DO

      END IF

    END DO

  END SUBROUTINE manual_load

END MODULE ic_module
```

To actually select particles one must also implement a standard
[rejection sampling](https://en.wikipedia.org/wiki/Rejection_sampling)
algorithm,
This is shown below.

```fortran
MODULE ic_module

  USE shared_data
  USE helper

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: manual_load

CONTAINS

  !This manual_load file is an example of using an accept/reject algorithm
  !to specify an arbitrary initial condition. In this case a power law
  !distribution with index -alpha is specified. It is designed to work with the
  !two_stream.deck example input deck
  SUBROUTINE manual_load

    INTEGER :: ispecies
    TYPE(particle), POINTER :: current
    REAL(num) :: random_number, probability
    REAL(num) :: momentum_x, momentum_y, momentum

    !Specify particle's momentum in mc units for simplicity
    REAL(num), PARAMETER :: p_min = 0.002_num
    REAL(num), PARAMETER :: p_max = 0.02_num
    REAL(num), PARAMETER :: alpha = 2.0_num

    DO ispecies = 1, n_species

      IF (TRIM(species_list(ispecies)%name) == 'Right') THEN
        !Have found the species that I want, so grab the start of the
        !linked list
        current=> species_list(ispecies)%attached_list%head

        !Now loop until I reach the end of the linked list
        DO WHILE(ASSOCIATED(current))
          !Erase whatever momentum the particle starts with. Can keep bits
          !if you want too. The particle will have momentum sampled from
          !whatever temperature is specified in the deck at this point.
          !part_p(1) = px
          !part_p(2) = py
          !part_p(3) = pz
          current%part_p = 0.0_num
          !Loop around until a momentum is accepted for this particle
          DO
            !Generate random x and y momenta between p_min and p_max
            momentum_x = random() * (p_max-p_min) + p_min
            momentum_y = random() * (p_max-p_min) + p_min
            momentum = SQRT(momentum_x**2+momentum_y**2)
            !From that value, have to generate the probability that a particle
            !with that momentum should be accepted. 
            !This is just the particle distribution function scaled to have
            !a maximum of 1 (or lower).
            !In general you will have to work this out yourself
            !Total momentum runs from SQRT(2.0_num)*p_min to SQRT(2.0_num)*p_max
            probability = momentum**(-alpha) / (SQRT(2.0_num)*p_min) **(-alpha)
            !Once you know your probability you just generate a random number
            !between 0 and 1 and if the generated number is less than the
            !probability then accept the particle and exit this loop.
            random_number=random()
            IF (random_number <= probability) EXIT
          END DO
          current%part_p(1) = momentum_x * m0 * c
          current%part_p(2) = momentum_y * m0 * c
          !Advanced the linked list
          current=>current%next
        END DO

      END IF

    END DO

  END SUBROUTINE manual_load

END MODULE ic_module
```

# Efficient sampling {#efficient_sampling}

The power law example in the previous section has a maximum acceptance
probability of 100% at $|p| = SQRT(2.0)*p_{min}$, a minimum of 1% at
$|p| = SQRT(2.0)*p_{max}$ and an average of 2.5%. This means that in
general, we will have to consider 40 times the number of particles we
need to assign. This is inefficient and, for large numbers of particles,
potentially slow. The maximum probability cannot be above 100%, but a
smaller $p_{max}$ increases the average acceptance and speeds up the
loading. **Watch out for large power-law indices or distributions with
rapid fall-off, where the average acceptance can be very low, and the
overall loading time large.**


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
