---
draft: false
toc: true
type: docs

title: boundaries block
linktitle: boundaries block
weight: 100
menu:
  documentation:
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

The `boundaries` block accepts the following parameters:
- `bc_{x,y,z}_min` - The condition for the lower boundary
for both fields and particles. "xbc_left", "ybc_down" and "zbc_back"
are accepted as a synonyms.
- `bc_{x,y,z}_min_{field,particle}` - The condition for
the lower boundary for {fields,particles}.
"xbc_left_{field,particle}", "ybc_down_{field,particle}" and
"zbc_back_{field,particle}" are accepted as a synonyms.
- `bc_{x,y,z}_max` - The condition for the upper boundary
for both fields and particles. "xbc_right", "ybc_up" and "zbc_front"
are accepted as a synonyms.
- `bc_{x,y,z}_max_{field,particle}` - The condition for
the upper boundary for {fields,particles}.
"xbc_right_{field,particle}", "ybc_up_{field,particle}" and
"zbc_front_{field,particle}" are accepted as a synonyms.
- `cpml_thickness` - The thickness of the
[CPML](#cpml_boundary_conditions) boundary in terms of the
number of grid cells. The default value is 6.
- `cpml_kappa_max` - A tunable
[CPML](#cpml_boundary_conditions) parameter.
- `cpml_a_max` - A tunable
[CPML](#cpml_boundary_conditions) parameter.
- `cpml_sigma_max` - A tunable
[CPML](#cpml_boundary_conditions) parameter.
There are ten boundary types in EPOCH and each boundary of the domain
can have one and only one of these boundaries attached to it. These
boundary types are:
- `periodic` - A simple periodic boundary condition. Fields
and/or particles reaching one edge of the domain are wrapped round to
the opposite boundary. If either boundary condition is set to periodic
then the boundary condition on the matching boundary at the other side
of the box is also assumed periodic.
- `simple_laser` - A characteristic based boundary condition
to which one or more EM wave sources can be attached. EM waves impinging
on a *simple_laser* boundary are transmitted with as little reflection
as possible. Particles are fully transmitted. The field boundary
condition works by allowing outflowing characteristics to propagate
through the boundary while using the attached lasers to specify the
inflowing characteristics. The particles are simply removed from the
simulation when they reach the boundary. See [ laser
blocks][Input_deck_laser] for details.
- `simple_outflow` - A simplified version of *simple_laser*
which has the same properties of transmitting incident waves and
particles, but which cannot have EM wave sources attached to it. These
boundaries are about 5% more computationally efficient than
*simple_laser boundaries* with no attached sources. This boundary
condition again allows outflowing characteristics to flow unchanged, but
this time the inflowing characteristics are set to zero. The particles
are again simply removed from the simulation when they reach the
boundary.
- `reflect` - This applies reflecting boundary conditions to
particles. When specified for fields, all field components are clamped
to zero.
- `conduct` - This applies perfectly conducting boundary
conditions to the field. When specified for particles, the particles are
reflected.
- `open` - When applied to fields, EM waves outflowing
characteristics propagate through the boundary. Particles are
transmitted through the boundary and removed from the system.
- `cpml_laser` - See [\#CPML boundary
conditions](#cpml_boundary_conditions).
- `cpml_outflow` - See [\#CPML boundary
conditions](#cpml_boundary_conditions).
- `thermal` - See [\#Thermal
boundaries](#thermal_boundaries).
- `**NOTE: If simple_laser, simple_outflow, cpml_laser,
cpml_outflow or open are specified on one or more boundaries then the
code will no longer necessarily conserve mass.**`
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

There are also four configurable parameters:
- `cpml_thickness` - The thickness of the CPML boundary in
terms of the number of grid cells. The default value is 6.
- `cpml_kappa_max`, `cpml_a_max`,
`cpml_sigma_max` - These are tunable parameters which
affect the behaviour of the absorbing media. The notation follows that
used in the two references quoted above. Note that the
"cpml_sigma_max" parameter is normalised by $\sigma_{\rm opt}$ which
is taken to be 3.2/dx (see Taflove and Hagness[^1] for details). These
are real valued parameters which take the following default values:
cpml_kappa_max=20, cpml_a_max=0.15, cpml_sigma_max=0.7
An example usage is as follows:
```
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
```

# Thermal boundaries {#thermal_boundaries}

Thermal boundary conditions have been added to the "boundaries" block.
These simulate the existence of a "thermal bath" of particles in the
domain adjacent to the boundary. When a particle leaves the simulation
it is replace with an incoming particle sampled from a Maxwellian of a
temperature corresponding to that of the initial conditions. It is
requested using the keyword "thermal". For example:

```
    begin:boundaries
       bc_x_min = laser
       bc_x_max = thermal
    end:boundaries
```

# References

<references />

[^1]: A. Taflove and S. C. Hagness, Computational Electrodynamics: The
    Finite-Difference Time-Domain Method. Artech House,
    2000.[1](https://www.researchgate.net/publication/202924435_Computational_Electrodynamics_The_Finite-Difference_Time-Domain_Method)

[^2]: J. Roden and S. Gedney, "Convolution pml (cpml): An efficient fdtd
    implementation of the cfs-pml for arbitrary media," Microw. Opt.
    Technol. Lett.,
    2000.[2](https://www.researchgate.net/publication/228078114_Convolutional_PML_CPML_an_efficient_FDTD_implementation_of_the_CFS-PML_for_arbitrary_media)



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
[Input_deck_laser]: /documentation/input_deck/input_deck_laser
