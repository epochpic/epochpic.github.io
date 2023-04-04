---
title: Boundary Conditions

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Core Structure
    weight: 90
---

Boundary conditions in EPOCH are split into three types
 
-  Simple field boundaries.
-  Laser and outflow boundaries.
-  Particle boundaries.
  

These boundaries can be combined in different ways to give different
effects. From the end user perspective there are 4 boundaries which can be
applied to each edge of the simulation domain. These are
 
-  Periodic
    - Particles periodic
    - Fields periodic
    - Lasers off
-  Other
    - Particles reflect
    - Fields clamped zero
    - Lasers off
-  Simple Laser
    - Particles transmissive
    - Fields clamped zero
    - Lasers applied at half timestep for $B$ field
-  Simple outflow
    - Particles transmissive
    - Fields clamped zero
    - No lasers applied at half timestep, but outflow conditions applied
  to $B$ field at half timestep
  
The boundary conditions are applied in too many places in the code to give a
full description of them, but the laser boundaries are only applied in
`src/fields.f90`. The boundaries requested by the user are converted
into the conditions on the fields and particles in the routine
`setup_particle_boundaries` in
`src/boundaries.F90`. For each of the six possible boundaries
(x_min, x_max, y_min, y_max, z_min, z_max) there is a variable which will
be named something like `bc_x_min_particle` or
`bc_y_max_field` which controls the boundary condition which will
be applied to either the field or the particles.

## Simple field boundaries
There are two subroutines which apply the standard boundary conditions:
`field_zero_gradient` and `field_clamp_zero`. The
type of boundary condition that the two apply is obvious from the name, but
the two functions have different calling conventions.

```perl
SUBROUTINE field_zero_gradient
  
REAL(num), DIMENSION(-2:,-2:,-2:), INTENT(INOUT) :: field
INTEGER, INTENT(IN) :: stagger_type, boundary
```   
  
`field_zero_gradient` is the routine which applies zero gradient
boundary conditions to a field variable passed in the parameter
`field`. The remaining two parameters are the stagger type and
the boundary number. These are named constants defined in
`src/shared_data.F90`, `c_stagger_*` for the
stagger type and `c_bd_*` for the boundary number.
The routine can be used as a global field boundary condition by
setting one of the field boundary conditions to c_bc_zero_gradient in
`setup_particle_boundaries`, but it is mostly used to give
boundary conditions for the autoloader.


```perl
SUBROUTINE field_clamp_zero
  
REAL(num), DIMENSION(-2:,-2:,-2:), INTENT(INOUT) :: field
INTEGER, INTENT(IN) :: stagger_type, boundary
```
 
`field_clamp_zero` is the routine which clamps the field given by
the `field` variable to zero on the boundary.
The remaining two parameters are the stagger type and
the boundary number. These are named constants defined in
`src/shared_data.F90`, `c_stagger_*` for the
stagger type and `c_bd_*` for the boundary number.
   
Additional boundary conditions should follow the same basic principle as these
routines. Note that all of the routines test for 
`\{x,y,z\}_\{min,max\}_boundary` to confirm that a given processor is at the
edge of the real domain, and so should have a real boundary condition applied
to it. This also explains why there are no explicit periodic boundary condition
routines, since by connecting the processors cyclically in a periodic direction
the domain boundary effectively becomes another internal processor boundary.

## Laser and outflow boundaries

The laser boundaries in EPOCH are based on a rewriting of Maxwell's
equations (combining Ampere-Maxwell and Faraday-Lenz) into a new form which 
expresses the fields explicitly in terms of waves
propagating in both directions along each co-ordinate axis with both S and P
polarisation states. In the $x$ direction,

$$
\partial_t(E_y \pm cB_z) \pm \partial_x(E_y \pm cB_z) = \pm \partial_yE_x c + \partial_zB_x c^2 -\frac{j_y}{\epsilon_0}
$$

$$
\partial_t(E_z \mp cB_y) \pm \partial_x(E_z \mp cB_y) = \pm \partial_zE_x c - \partial_yB_x c^2 -\frac{j_z}{\epsilon_0}
$$

It is then possible to rewrite these equations to provide a boundary condition
on $B_z$ and $B_y$ to give propagating EM waves at the boundary. For waves
travelling into the boundary, this gives a transmissive boundary, and if the
components for waves propagating out from the boundary are set to be non-zero
then it also introduces an EM wave propagating from the left boundary. 

This boundary condition is found in the file `laser.f90` which also
includes the routines for handling the `laser_block` objects which
represent how lasers are represented in EPOCH.

## Particle boundaries
Due to the time that is required to loop over all the particles the particle
boundary conditions in EPOCH combine the inter-processor boundary conditions
with the real boundary conditions. The boundary conditions for particles are in
the routine `particle_bcs` in the file `boundary.f90`  
Currently EPOCH includes only three particle boundary conditions
 
-  c_bc_open - Particles pass through the boundary and are destroyed. Total
  pseudoparticle number is not conserved in this mode.
-  c_bc_periodic - Particles which leave one side of the box reappear on
  the other side.
-  c_bc_reflect - Particles reflect off the boundary as if it was a hard
  boundary.
  

Although the routine looks rather messy, it is fairly easy to understand. The
sequence goes:
 
-  Loop over all species.
-  Create particle list objects for particles to be sent to and received from
  other processors.
-  Loop over all particles in the species.
-  If the particle has crossed a local boundary then it either
  has crossed the boundary of the problem domain and needs a boundary condition
  to be applied or it has just crossed a processor boundary and
  needs to be transferred to a neighbouring process. Set
  `{x,y,z}bd` which is used to identify which processor relative
  to the current processor the particle potentially needs to be moved to and
  then test for the domain boundary.
-  If the particle has crossed an open domain boundary then either add it to
  another list to be dumped to disk if the user has requested this, or
  otherwise just deallocate the particle to reclaim memory. Set
  `out_of_bounds` to indicate that the particle has left the
  system.
-  If the particle has crossed the domain boundary and that boundary has
  reflecting boundary conditions then reflect the particle.
-  If the particle has crossed the domain boundary and that boundary has
  periodic boundary conditions then move the particle to the opposite side
  of the domain.
-  End particle loop.
-  Remove all the particles which have left the current process.
-  Loop over all possible neighbouring processors for the current processor
  and exchange particle lists with that processor.
-  Add any received particles onto the particle list for the current
  species.
-  End species loop.
  

Note that, unlike for fields, there is explicit periodic boundary code. This is
because although the MPI routines place the particle on the correct processor
after the MPI routines, the particle's position variable still places it beyond
the other end of the domain. The MPI parallelism for exchanging particles is
hidden in the routines which deal with the particle list objects and are
described in the next section.


## MPI Boundaries
There are three routines which deal with MPI exchange for field variables in
EPOCH. Two are closely related and will be considered together. The third
deals with using MPI to sum variables at processor boundaries rather than
synchronise ghost cells.  

```perl
SUBROUTINE field_bc
  
REAL(num), DIMENSION(-2:,-2:,-2:), INTENT(INOUT) :: field
```   
  
`field_bc` exchanges the information in the ghost cells between
adjacent processors. Any field variable which is used in a calculation that
requires operations involving information from points other than the
current point should call this routine each time the variable is updated. This
will ensure that the ghost cells are populated from adjacent processors.
(i.e. if you only need to access field(ix,iy,iz) there is no need to update
ghost cells, whilst if you use field(ix-1,iy,iz) you do).
   
The `field_bc` routine just calls the
`do_field_mpi_with_lengths` routine which is a more general
routine that allows ghost cell information to be exchanged for fields with
an arbitrary number of cells, rather than fields which are
(-2:nx+3,-2:ny+3,-2:nz+3). This routine is used internally in the load
balancing routine when fields with both the old and new sizes must be handled
at the same time.

```perl
SUBROUTINE processor_summation_bcs
  
REAL(num), DIMENSION(-2:,-2:,-2:), INTENT(INOUT) :: field
INTEGER, INTENT(IN), OPTIONAL :: flip_direction
```
  
`processor_summation_bcs` is a routine which is used to deal with
variables, like $\vec{j}$ or number density that should be added at boundaries
to include contributions from particles on both sides of a processor boundary.
The routine is used for the current inside the particle pusher and inside most
of the routines for calculating derived variables. If you have a variable
which needs to add contributions from adjacent processors then you should
calculate the quantity on each processor, including contributions from the
particles to the ghost cells and then call this routine. When reflecting
boundary conditions are in operation, the current in the direction crossing
the boundary needs to be flipped over. This is decided upon using the
`flip_direction` parameter.

These routines can be used for most MPI calls required by all but the most
extreme modifications to EPOCH.