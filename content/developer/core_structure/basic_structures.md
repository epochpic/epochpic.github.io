---
title: Basic Structures

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Core Structure
    weight: 40
---

This page details a few basic background components of EPOCH. Here we introduce 
the global shared data modules and its physical constants, variables used to 
define the cell grid, and the functions used to load macro-particles onto the
grid.

## Physical constants
In order to ensure that different parts of the code run at the same precision
common physical constants are defined in `constants.F90` and any
new physical constants required by extensions to the code should be placed in
the same location. The constants available in the code are
 
-  pi - Ratio of a circle's circumference to its diameter.
-  q0 - Charge on electron.
-  m0 - Rest mass of electron.
-  c - Speed of light in vacuum.
-  kb - Boltzmann's constant.
-  mu0 - Permeability of free space.
-  epsilon0 - Permittivity of free space.
-  h_planck - Planck's constant.
-  ev - The value of an electron volt.
-  h_bar - Planck's constant divided by $2\pi$ .
-  a0 - The Bohr radius.
-  hartree - Double the Rydberg energy.
-  alpha - Fine structure constant.
-  atomic_time - Time in atomic units (h_bar / hartree).
-  atomic_electric_field - Electric field in atomic units (hatree / q0 / a0).
-  mc0 - Electron mass * speed of light.
-  m0c2 - Electron rest mass energy.

Further constants are used in the QED (photons) physics package and the
bremsstrahlnug package. 

Any new constants required should be specified in the same place in
`constants.F90`.

## Shape and size variables
As well as the physical constants, there are some important variables which
you will have to use to do any development with EPOCH. As a general note,
since EPOCH is written with separate 1D, 2D and 3D versions, definitions will
be given for the 3D version of the code and irrelevant dimensions should just
be left out.
 
-  `INTEGER :: nx, ny, nz` - The number of gridpoints on the current
  processor in each direction. This may change when the load balancer
  activates, so always use these variables rather than local copies.
-  `INTEGER :: nx_global, ny_global, nz_global` - The number of gridpoints
  in each direction of the whole domain. These numbers will never change and
  will be the numbers read in from the input deck.
-  `INTEGER(KIND=8) :: npart_global` - The global number of particles
  specified in the input deck. This is not updated as particles leave the
  domain through boundaries etc. so it is not guaranteed to be accurate.
-  `INTEGER :: n_species` - The number of species of particles specified.
-  `INTEGER :: nsteps` - The maximum number of steps that the core solver
  should take.

-  `INTEGER, DIMENSION(1:nproc{x,y,z}), ALLOCATABLE :: cell_{x,y,z}_min,`
  `cell_{x,y,z}_max` - The variables `cell_{x,y,z}_min` and
  `cell_{x,y,z}_max` represent the part of a global array which is held by the
  current processor. Since EPOCH is an MPI code, there doesn't exist a
  single copy of any of the global arrays anywhere, but if there did then each
  processor would be responsible for the slice which runs
  (cell_x_min(rank):cell_x_max(rank),
  cell_y_min(rank):cell_y_max(rank))
  in 2D.  These variables are used internally in the load balancer, where it is
  updated, but is also used when calculating distribution functions. Here it is
  used to define the extents of the MPI type which is used to write the
  distribution function to disk.

-Useful global parameters exist for tracking the position and size of the fields 
stored by each MPI rank. The variables:

- `INTERGER :: {x,y,z}_grid_min_local` - These give the position of the 
cell-**centre** which has indices (1,1,1) on the current MPI rank. The grids on 
each rank only contain a fraction of the total simulation cells.

- `LOGICAL :: {x,y,z}_{min,max}_boundary` - Logical flags to determine whether 
the current rank is on the simulation edge. If `x_min_boundary` is true, then 
the current MPI rank has no neighbouring ranks on the low-$x$ edge.  

## The timestep

The timestep is calculated in the subroutine `set_dt` in the file
`src/housekeeping/setup.F90`. All that the subroutine has to do is set
the variable `dt` to set the timestep for the whole code. Any
additional timestep constraints should be coded into this subroutine. This
should be implemented after the existing `dt=` lines but before the
line `dt = dt_multiplier * dt`. Such a modification should be set so
that it only changes the timestep if the timestep is MORE restrictive than that
calculated from the core code. An example would be:
 ```perl
  dt = dx * dy / SQRT(dx**2 + dy**2) / c
  dt = MIN(dt, my_new_dt)
 ```

In the core EPOCH code the timestep can be calculated identically on each
processor, so there is no requirement to synchronise the timestep across
multiple processors. If your new timestep restriction uses information local to
each processor then some additional lines must be added to the
`set_dt` routine after the timestep has been calculated which
should read:
 ```perl
  REAL(num) :: dt_global
          .
          .
          .
  CALL MPI_ALLREDUCE(dt_global, dt, 1, mpireal, MPI_MIN, comm, errcode)
  dt = dt_global
 ```

This uses another MPI command to determine the most restrictive timestep across
all processors. EPOCH is not written in a way that permits operation with
different timesteps on different processors, and the behaviour of the code is
undefined (and likely wrong) if the code runs with different timesteps on
different processors.

## Input deck variables
 
-  `CHARACTER(LEN=entry_length) :: blank` - A special string which the input
  deck parser uses to indicate that it's passing a blank string rather than a
  string read from the deck which just happens to be blank.
-  `INTEGER :: deck_state` - An integer determining the current sweep of
  the input deck by the deck parser.
-  `INTEGER, PARAMETER :: num_vars_to_dump` - A variable describing the
  number of variables which should be selectable in the input deck as possible
  variables to dump.
-  `CHARACTER(LEN=entry_length) :: extended_error_string` - String used by
  some error codes in the deck parser to give more user friendly error
  messages.
-  `INTEGER :: data_dir_max_length` - The maximum number of characters in
  the name of the output directory.
-  `INTEGER :: n_zeros` - The number of leading zeros in the output filenames
  from EPOCH.
  
## Initial conditions (autoloader) variables

Initial conditions for the autoloader for a given species are described in
EPOCH by the Fortran TYPE `initial_conditions_block`. The
definition (in 3D) is:
 ```perl
TYPE initial_condition_block
  REAL(num), DIMENSION(:,:,:), POINTER :: density
  REAL(num), DIMENSION(:,:,:,:), POINTER :: temp
  REAL(num), DIMENSION(:,:,:,:), POINTER :: drift

  REAL(num) :: density_min
  REAL(num) :: density_max
END TYPE initial_condition_block
 ```

In 2D, the arrays have one fewer index, and in 1D they have two fewer.

 
-  `REAL(num) :: density` - Number density for the particles in the species.
  When defined runs (-2:nx+3,-2:ny+3,-2:nz+3).
-  `REAL(num) :: temp` - Temperature in Kelvin of the species in space. When
  defined runs (-2:nx+3,-2:ny+3,-2:nz+3,1:3). The final index of the array
  is a direction index, used to give anisotropic thermal distributions.
-  `REAL(num) :: drift` - Velocity drift in $m/s$ of the species in space.
  When defined runs (-2:nx+3,-2:ny+3,-2:nz+3,1:3). The final index of the array
  is the velocity direction component.
-  `density_min` - The minimum density below which the autoloader
  should not load particles.
-  `density_max` - The maximum density above which the autoloader
  should clip the density function.
  

The initial conditions themselves are in the variable
 ```perl
TYPE(initial_condition_block), DIMENSION(:), ALLOCATABLE :: initial_conditions
 ```
which is allocated to an array of size `1:n_species`.