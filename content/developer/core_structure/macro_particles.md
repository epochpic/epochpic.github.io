---
title: Particles

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Core Structure
    weight: 41
---

In the PIC method, the simulation grid is filled with macro-particles, which 
respresent a large number of _real_ particles. They are described primarily by a 
position on the grid, and a momentum, charge, mass and weight. In EPOCH, the 
first few variables describe a _single_ real particle within the macro-particle,
and the weight describes how many real particles the macro-particle represents.

Macro-particles are split between different species, set by the species blocks 
in the input deck. All macro-particles present on a single rank are contained 
within the `species_list` variable, which contains lists of particles for all 
species present, along with information about each species.

In short, the data is distributed among types as follows:

- Particle type: contains information about a single macro-particle.
- Particle list: a linked list object, containing a list of macro-particles.
- Particle species: contains information about a set of particles as a whole, 
  and a corresponding particle list.
- Species list: an array containing all the particle species.

## The particle data-type

Particles are represented as linked lists of Fortran TYPES. The definition can 
be found in `shared_data.F90`, which is shared globally among all modules. 
Hence, any module can use a particle data-type. 

In this data-type, you will 
notice that many parameters are locked behind pre-processor flags, and must 
be manually switched on through the Makefile. This is because as particles move 
around the grid, they must be transferred from processor to processor as they 
move in and out of cells controlled by each rank. This MPI transfer is a major 
bottleneck for the code, so by default, EPOCH assigns the minimum amount of
information to each particle, to minimise the rank-to-rank data transfer.

The datatype is shown here:

 ```perl
TYPE particle  
    REAL(num), DIMENSION(3) :: part_p
    REAL(num), DIMENSION(c_ndims) :: part_pos
#if !defined(PER_SPECIES_WEIGHT) || defined(PHOTONS)
    REAL(num) :: weight
#endif
#ifdef DELTAF_METHOD
    REAL(num) :: pvol
#endif
#ifdef PER_PARTICLE_CHARGE_MASS
    REAL(num) :: charge
    REAL(num) :: mass
#endif
    TYPE(particle), POINTER :: next, prev
#ifdef PARTICLE_DEBUG
    INTEGER :: processor
    INTEGER :: processor_at_t0
#endif
#ifdef PARTICLE_ID4
    INTEGER :: id
#elif PARTICLE_ID
    INTEGER(i8) :: id
#endif
#ifdef COLLISIONS_TEST
    INTEGER :: coll_count
#endif
#ifdef WORK_DONE_INTEGRATED
    REAL(num) :: work_x
    REAL(num) :: work_y
    REAL(num) :: work_z
    REAL(num) :: work_x_total
    REAL(num) :: work_y_total
    REAL(num) :: work_z_total
#endif
#ifdef PHOTONS
    REAL(num) :: optical_depth
#endif
#if defined(PHOTONS) || defined(BREMSSTRAHLUNG)
    REAL(num) :: particle_energy
#endif
#if defined(PHOTONS) && defined(TRIDENT_PHOTONS)
    REAL(num) :: optical_depth_tri
#endif
#ifdef BREMSSTRAHLUNG
    REAL(num) :: optical_depth_bremsstrahlung
#endif
#if defined(PROBE_TIME)
    REAL(num) :: probe_time
#endif
END TYPE particle
 ```
And the descriptions are
 
-  `REAL(num) :: part_p(3)` - The particle momentum. Always dimension 3 even
  in 1D and 2D codes. Describes the momentum of a single particle within the 
  macro-particle.
-  `REAL(num) :: part_pos(ndims)` - The particle position. Has
  the same dimensions as that of the code.
-  `REAL(num) :: weight` - The particle weight if the code is running with
  per particle weighting (otherwise weight is a species parameter).
-  `REAL(num) :: charge` - The particle charge in Coulombs if the code is
  running with per particle charge (otherwise charge is a species parameter).
-  `REAL(nun) :: mass` - The particle mass in kilograms if the code is
  running with per particle mass (otherwise mass is a species parameter).
-  `TYPE(particle), POINTER :: next, prev` - The pointers to the next and
  previous elements of the linked list.
-  `INTEGER :: processor` - The rank of the processor that the particle
  thinks it is on. Used for debugging.
-  `INTEGER :: processor_at_t0` - The rank of the processor that the
  particle started on. Used for debugging.
-  `INTEGER :: id` - A unique integer assigned to each particle, only set for 
  species which output the id, and only when the code is compiled with ID 
  support.
- `INTEGER :: coll_count` - An integer present for debugging the number of
  collisions a particle undergoes (this is not used in the current version of
  EPOCH).
- `REAL(num) :: work_{x,y,z}` - Work done by the electric field on the 
  particle during the last particle push, in each direction. 
  Describes work done to a single particle within the macro-particle.
- `REAL(num) :: work_{x,y,z}_total` - Work done by the electric fields on the 
  particle over the full simulation, in each direction. 
  Describes work done to a single particle within the macro-particle.
- `REAL(num) :: optical_depth` - For some secondary-particle emission processes,
  a particle is assigned an "optical depth of emission", related to how far the
  particle has moved, and the cross section of emission. The total optical depth 
  is tracked, and an emission is sampled once the optical_depth goes negative. 
  The original `optical_depth` here describes the remaining optical depth to 
  travel 
  before photon emission through the QED non-linear Compton scatter process for 
  electrons. For photons, this describes the remaining optical depth before
  Breit-Wheeler pair production.
- `REAL(num) :: particle_energy` - Contains the relativistic particle energy, 
  primarily for use in the QED routines.
- `REAL(num) :: optical_depth_tri` - Remaining optical depth before emission of 
  a pair through the electron trident process.
- `REAL(num) :: optical_depth_bremsstrahlung` - Remaining optical depth before 
  emission of a bremsstrahlung photon for electrons, or a Bethe-Heitler pair 
  for photons.
- `REAL(num) :: probe_time` - If a particle passes a probe, a copy of the
  particle is stored in a new list. This parameter is set to the time the
  particle passes the probe, for the particle copy.
  
**WARNING**: Simply adding a new parameter to the definition of the particle 
type is NOT
sufficient to extend the particle type, since the communications when the
particle crosses a processor boundary do not know about the new parameter and
it will not be transmitted with the particle. How to add new properties to the
particle communication layer is described later.

## Particle list type

The entire linked list of particles is encapsulated in another Fortran TYPE,
called `particle_list`, which is defined as:
 ```perl
TYPE particle_list
  TYPE(particle), POINTER :: head
  TYPE(particle), POINTER :: tail
  INTEGER(KIND=8) :: count
  ! Pointer is safe if the particles in it are all unambiguously linked
  LOGICAL :: safe

  TYPE(particle_list), POINTER :: next, prev
END TYPE particle_list
 ```
And its properties are:
 
-  `TYPE(particle), POINTER :: head` - The first particle in the linked list.
-  `TYPE(particle), POINTER :: tail` - The last particle in the linked
  list. New particles added to the end of the list are added onto the end of
  the tail element, and the new last particle becomes the new tail element.

-  `INTEGER(KIND=8) :: count` - The number of particles in this particle
  list. Note that the `particle_list` type is not
  directly MPI aware, so this is literally the number of particles in
  _this_ particle list, not the number of particles of this species on all
  processors.
-  `LOGICAL :: safe` - A particle list _safe_ if the particles in it
  are unambiguously linked. That is that the `count`th particle is
  guaranteed to have its `next` property be null. Most particle
  lists within EPOCH are safe, but sometimes it is useful to be able to have
  particle lists which are subsets of longer particles lists, and these
  particle lists are not _safe_.
-  `TYPE(particle_list), POINTER :: next, prev` - At present, EPOCH does
  not use these pointers, which are intended to allow multiple particle lists to
  be attached together. Certain parts of EPOCH, such as the I/O system are
  aware of these pointers and will automatically use them if they are ever
  set. They are reserved for future use.
  
The `particle_list` objects are used to abstract all the functions
of the linked list, including adding and removing particles and transporting
particles between processors. 
 
## Particle species type

The particle species are represented by yet another Fortran TYPE, this time
called `particle_species`, which in 2D is defined as:
 ```perl
 TYPE particle_species
    ! Core properties
    CHARACTER(string_length) :: name
    TYPE(particle_species), POINTER :: next, prev
    INTEGER :: id
    INTEGER :: dumpmask
    INTEGER :: count_update_step

    REAL(num) :: charge
    REAL(num) :: mass
    REAL(num) :: weight
    INTEGER(i8) :: count
    TYPE(particle_list) :: attached_list
    TYPE(particle_pointer_list), POINTER :: boundary_particles => NULL()
    LOGICAL :: immobile
    LOGICAL :: fill_ghosts

    ! Parameters for relativistic and arbitrary particle loader
    INTEGER :: ic_df_type
    REAL(num) :: fractional_tail_cutoff

    TYPE(primitive_stack) :: dist_fn
    TYPE(primitive_stack) :: dist_fn_range(3)

#ifndef NO_TRACER_PARTICLES
    LOGICAL :: zero_current
#endif

    INTEGER :: atomic_no
    LOGICAL :: atomic_no_set = .FALSE.

    ! Specify if species is background species or not
    LOGICAL :: background_species = .FALSE.
    ! Background density
    REAL(num), DIMENSION(:,:), POINTER :: background_density
    ! Do we need to make secondary lists for this species?
    LOGICAL :: make_secondary_list = .FALSE.
    ! Has species list been randomised in order?
    LOGICAL :: is_shuffled

    ! Specifiy if species is background for collisions
    LOGICAL :: coll_background = .FALSE.
    LOGICAL :: coll_fast = .FALSE.
    LOGICAL :: coll_pairwise = .FALSE.

    ! ID code which identifies if a species is of a special type
    INTEGER :: species_type

    ! particle cell division
    INTEGER(i8) :: global_count
    LOGICAL :: split
    INTEGER(i8) :: npart_max
    ! Secondary list
    TYPE(particle_list), DIMENSION(:,:), POINTER :: secondary_list

    ! Loading of particles
    REAL(num) :: npart_per_cell
    TYPE(primitive_stack) :: density_function, temperature_function(3)
    TYPE(primitive_stack) :: drift_function(3)

    ! Thermal boundaries
    REAL(num), DIMENSION(:,:), POINTER :: ext_temp_x_min, ext_temp_x_max
    REAL(num), DIMENSION(:,:), POINTER :: ext_temp_y_min, ext_temp_y_max

    ! Species_ionisation
    LOGICAL :: electron
    LOGICAL :: ionise
    INTEGER :: ionise_to_species
    INTEGER :: release_species
    INTEGER :: n
    INTEGER :: l
    REAL(num) :: ionisation_energy
    REAL(num), ALLOCATABLE :: coll_ion_incident_ke(:)
    REAL(num), ALLOCATABLE :: coll_ion_cross_sec(:)
    REAL(num), ALLOCATABLE :: coll_ion_mean_bind(:,:)
    REAL(num), ALLOCATABLE :: coll_ion_secondary_ke(:,:)
    REAL(num), ALLOCATABLE :: coll_ion_secondary_cdf(:,:)

    ! Attached probes for this species
#ifndef NO_PARTICLE_PROBES
    TYPE(particle_probe), POINTER :: attached_probes
#endif

    ! Particle migration
    TYPE(particle_species_migration) :: migrate

    ! Initial conditions
    TYPE(initial_condition_block) :: initial_conditions

    ! Per-species boundary conditions
    INTEGER, DIMENSION(2*c_ndims) :: bc_particle
  END TYPE particle_species
 ```
Again, most of these properties are self explanatory, but they are detailed
below.
 
-  `CHARACTER(LEN=entry_length) :: name` - The name of the particle
  species. Used when constructing things like "ekbar_electron" and similar
  names.
-  `TYPE(particle_species), POINTER :: next, prev` - Particle species are
  connected to each other as a linked list using pointers as well as being
  available through a simple array. These pointers are used behind the scenes
  in the I/O.
-  `INTEGER :: id` - The number of the species, so for the species
  `species_list(1)`, the id field would be 1. For
  `species_list(2)`, the id field would be 2 etc.
-  `INTEGER :: dumpmask` - Bitmask to determine when this species should be
  dumped in diagnostic output.
-  `INTEGER :: count_update_step` - The last step where the `count` parameter
  was updated.
-  `REAL(num) :: charge` - The charge on a single particle of the species in
  Coulombs.
-  `REAL(num) :: mass` - The mass of a single particle of the species in
  kilograms.
-  `REAL(num) :: weight` - The per-species particle weight.
-  `INTEGER(KIND=8) :: count` - The number of particles of this species on
  all processors. NOTE that this is only accurate if the code is compiled with
  the correct preprocessor options. Without the correct preprocessor options,
  this will be accurate at the start of the code runtime, but will not be if
  any particles enter or leave the domain. This is mainly a debugging
  parameter.

-  `TYPE(particle_list) :: attached_list` - This is the
  `particle_list` object which holds the particles assigned to this
  species on this processor. Particles are attached to this list except between 
  the calls to `reorder_particles_to_grid` and
  `reattach_particles_to_mainlist` in
  `epoch{1,2,3}d.F90` where the particles are instead attached to
  `secondary_list`. This is explained later.
- `TYPE(particle_pointer_list), POINTER :: boundary_particles` - A list of 
  particles which have been pushed out of the simulation boundaries.
- `LOGICAL :: immobile` - These particles skip the particle push if true.
- `LOGICAL :: fill_ghosts` - Loads particles into ghost cells surrounding the
  simulation if true.
- `INTEGER :: ic_df_type` - Sets temperature calculation for the delta-f loader.
- `REAL(num) :: fractional_tail_cutoff` - Sets a cut-off for the relativistic 
  temperature to momentum calculation in 
  `src/user_interaction/particle_temperature.F90`
- `TYPE(primitive_stack) :: dist_fn` - Arbitrary momentum distribution function 
  for particle loading.
- `TYPE(primitive_stack) :: dist_fn_range(3)` - Set upper and lower ranges to 
  the momentum distribution functions.
-  `LOGICAL :: zero_current` - Whether or not this species is a tracer particle. 
  If
  a species is a tracer species then it moves under the fields as normal for a
  particle with its mass and charge but contributes no current.
- `INTEGER :: atomic_no` - Set atomic number of species. Used for bremsstrahlung 
  and ionisation.
- `LOGICAL :: atomic_no_set` - Identifies is an atomic number has been set for
  the species.
- `LOGICAL :: background_species` - If true, the species is not to load any 
  particles. Instead, this is represented with a density value in each cell, 
  and it can only act as a target species for bremsstrahlung radiation.
- `REAL(num), DIMENSION(:,:,:), POINTER :: background_density` - The number 
  density of a background species (for bremsstrahlung radiation).
- `LOGICAL :: make_secondary_list` - Checks if this species will ever need to 
  create a secondary list (see `secondary_list`).
- `LOGICAL :: is_shuffled` - Checks if the order of particles in secondary lists
  has been randomised
- `LOGICAL :: coll_background`- Checks if this species forms the background 
  species in a fast-background pair for background particle collisions.
- `LOGICAL :: coll_fast` - Checks if this species describes the fast species in
  a fast-background pair for background particle collisions.
- `LOGICAL :: coll_pairwise` - Checks if this species undergoes binary
  collisions with another species.
- `INTEGER :: species_type` - Tag the species as a particular type of particle, 
  using constants like `c_species_id_electron`. This is required for the 
  physics packages.
- `INTEGER(i8) :: global_count` - The number of particles from this species
  summed over all ranks.
-  `LOGICAL :: split` - EPOCH includes a very early version of a particle
  splitting operator. It works mechanically but has undesirable properties at
  present. If this flag is true then the code attempts to split the particles
  when the pseudoparticle number density drops too low.
-  `INTEGER(KIND=8) :: npart_max` - Used with the particle splitting
  operator. When the total number of particles equals this number, further
  particle splitting is suppressed.
-  `TYPE(particle_list), DIMENSION(:,:,:), POINTER :: secondary_list` - This
  describes an array of particle lists. The subroutine 
  `reorder_particles_to_grid` allocates
  `secondary_list(0:nx+1,0:ny+1,0:nz+1)` and then loops over all
  particles. It calculates the cell in which each particle is and moves the
  particle from `attached_list` to the correct element of
  `secondary_list` for that cell. This means the particles which
  are nearby in space are now linked together in an array of linked lists.
  This allows things such as collision operators which require direct
  interaction between nearby particles.
-  `INTEGER(KIND=8) :: npart_per_cell` - The number of pseudoparticles per
  cell in the initial conditions. This is used with the moving window function
  to ensure that the same number of particles per cell are used for the new
  material introduced at the leading edge of the window.
-  `REAL(num), DIMENSION(:,:), POINTER :: density` - The density of the
  plasma at the leading edge of the window at the start of the simulation. This
  is used to structure the density of the new material introduced at the leading
  edge of the plasma.
-  `REAL(num), DIMENSION(:,:,:), POINTER :: temperature` - The temperature
  in of the plasma at the leading edge of a moving window at the start of the
  simulation. The final index of the array is the direction in which the
  temperature is set (1=x, 2=y, 3=z).
- `REAL(num), DIMENSION(:,:,:), POINTER :: ext_temp_{x,y,z}_{min,max}` - Sets
  the temperature on the boundary for this particle species if `thermal` 
  boundaries have been used.
-  `LOGICAL :: electron` - Species is tagged as an electron for the ionisation 
  routines.
-  `LOGICAL :: ionise` - If the ionisation model is activated then this
  species should ionise.
-  `INTEGER :: ionise_to_species` - The species number for the next ionised
  state of this species.
-  `INTEGER :: release_species` - Specifies what type of particle should be
  released when this species ionises (i.e. which species is the electron).
-  `REAL(num) :: ionisation_energy` - The ionisation energy for the next
  ionisation of this species.
-  `INTEGER :: n, l` - The principle and angular quantum numbers of the
  outermost electron for this species, assuming a ground-state electron 
  configuration. This is used for field ionisation.
-  `REAL(num), ALLOCATABLE :: coll_ion_incident_ke(:)` - A table of 
   logarithmically spaced kinetic energy values for incident electrons in 
   collisional ionisation.
-  `REAL(num), ALLOCATABLE :: coll_ion_cross_sec(:)` - The collisional 
    ionisation cross sections corresponding to incident electrons with kinetic
    energies given by `coll_ion_incident_ke(:)`.
-  `REAL(num), ALLOCATABLE :: coll_ion_mean_bind(:,:)` - For each 
    incident/ejected electron energy pair for collisional ionisation, 
    this gives the mean bound electron energy, weighted by the ionisation cross 
    section of each bound shell.
-  `REAL(num), ALLOCATABLE :: coll_ion_secondary_ke(:,:)` - An array of possible 
    ejected electron energies for each incident electron kinetic energy in the 
    `coll_ion_incident_ke(:)` table.
-  `REAL(num), ALLOCATABLE :: coll_ion_secondary_cdf(:,:)` - The cumulative 
    distribution function for the probability of emission of an ejected electron 
    energy for a given incident energy during collisional ionisation, 
    corresponding to the energies in `coll_ion_secondary_ke`.
-  `TYPE(particle_probe), POINTER :: attached_probes` - A pointer pointing to
  the head of an attached linked list of particle probe diagnostics.
-  `TYPE(particle_species_migration) :: migrate` - Determines the criteria for 
  particles being moved to other species, using the particle migration routines.
-  `TYPE(initial_condition_block) :: initial_conditions` - Stores parameters 
  read from the species block, describing the initial conditions of the species.
-  `INTEGER, DIMENSION(2*c_ndims) :: bc_particle` - Boundary conditions for 
  particles in this species, which override the global boundaries set in the 
  input deck boundary block.