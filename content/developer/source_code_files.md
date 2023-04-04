---
draft: false
toc: true
type: docs

title: General layout of the EPOCH code
linktitle: Source code files
weight: 270
menu:
  developer:
    parent: Overview 
    weight: 10
---

The names of the source files in EPOCH are fairly self explanatory but, for
clarity, they are explained here. Some files have the normal Fortran file 
extension `.f90`, while some have the slightly unusual `.F90`. The difference is 
that files with the `.F90` extension are passed through the preprocessor before 
they are compiled allowing the use of precompiler directives (the `#ifdef` 
commands).

## Directories
All source files are contained in the `src` directory and its
subdirectories. There is a stylistic reason for the layout of the files, which
is explained here

 
-  `src` - Files in this directory are the core files for the
  basic EPOCH code, such as the field solvers, the particle pusher, the
  boundary conditions and the lasers.
-  `src/deck` - Files in this directory are responsible for
  dealing with the permanent input deck parser and include the core parts of
  the deck handler, and also the routines which deal with the blocks in the
  input deck files.
-  `src/housekeeping` - Files in this directory deal with those
  parts of the code operation which are not physics; including the load
  balancer, the MPI setup routines and the moving window.
-  `src/include` - This directory contains the shape function
  code fragments which are inserted into the particle push at compile time.
-  `src/io` - The files involved in all I/O activities, including
  the distribution functions and the particle probes.
-  `src/parser` - The files for the maths expression parser are
  in this directory, including both the core implementation of the shunting yard
  algorithm and the routines for implementing the permanent functions,
  constants and operators for the input deck.
-  `src/physics_packages` - Contains routines which implement
  additional physics for the code.
-  `src/user_interaction` - Contains any Fortran routines which
  a user has to modify to use the code with internal initial conditions, or to
  temporarily extend the maths parser or the input deck.

## The files in `src`
 
- `boundary.F90` - Includes all boundary conditions except laser
  and transmissive boundaries; including field and particle MPI boundaries, and
  field and particle domain boundaries.
- `constants.F90` - A collection of physical constants, and also integer flags 
  for I/O, boundaries, and other variables.
- `epoch1d.F90`, `epoch2d.F90`, `epoch3d.F90 ` - Main driver for the code. 
  Reading this routine gives the basic layout of the code flow.
- `fields.f90` - The Maxwell field solver.
- `laser.f90` - Includes laser and transmissive boundary
  conditions for each boundary and also the housekeeping routines for the laser
  objects.
- `particles.F90` - The particle pusher.
- `shared_data.F90` - This file includes all the global variable
  and type definitions. Usually new variables should be defined in this file.
- `gen_commit_string` - This is a script used to generate an ID
  string when compiling the code.
- `gen_src_module` - This is a script which is used at build
  time to generate a Fortran module containing the source code. This is used for
  embedding the EPOCH source code into restart dumps.
  

## The files in `src/deck`
 
- `deck.F90` - The main input deck routines. Deals with opening
  files, reading data and MPI distribution of the data to all processes. Also
  includes the routines which deal with calling the right reader routines to
  deal with a given block.
- `deck_boundaries_block.f90` - Reader routine for the
  "boundaries" block of the input deck.
- `deck_bremsstrahlung_block.f90` - Reader routine for the
  "bremsstrahlung" block of the input deck.
- `deck_collision_block.f90` - Reader routine for "collision"
  blocks in the input deck.
- `deck_constant_block.f90` - Reader routine for "constant"
  blocks in the input deck.
- `deck_control_block.f90` - Reader routine for "control"
  block in the input deck.
- `deck_dist_fn_block.f90` - Reader routine for "dist_fn"
  blocks in the input deck.
- `deck_fields_block.f90` - Reader routine for "fields"
  blocks in the input deck.
- `deck_io_block.F90` - Reader routine for the "io" block in
  the input deck.
- `deck_io_global_block.F90` - Reader routine for the "io_global" block in
  the input deck.
- `deck_laser_block.f90` - Reader routine for "laser" blocks
  in the input deck.
- `deck_part_from_file_block.F90` - Reader routine for the "part_from_file" 
  block in the input deck.
- `deck_particle_probe_block.F90` - Reader routine for
  "particle_probe" blocks in the input deck.
- `deck_species_block.F90` - Reader routine for "species"
  blocks in the input deck. This contains scripts which create the species_list 
array which holds the particle lists for each species.
- `deck_stencil_block.F90` - Reader routine for "stencil"
  blocks in the input deck.
- `deck_subset_block.F90` - Reader routine for "subset"
  blocks in the input deck.
- `deck_window_block.f90` - Reader routine for the "window"
  block in the input deck.
- `strings.f90` - Basic string handling routines such as
  "str_cmp" and routines for converting strings to numbers WITHOUT using the
  maths parser are covered in this routine.
- `strings_advanced.f90` - The routines which pass maths along
  to the maths parser routines are here.
  

## The files in `src/housekeeping`
 
- `balance.F90` - Contains the routines for the load balancer
  and related routines.
- `current_smooth.F90` - Contains the current smoothing routines.
- `epoch_source_info_dummy.f90` - A dummy module used when the
  code is being built without the ability to write the source code into restart
  dumps.
- `finish.F90` - Deallocates fields, species, subsets and physics packages at 
  the end of the simulation.
- `mpi_routines.F90` - Contains the routines dealing with the
  setup of the MPI layer and the creation of the communicator. Also allocates
  all arrays for the first time before load balancing.
- `mpi_subtype_control.f90` - Contains the routines that setup
  the mpi types required by the I/O subsystem.
- `particle_id_hash.F90` - Stores scripts responsible for identifying particles 
  in persistent subsets.
- `particle_pointer_advance.f90` - Contains subroutines which
  walk through the lists of particles and species for I/O purposes.
- `partlist.F90` - Contains the routines which deal with the
  particle lists which are used for inter-processor communication of particles.
- `prefetch.F90` - Hold scripts for pre-fetching particles. This requires Intel 
  compilation of the code, and can improve performance.
- `random_generator.f90` - Contains the random number generator
  routines.
- `redblack_module.f90` - Contains MPI routines used for the load-balancer.
- `setup.F90` - Deals with the setup of the grids and domains and
  restarting from previous output dumps.
- `shape_functions.F90` - Contains the particle shape functions
  used for calculating the particle weighting.
- `split_particle.F90` - Is the implementation of a
  demonstration of particle splitting routines.
- `terminal_controls.F90` - Makes the logo colourful on startup.
- `timer.f90` - Tracks the run-time of the code.
- `utilities.f90` - Contains growable arrays used by the species
  block parser.
- `version_data.F90` - Contains version information for the
  current EPOCH code.
- `welcome.F90` - The routine which prints the banner message and
  compiler options info.
- `window.F90` - The routines which deal with the moving window.
  

## The files in `src/io`
 
- `calc_df.F90` - Despite the slightly confusing name, this
  subroutine deals with derived functions like number density, charge density
  and mass density.
- `diagnostics.F90` - Contains the routines which actually dump
  the data, decide what to dump and also the routine to calculate the timestep.
- `dist_fn.F90` - Contains the routines to calculate the
  distribution functions, and also the routines handling the requests for
  distribution functions.
- `iterators.F90` - Contains the iterator functions used to write
  particle data into SDF files.
- `probes.F90` - Contains the routines which write the data from
  the particle probes. Also includes the routines which deal with user requests
  to add new particle probes.
- `simple_io.f90` - Contains routines for performing the simple
  binary I/O required by species_external and fields_external blocks.
  

## The files in `src/parser`
 
- `evaluate.f90` - Contains the routines which actually evaluate
  a tokenized expression. The core of this is a simple implementation of an
  RPN calculator.
- `evaluator_blocks.f90` - Contains the routines which evaluate
  a given token into a numerical values. Actually implements the functions,
  constants and operators in EPOCH's maths parser.
- `shunt.F90` - EPOCH's implementation of the
  "shunting yard" algorithm used to simultaneously tokenize the input and
  convert it from infix notation to RPN.
- `stack.f90` - Deals with routines for pushing onto and popping
  off stacks.
- `tokenizer_blocks.f90` - Deal with converting strings found
  in a string being tokenized into tokens. Essentially a large collection of
  "str_cmp" commands testing a string against a known name.
  

## The files in `src/physics_packages`
 
- `TABLES` - Subdirectory containing tables
for QED emissions, bremsstrahlung radiation, and both collisional and field 
ionisation. 
- `background_collisions.F90` - Package handling particle collisions for special
cases. Faster than the `collisions.F90` model, but assumes a heavy species
remains immobile during the collision. Appropriate for hot electron scatter
through cold ions.
- `bethe_heitler.F90` - Package sampling pair-production via the Bethe-Heitler 
process. This is run during the bremsstrahlung update.
- `bremsstrahlung.F90` - Runs bremsstrahlung radiation of electrons and positrons,
discussed in Appendix B of [Morris et al](https://aip.scitation.org/doi/10.1063/5.0055398).
- `collision_ionise.F90` - Package sampling collisional ionisation triggered by 
incident electrons. See [here](https://aip.scitation.org/doi/abs/10.1063/5.0126336) 
for details.
- `collisions.F90` - Package handling particle collisions. Estimates the 
number of binary Coulomb collisions over a simulation time-step, and samples a 
cumulative scatter angle. See [here](https://aip.scitation.org/doi/abs/10.1063/1.4742167)
for details. 
- `file_injectors.F90` - Injects particles from user-written text files (ASCII).
- `injectors.F90` - Package dealing with particle injection through
a boundary. Currently this handles drifting and non-drifting Maxwellian and
flux-Maxwellian distributions. 
- `ionise.F90` - Physics package dealing with field ionisation. 
Field ionisation consists of three distinct regimes; multiphoton in which
ionisation is best described as absorption of multiple photons, tunnelling 
in which deformation of the atomic Coulomb potential is the dominant factor,
and barrier suppression ionisation in which the electric field is strong enough
for an electron to escape classically. It is possible to turn off multiphoton or
barrier suppression ionisation through the input deck.
- `numerics.f90` - Some additional numerics
routines, primarily for the photons (QED) package.
- `photons.F90` - Package for some QED effects. 
Models Breit-Wheeler pair production, synchrotron emission and radiation 
reaction as described in [Duclous et al](http://iopscience.iop.org/article/10.1088/0741-3335/53/1/015009) and 
[Ridgers et al](https://www.sciencedirect.com/science/article/pii/S0021999113008061?via)   

## The files in `src/user_interaction`
 
- `custom_deck.f90` - This file is where and end user can
  temporarily extend the input deck.
- `custom_laser.f90` - The file where an end user specifies
  laser time profiles without using the input deck.
- `custom_parser.f90` - The file where an end user can
  temporarily add new functions and constants to the input deck.
- `deltaf_loader.F90` - Sets up parameters for the delta-f model.
- `helper.F90` - This file contains all the internal workings of
  the autoloader. This is in user_interaction for historical reasons, since
  early versions of the code required the end user to modify some parts of the
  functions contained in this file. As the autoloader has increased in
  complexity, this has ceased to be the case, so it is likely that soon this
  file will be move to "housekeeping".
- `ic_module.f90` - This file is where the internal and manual
  initial conditions are set.
- `particle_temperature.F90` - Contains the routines for
  thermally loading a particle species.