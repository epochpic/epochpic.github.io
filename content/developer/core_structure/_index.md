---
title: Code Structure

draft: false
toc: true
type: docs

menu:
  developer:
    name: Core Structure
    weight: 40
---

EPOCH is designed so that it can fairly easily be extended while still being
written in (more or less) standard Fortran90 and MPI1.2. This section details
in increasing complexity what a programmer needs to know to develop the core 
systems of EPOCH, including the particle pusher, field solver, macro-particle
weights and current solver.

Specifically, the core structure pages contain:
- [Basic structures](/developer/core_structure/basic_structures.html) - Some
global and local variables, including physical constants, grid-properties, the 
time-step, and deck variables
- [Particles](/developer/core_structure/macro_particles.html) - How particles
are stored in EPOCH, and the information contained in their data-structures
- [Linked lists](/developer/core_structure/linked_lists.html) - An explanation 
of the computer-science concept "linked lists". These variable-length arrays are 
used to hold the EPOCH macro-particles.
- [Partlist](/developer/core_structure/partlist.html) - A description of the 
EPOCH subroutines used to create linked-lists for holding particles.
- [Field solver](/developer/core_structure/field_solver.html) - Shows where the 
fields are evaluated on the EPOCH grid, and the equations solved.
- [Particle pusher](/developer/core_structure/particle_pusher.html) - Describes 
how particle positions and velocities are updated in EPOCH.
- [Weighting functions](/developer/core_structure/shape_functions.html) - 
Explains how macro-particles are interpolated onto grid points, and vice versa.
- [Current solver](/developer/core_structure/current_solver.html) - Explains how 
current densities are calculated within the code.
- [Boundary conditions](/developer/core_structure/boundary_conditions.html) -
Details the different types of boundaries in an EPOCH simulation.
- [Parallelism](/developer/core_structure/parallelism.html) - Discusses how 
parallelism is achieved in the code, and how the EPOCH load-balancer works.
- [Pre-compiler flags](/developer/core_structure/precompiler_flags.html) - How 
the pre-compiler flags in the Makefile work, and how to use them in the code.