---
title: Particle Pusher

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Core Structure
    weight: 69
---

EPOCH's particle pusher is based on the one from the PSC by Hartmut Ruhl, and
is a Birdsall and Landon type PIC scheme using Villasenor and Buneman current
weighting. It is contained in the file `particles.F90`. The
operation of the particle pusher is fairly simple, but there are a few elements
which need some clarification.
 
-  The update to the particle momenta etc. does not explicitly include the
  particle weight function. This means that the pseudoparticle momenta etc. are
  the momentum for a single real particle of the collection of real particles
  represented by that pseudoparticle, NOT the momentum of the whole collection
  of real particles.
-  `gamma` - The variable gamma which appears in various places
  is the relativistic $\gamma$ which is needed to convert the particle momentum
  into the particle velocity using the relation $\vec{p} = \gamma m \vec{v}$.
  Here, $\vec{p}$ is the particle momentum, $\vec{v}$ is the particle
  velocity and $m$ is the particle rest mass.
  If EPOCH was not relativistic then this would simply be $1$. Since
  EPOCH is relativistic, gamma is defined as
  $\left(\vec{p}.\vec{p}/m c^2 + 1\right)^{1/2}$.
-  `cell_x1=cell_x1+1` - There are lines like this after all
  the sections of the routine where the cell a particle is in is
  calculated. This is because, for a cell centred variable, the domain runs
  (1:nx,1:ny,1:nz) rather than (0:nx-1,0:ny-1,0:nz-1).

The most complicated parts of the particle pusher are interpolating the grid 
electric and magnetic fields over the [macro-particle shape](/developer/core_structure/shape_functions.html), and the [current density solver](/developer/core_structure/current_solver.html).