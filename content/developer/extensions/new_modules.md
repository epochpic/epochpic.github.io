---
title: New Module

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Extending EPOCH
    weight: 10
---

Adding a new module or physics package can turn out to be a very complicated 
process, but plugging it into the existing PIC-loop is usually quite 
straightforward. In this page, we will assume you have the source-code for a new 
module, and we will explain how to add it to EPOCH's calculations.

## The main driver routine
When adding completely new routines to the code, they should be added to the
file `src/epochnd.F90`. This routine simply calls other routines
to perform the actual execution of the code. The first section of the code
controls basic setup, MPI initialisation and
initial conditions. If you wish to add new startup conditions then
you should find the location in this routine where the
initial conditions are setup. The code is fairly complicated, but
there are a few key points at which the code significantly changes state.
 
-  After the call to `read_deck` the code has read the basic
  information from the input deck files and any tests or changes which have to
  be made to input deck values should be made immediately after this line. Note
  that although the variables from the deck have been set,
  none of these values have been used so allocatable
  variables have yet to be allocated. The grid does not exist at this point.
-  After the call to `open_files` the code has finished
  allocating all field variables, although particles may not yet have been set
  up. The grid now exists.
-  There are now a series of `IF` statements which test for
  things like `IF (IOR(ictype, c_ic_autoload) .NE. 0)`.
  These are the lines which test for all possible states of the
  initial conditions. The last test is for the manual load routine
  (`c_ic_manual`). After this test all the particles have been
  loaded and are now on their correct processor. The load balancer has now been
  called at least once so the domains may no longer be identical.
-  The main loop is a simple do loop beginning with just the single command
  `DO`. Inside this loop there are several calls to routines which
  actually advance the system. Most routines which can change currents should
  take place after the particle pusher but before the final update for the $E$
  and $B$ fields. These routines are
   
   -  `set_dt` - This routine sets the timestep.
   -  `update_eb_fields_half` - Time centre the $E$ and $B$
    fields.
   -  `push_particles` - The particle pusher.
   -  `reorder_particles_to_grid` - Groups particles into
    linked lists at each grid point. Used for the particle splitting routine,
    and binary collisions. Any routine
    which needs to have nearby particles grouped together should take place
    after the call to this routine.
   -  `split_particles` - The particle
    splitting operator.
   -  `reattach_particles_to_mainlist` - Undoes the
    particle grouping and rebuilds the main list of particles used by the
    particle pusher. Any routine which needs to have nearby particles grouped
    together should take place before the call to this routine.
   -  `update_eb_fields_final` - Updates the $E$ and $B$
    fields to the full timestep.
    
-  After the call to `update_eb_fields_final` the code is
  ready for another timestep. Any routines which do not change the time
  integrated properties of the code (like the moving window) should come after
  this call.
  
## The particle reordering routine 

After `reorder_particles_to_grid`, the particles have been moved to 
particle-lists unique to each cell. The main list
`species(ispecies)%attached_list` is empty and cannot be used
during this period. The particles should now be accessed using the variable
`species(ispecies)%secondary_list(ix,iy,iz)` which is the array of
linked lists. This array is allocated on the call to
`reorder_particles_to_grid` and deallocated on the call to
`reattach_particles_to_mainlist`, and should not be used outside
the section of code between these two calls. The particles themselves remain
unchanged. No attempt is made to check that particles do not cross processor
boundaries in this section, so if a particle's position is changed, it is up to
the user to ensure that the particle is transferred to another processor if
required. However, if a particle is transferred to another processor, it is
acceptable to relink it to `species(ispecies)%attached_list` since
the other lists are simply appended to that list when the particles are
reattached to the main list. 

You should avoid using these routines if possible, as they have a significant 
impact on performance. However, this remains the best method for cycling through 
processes which require particles to interact with other particles within the 
same cell, such as collisions or collisional ionisation. It is likely that new
physics modules for two-body interactions between incident and target particles
will also need to use this method (especially if the target particle changes).

