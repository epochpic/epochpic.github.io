---
draft: false
toc: true
type: docs

title: injector block
linktitle: injector block
weight: 230
menu:
  documentation:
    parent: Input deck
    weight: 150
---

The *injector* block specifies a particle source to be introduced
through a simulation boundary. Each injector block specifies a source of
a single species of particle defined by a density, centre of mass drift
momentum, temperature and number of simulation particles per cell. Injectors may also be set-up to
inject particles with properties and at times read from files created by the user.
The
current version of the injectors is incompatible with the
**`-DPER_SPECIES_WEIGHT`** compiler flag, and attempting to use an injector
with a version of EPOCH compiled with this flag will fail.

# Concepts

EPOCH can inject particles through any of the simulation boundaries.
This plasma is either a drifting Maxwellian corresponding to a
collisionally thermalized beam or a "flux Maxwellian" corresponding to
a Maxwellian source accelerated by an electrostatic accelerator. It can
have any temporal or transverse spatial profile of density, temperature
or drift that you wish to specify.

EPOCH does not automatically make any assumption about the plasma that
you wish to inject and does not correct for currents injected into the
domain. Current due to an injected beam will be smoothly created as the
particles enter the domain. If you wish to inject a neutral beam, you
will have to use multiple injectors to inject electrons and ions so as
to produce a neutral beam. Great care must be taken when introducing
relativistic beams since the current due to a highly relativistic beam
will not be the current due to the centre of mass velocity since EPOCH
does not use the Maxwell-Jüttner distribution for loading particles.

The user may over-ride this behaviour and inject particles with specific momenta, positions, weights
and ID values, at specific simulation times. These particle parameters are read from files, and syntax
for these is provided [here](#inject-particles-from-file).

# Boundary conditions {#boundary_conditions}

The injectors only work properly with certain boundary conditions. For
most purposes the "open" boundary condition is the only one that makes
sense with injectors since particles are flowing freely through the
boundary. Remember that in any version of EPOCH that supports injectors
you can also use per species boundary conditions to allow you to have
different boundary conditions for injected and bulk particles.

# Moving window {#moving_window}

Injectors and moving windows can be tricky to work with, so the default
behaviour of EPOCH is to stop all injectors when the window starts to
move. If you wish to override this behaviour then simply explicitly set
t_end in the injector block to a value after the window starts to move.
Setting

```perl
 t_end = t_end 
```

will cause the injectors to continue running until the end of the
simulation even with the moving window. You must take great care when
specifying injectors for a moving window because you will likely get
gaps or bunches in particles injected through the x boundary and there
will probably be some shearing of particles introduced through y and z
boundaries. It is in general recommended that you specify a velocity
profile for the moving window that stops at times when particles are to
be injected and then starts again once the injection is complete.

# Keys

-   `boundary` - specifies which boundary to attach the particle source
    too. Same specification as the
    [laser block][Input_deck_laser], so
    permitted values are x_min, x_max, y_min, y_max, z_min and
    z_max

-   `species` - specifies which species should be injected through the
    boundary. Just specify the name of the species required.

-   `t_start` - Time at which to start the injector

-   `t_end` - Time at which to end the injector

-   `npart_per_cell` - target pseudo-particle density for the injector.

    Average number of particles injected will be this value or slightly
    higher if very few particles are specified

-   `number_density` - Number density of the particle source in $m^{-3}$.
    Can be space varying along the boundary to which the injector is
    attached and time varying

-   `number_density_min` - Minimum number density in $m^{-3}$ below
    which pseudo particles are not loaded. Use if the density has a
    profile to avoid injecting low weight particles in low density
    regions

-   `number_density_max` - Maximum particle number density in $m^{-3}$.
    When the number density in a cell rises above *number_density_max*
    the injector clips the density to number_density_max allowing easy
    implementation of exponential rises to plateaus for time-varying injectors.
    Note that the number of particles per cell is kept fixed and the number
    density adjustment is achieved by modifying the particle weight. This
    flag has no effect for particles with per-species weighting.  If the
    flag has a negative value then no clipping is performed.
    This is the default.

-   `temp_x` - Temperature in x direction (K)

-   `temp_y` - Temperature in y direction (K)

-   `temp_z` - Temperature in z direction (K)

-   `temp` - Sets an isotropic temperature distribution in Kelvin. If both temp
    and a specific temp_x, temp_y, temp_z parameter is specified then the last
    to appear in the deck has precedence. If neither are given then the
    injector will have a default temperature of zero Kelvin.

-   `temp_{x,y,z}_ev, temp_ev` - These are the same as the temperature
    parameters described above except the units are given in electronvolts
    rather than Kelvin, i.e. using 1ev = 11604.5K .

-   `drift_x` - Momentum drift in x direction in $kg.m/s$

-   `drift_y` - Momentum drift in y direction in $kg.m/s$

-   `drift_z` - Momentum drift in z direction in $kg.m/s$

-   `drift_{x,y,z}` - Specifies a momentum space offset in $kg m/s$ to the
    distribution function for this injector. By default, the drift is zero.
    
-   `use_flux_maxwellian` - Logical flag to determine whether to use an
    accelerated flux Maxwellian rather than a drifting Maxwellian. This
    calculates the flux due to passing a Maxwellian source into an
    electrostatic accelerator instead of a drifting Maxwellian. If your
    particle source is a lab accelerator then you may want to set this to true.

# Example Deck {#example_deck}

```perl
begin:injector
   boundary = x_min
   species = Electron
   number_density = dens
   temp_x = temp
   drift_x = drift_p
   npart_per_cell = 32
end:injector
```

# Inject particles from file 

The plasma injectors may be over-written to allow the user to inject macro-particles
with specific momenta, positions and weight, at given simulation times on given boundaries.
Files containing injected particle properties must be formatted in a particular way.
Each variable type (position, momentum, weight, time) must be stored in a separate file.
Each line of a given file corresponds to a variable value for one particle, and particles must
be arranged in chronological order.

For example, a user wants to inject 3 particles of weights 10, 20 and 30, at times 1.0e-15 s,
2.0e-15 s and 3.0e-15 s respectively, into a 1D simulation through the _x\_min_ boundary.
The file containing injection time data (_inject\_t.txt_) would contain:

```perl
1.0e-15
2.0e-15
3.0e-15
```

and the weight data file (_inject\_w.txt_) would contain:

```perl
10
20
30
```

The user could create similar files to describe the $p_x$, $p_y$ and $p_z$
momentum components of each injected particle, where the first value in 
each file would be assigned to the 1.0e-15 macro-particle. In higher
dimensions, injection position on the boundary must also be specified.
Particle ID may be given if **`-DPARTICLE_ID`** or **`-DPARTICLE_ID4`**
are specified.

The user may have multiple file-injectors running simultaneously, by
defining multiple file-injector blocks. An example block is provided below 
for a 2D simulation. In this example, all files are present in the same
directory as the input deck.

```perl
begin:injector

  boundary = x_min
  species = Electron
  inject_from_file = T 
  
  y_data = "inject_y.txt"
  px_data = "inject_px.txt"
  py_data = "inject_py.txt"
  w_data = "inject_w.txt"
  t_data = "inject_t.txt"

end:injector
```

- `inject_from_file` - If "T", the code will ignore the flux-Maxwellian keys,
  and will instead inject particles based on the \{...\}\_data keys.
  
- `{x, y, z}_data` - Files containing the positions of injected particles.
  These are not used in 1D simulations, but must be used in 2D and 3D. In this example,
  no $x$ file is given, as all particles are injected through _x\_min_.
  
- `{px, py, pz}_data` - Files containing the momenta of injected particles.
  These are optional parameters - if a momentum component file is missing, this 
  component will be set to zero for all injected particles.
  
- `w_data` - The file containing the weights of all injected particles. This 
  data is mandatory.
 
- `t_data` - The file containing the times each particle passes the boundary.
  Injected particles will be positioned outside the simulation window, such that
  they pass the boundary at the time specified in this file. This data is mandatory.
  
- `id_data` - The file containing the ID values assigned to each injected
  particle. This is optional, and may only be used if the code is compiled with 
  either **`-DPARTICLE_ID`** or **`-DPARTICLE_ID4`**.

# Warnings

Currently injectors are a beta feature of EPOCH. We believe them to work
correctly, but unusual results must be considered suspect. If you get
unexpected results, please contact the EPOCH development team.



<!-- ########################  Cross references  ######################## -->


[Input_deck_laser]: /documentation/input_deck/input_deck_laser
