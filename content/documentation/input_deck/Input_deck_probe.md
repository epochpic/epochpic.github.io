---
draft: false
toc: true
type: docs

title: Probe block
linktitle: Probe block
weight: 180
menu:
  documentation:
    parent: Input deck
    weight: 10
---

This block contains information about particle probes used for output.
See [EPOCH input deck][Input_deck] for more information
on the input deck.

Sometimes it is useful to consider all the properties of particle which 
pass through a point/line/plane (depending on dimension) in the 
simulation. To allow this, it is possible to specify one or more 
*`Particle Probe`* blocks in the input deck. These record 
copies of all particles which cross a point/line/plane in a given
direction which meet minimum and maximum kinetic energy criteria and
output the particle properties into the normal output files. Each output
file only contains properties for particles which have passed the probe
since the previous output, and not all particles which have passed the 
probe since the start of the simulation. 

Particle
probes record the positions, momenta and weight of all particles passing
through the plane.
If the code is compiled with **`-DPARTICLE_ID`** or **`-DPARTICLE_ID4`**, 
the code also outputs the ID of passing particles. 
If the code is compiled with **`-DPROBE_TIME`**, the time at which the
particle touches the probe surface is also output.

To use particle probes, the code must not have been
compiled with the **`-DNO_PARTICLE_PROBES`** compiler option. This is
a fairly heavyweight diagnostic since each particle position must be tested
from within the particle push. The code will run faster if it is not compiled
in.

The probe is specified in terms of a point in the plane and the normal
vector to the plane which is to be monitored. Particles are only
recorded if they cross the plane in the direction given by the normal
vector. If you want to record particles travelling in both directions
then use two particle probes, one with an opposite signed normal vector
to the other.

```perl
begin:probe
   name = electron_back_probe

   point = (50.0e-6, -50.0e-6)
   normal = (1.0, 0.0)

   ek_min = 0.0
   ek_max = -1.0
   include_species : s1

   dumpmask = always
end:probe
```

-   `name` - The name that the probe should have in output
    dumps. Output variables are then named this as a prefix. For
    example, the block shown above will result in the name
    **electron_back_probe_px** for the x momentum. The particle
    positions would just be called **electron_back_probe**.
-   `point` - An arbitrary point in the plane of the probe.
-   `normal` - A vector normal to the plane of the probe, in
    the direction of crossings you wish to monitor.
-   `include_species` - The species to which this probe
    should be applied. To probe several species, use several probe
    blocks in the input deck. "probe_species" is accepted as a
    synonym.
-   `ek_min` - The minimum kinetic energy of particles to
    store information about. Set to 0 for no minimum kinetic energy.
-   `ek_max` - The maximum kinetic energy of particles to
    store information about. Set to -1 for no maximum kinetic energy.
-   `dumpmask` - The dump code for this particle probe. This
    is the same as that for the main output controls in **input.deck**.
    Note that the code has to store copies of particles which pass
    through the probe until a dump occurs. This means that the code's
    memory requirements can increase drastically if this code only dumps
    probe information infrequently. If this is set to **never** then the
    code effectively never uses the probe.



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
