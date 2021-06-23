---
draft: false
toc: true
type: docs

title: Input deck particle file
linktitle: Input deck particle file
weight: 250
menu:
  documentation:
    parent: Input deck
    weight: 170
---

This block contains information about the block used to load particles
from file. See [EPOCH input deck][Input_deck] for more
information on the input deck.

The particles_from_file block is similar in function to the fields
block, it allows the loading of custom particle data from raw binary
data files. An example usage of the block is shown below

```perl
begin:particles_from_file 
   species = "electron" 

   # Load mandatory data for 3D simulation
   x_data = "xdata.dat"
   y_data = "ydata.dat"
   z_data = "ydata.dat"
   w_data = "ydata.dat"

   # Load particle ids in 4 byte int format,
   # ignoring first 8 bytes of file 
   #offset = 8
   #id4_data = "iddata.dat"
end:particles_from_file
```

Specifying a particles_from_file block for a species causes EPOCH to
load the per-particle data from the specified files. Data files are
assumed to be in order such that the first variable in each file will be
attributed to one particle, the second variable in each file to a second
electron, and so on. A target species to load to, as well as particle
position and weight data (unless has been set) must be supplied. With
the exception of particle ID, any optional parameters which are left
unspecified will be initialised to zero.
If the code has been compiled with or then particle IDs may be loaded
from a raw binary file of integers of either size 4 or size 8 regardless
of the compile time flag choice. If no particle ID data is supplied, IDs
will be generated sequentially from 1.
All other data should be in the form of floating point numbers of the
same precision as in the core code.
A particles_from_file block accepts the following parameters:
- `species` - Name of the species to which the particles will
be loaded. This is a mandatory parameter and the corresponding species
block must be defined.
- `{x,y,z}_data` - File containing particle position data in
$m$. This data must be supplied, up to the dimensionality of the
simulation.
- `w_data` - File containing pseudoparticle weight, this is
the number of real particles the pseudoparticle represents. This data
must be supplied.

-   `{px,py,pz}_data` - File containing particle momentum
    data in $kg\,ms^{-1}$. The default value is zero.
-   `id{4,8}_data` - File containing particle IDs in either
    4 or 8 byte unsigned integer representation.
-   `offset` - File offset. Number of bytes at the head of
    the file to be ignored, may be specified multiple times. see for
    more details of behaviour.



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
