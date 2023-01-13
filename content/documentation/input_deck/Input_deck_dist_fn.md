---
draft: false
toc: true
type: docs

title: dist_fn block
linktitle: dist_fn block
weight: 170
menu:
  documentation:
    parent: Input deck
    weight: 100
---

This block contains information about distribution functions that should
be calculated for output. See [EPOCH input
deck][Input_deck] for more information on the input
deck.

Sometimes it is useful to reconstruct part of the full phase space for
one or more particle species. This functionality is provided through a
*dist_fn* block. The distribution function is integrated over all
dimensions which are not axes of the distribution function.

Calculating distribution functions requires some degree of integration
of data leading to various possible ways of normalising the resulting
distribution function. In EPOCH, distribution functions are normalised
so that the value at every point of the distribution function is the
number of particles within that cell of the distribution function,
ignoring all phase space directions which are not considered as an axis
of the distribution function. Summing the distribution function should
give the total number of real particles (as opposed to computational
pseudoparticles) in the simulation.

An example *dist_fn* block is given below:

```perl
begin:dist_fn
   name = x_px
   ndims = 2
   dumpmask = always

   direction1 = dir_x
   direction2 = dir_px

   # Range is ignored for spatial coordinates
   range1 = (1, 1)
   range2 = (-50.0e-20, 50.0e-20)

   # Resolution is ignored for spatial coordinates
   resolution1 = 1
   resolution2 = 5000

   restrict_py = (-3.0e-20, 3.0e-20)

   include_species:Electron
   include_species:Carbon
end:dist_fn
```

-   `name` - The name of the distribution function when it is
    output. This name is appended with the name of each species for
    which the data is output and so, for example, when applied to a
    species named carbon the output is called *x_px_Carbon*. The
    Cartesian grid which describes the axes of the distribution function
    would then be called *grid_x_px_Carbon*.
-   `ndims` - The number of dimensions in this phase space
    reconstruction. Due to difficulties in visualising data in more than
    three dimensions, this is restricted to being 1, 2 or 3.
-   `dumpmask` - Determines which output dumps will include
    this distribution function. The dumpmask has the same semantics as
    those used by variables in the "output" block, described
    [here][Input_deck_output_block]. The dumpmask from
    "distribution_functions" in the output block is applied first and
    then this one is applied afterwards. For example, if the dist_fn
    block contains "dumpmask = full" and the output block contains
    "distribution_functions = always" then this distribution function
    will be only be dumped at full dumps. The default dumpmask is
    "always".
-   `direction**n**` - This is the phase space to sample
    along axis . This can be any one of: dir_x, dir_y, dir_z,
    dir_px, dir_py, dir_pz, dir_en, dir_gamma_m1, dir_xy_angle,
    dir_yz_angle, dir_zx_angle with spatial codes only being
    available in dimensionalities of the code which have that direction.
    Therefore dir_z does not exist in EPOCH1D or EPOCH2D and dir_y
    does not exist in EPOCH1D.

The flags "dir_xy_angle", "dir_yz_angle" and "dir_zx_angle"
calculate the distribution of particle momentum directions in the X-Y,
Y-Z and Z-X planes. In general, "dir_ij_angle" collapses the particle
momentum into the $ij$ plane, and quotes the momentum angle with 
respect to the positive $i$ unit vector. The angle is output in the
range of $\pi$ to $-\pi$. Angles towards $+j$ are positive, and vice-versa
for negative $j$.  

- `range**n**` - The range between which this axis should
run. This is in the form of (minimum, maximum). Any particle which
exceeds the range is ignored. For momentum directions this parameter is
specified in $kg\ ms^{-1}$. If the range of a momentum direction is set
so that the maximum and the minimum are equal then the code will
automatically set the range to exactly span the range of particle
momenta at the point of writing the dump.
- `resolution**n**` - The number of gridpoints in a given
direction. This is ignored for spatial dimensions where the resolution
is always the same as the resolution of the underlying simulation.
- `include_species` - Specifies a species which should be
included in the output. This is useful since it is rare that momentum
limits are appropriate for both electrons and ions, so usually for a
given dist_fn block only electrons or ions are considered. It is
possible to have two dist_fn blocks with the same name but different
ranges and different include_species settings produce the effect of a
single diagnostic for all species in the output file.
- `output_deltaf` - If set to "T", the particle weights used
in calculating the distribution function is adjusted by subtracting the
Delta-f distribution function for the particle species. The default
value is "F".
- `restrict_{x,y,z,px,py,pz}` - Restrictions are specified
in the same way as ranges, but have a subtly different behaviour. Ranges
specify the range of a visible axis on the resulting distribution
function, whereas restrictions allow you to specify minimum and maximum
values for each spatial and momentum direction and use only particles
which fall within this range when calculating the distribution function.
Restrictions can be specified even for properties which are not being
used as axes. It is possible to set a restriction that is more
restrictive than the range applied. This is not trapped as an error and
such parts of the distribution function are guaranteed to be empty. The
available spatial restrictions depend on the dimensionality of the code.
Therefore, attempting to set restrict_z in EPOCH1D will produce a
warning.
At present, the code to calculate the distribution functions has one
limitation: it ignores particle shape functions when calculating
properties on the spatial axis, meaning that the result is less smooth
than normal properties from the code.



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
[Input_deck_output_block]: /documentation/input_deck/input_deck_output_block
