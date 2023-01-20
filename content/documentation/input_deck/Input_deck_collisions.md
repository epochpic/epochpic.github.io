---
draft: false
toc: true
type: docs

title: collisions block
linktitle: collisions block
weight: 190
menu:
  documentation:
    parent: Input deck
    weight: 120
---

This block contains information about particle collisions. See [EPOCH
input deck][Input_deck] for more information on the
input deck.

EPOCH has a particle collision routine with scattering algorithms based
on the model presented by Sentoku and Kemp[^1] or the model presented by
Pérez et al [^2], which in turn was based on the work of Nanbu and
Yonemura[^3]. This adds a new output block named "collisions", and since version 4.19, there are two 
possible collision modes: binary and background.
Binary collisions pair particles together, and apply scatter to both particles.
In the background mode, particle species are split into fast-background pairs, where
only particles in the fast species are scattered. Originally, EPOCH could only
run in the full binary-collisions mode, which accepts the following keys:

- `use_nanbu` - This logical flag determines whether the scattering angle of
Pérez/Nanbu will be used. The default is "T". If "F", the
Sentoku-Kemp algorithm will be used.

-   `coulomb_log` - This may either be set to a real value,
    specifying the Coulomb logarithm to use when scattering the
    particles or to the special value "auto". If "auto" is used then the
    routine will calculate a value based on the local temperature and
    density of the particle species being scattered, along with the two
    particle charges. If omitted, the default value is "auto".

-   `collide` - This sets up a symmetric square matrix of
    size $nspecies \times nspecies$ containing the collision frequency
    factors to use between particle species. The element (s1,s2) gives
    the frequency factor used when colliding species s1 with species s2.
    If the factor is less than zero, no collisions are performed. If it
    is equal to one, collisions are performed normally. For any value
    between zero and one, the collisions are performed using a frequency
    multiplied by the given factor.
If "collide" has a value of "all" then all elements of the matrix are
set to one. If it has a value of "none" then all elements are set to
minus one.
If the syntax "species1 species2 _value_" is used, then the
(species1,species2) element of the matrix is set to the factor
"_value_". This may either be a real number, or the special value "on"
or "off". _value_ may also be set to "background" to specify a fast-background pair (see below). The "collide" parameter may be used multiple times.
The default value is "all" (ie. all elements of the matrix are set to
one, modelling physical collisions). 

- `collisional_ionisation` - If this logical flag is set to
"T" then the collisional ionisation model is enabled. This process is
independent of *field_ionisation* (see
[here][Input_deck_species__ionisation]). However, in
order to set up *collisional_ionisation* you must also specify
ionisation energies and electrons in a *species* block (see
[here][Input_deck_species__ionisation]). The default
value is "F".

- `coll_n_step` - Number of time-steps, $n$, between collision calculations. This key speeds up the
collisions routine by reducing the number of collision calculations performed. On steps which apply
collisions, the calculation is performed using a time-step of $n$*dt, where dt is the simulation time-step.
The default is 1 (calculate collisions every step).

- `ci_n_step` - Only performs the collisional ionisation calculation once every $n$ steps, where $n$ is set by this parameter. This is done to speed up the code, and the default is 1 (every step). When this is greater than 1, the assumed time-step for the collisional ionisation calculation is $n$*dt. Note that an ion may only be ionised once per calculation, so if $n$ is too high, the number of ions will be underestimated.

An example deck using full binary collisions could be set up as follows.

```perl
begin:collisions
   use_collisions = T
   use_nanbu = T
   coulomb_log = auto
   collide = all
   collide = spec1 spec2 off
   collide = spec2 spec3 0.5
   coll_n_step = 10
end:collisions
```

With this block, collisions are turned on, the Nanbu-Pérez scattering
algorithm is used and the Coulomb logarithm is automatically calculated.
All values of the frequency array are set to one except (spec1,spec2) is
set to minus one (and also (spec2,spec1)) and (spec2,spec3) is set to
0.5. Note: only a frequency value of 1 provides a physical scatter.
Collisions are only calculated once every 10 steps, but using an 
inferred time-step of 10*dt.

# Background collisions

The background collisions mode offers a speed-up compared to the binary 
method when applicable. This mode assumes the particles in one species are
considerably faster than the particles in a background species, so the 
relative velocity between fast and background particles is roughly the 
fast particle speed. 
This eliminates the need to pair particles together in a local cell, as in
the binary-collision case. However, background particles will not
experience scatter in this mode. This mode was originally intended for 
electron-ion collisions, where both species had temperatures on the order 
of keV. An example of fast-background collisions is given below:
  
```perl
begin:collisions
   use_collisions = T
   coulomb_log = 5
   collide = none
   collide = Electron Ion1 background
   collide = Electron Ion2 background
   collide = Electron Electron on
   
   use_cold_correction = F
   rel_cutoff = 0.01
   back_update_dt = 5.0e-15 
end:collisions
```

The above example deactivates all collisions, then sets up two fast-background pairs,
with the Electron providing the fast species in both pairs, and Ion1 and Ion2 taking
the background roles. Full binary collisions are used for Electron-Electron collisions,
as these do not satisfy the fast-background assumptions. Additional speed-up parameters are
used which only apply to background collisions. These are: 

- `use_cold_correction` - The Nanbu-Pérez collisions model has a low
temperature correction factor, given in equation (20) of Pérez[^2]. 
In some cases, this only affects particles with energy
on the order of eV, and may be ignored in hot plasma. If "F", this calculation is skipped. The
default is "T", to perform the full calculation.

- `rel_cutoff` - Collisions are calculated in the centre-of-mass frame
between colliding fast and background particles. In practice, only the fast
particle momentum is transformed. This parameter takes a number between 0 and 1, and
if the fractional momentum change between frames is lower than this number, the
frame transform is skipped. The default is 0, so frame transforms are always considered.

- `back_update_dt` - If the background species number density varies
slowly, we do not need to re-calculate each step. This parameter specifies the 
time-step of recalculation for the background number density, and also the 
Coulomb logarithm if `coulomb_log` is set to auto. The default is 0,
so background variables are re-calculated each step.

# References

<references />

[^1]: Y. Sentoku and A. J. Kemp, "Numerical methods for particle
    simulations at extreme densities and temperatures: Weighted
    particles, relativistic collisions and reduced currents," J. Comput.
    Phys., 2008.
    [1](http://www.sciencedirect.com/science/article/pii/S0021999108001988)

[^2]: F. Pérez et al, "Improved modeling of relativistic collisions and
    collisional ionization in particle-in-cell codes ," Physics of
    Plasmas, 2012. [2](https://doi.org/10.1063/1.4742167)

[^3]: K. Nanbu and S. Yonemura, "Weighted Particles in Coulomb Collision
    Simulations Based on the Theory of a Cumulative Scattering Angle,"
    J. Comput. Phys., 1998. [3](https://doi.org/10.1006/jcph.1998.6049)



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
[Input_deck_species__ionisation]: /documentation/input_deck/input_deck_species#ionisation
