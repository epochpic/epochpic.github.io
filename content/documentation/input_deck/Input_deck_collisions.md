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
Yonemura[^3]. This adds a new output block named "collisions" which
accepts the following four parameters.
- `use_collisions` - This is a logical flag which determines
whether or not to call the collision routine. If omitted, the default is
"T" if any of the frequency factors are non-zero (see below) and "F"
otherwise.
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
    size $nspecies\,\times\,nspecies$ containing the collision frequency
    factors to use between particle species. The element (s1,s2) gives
    the frequency factor used when colliding species s1 with species s2.
    If the factor is less than zero, no collisions are performed. If it
    is equal to one, collisions are performed normally. For any value
    between zero and one, the collisions are performed using a frequency
    multiplied by the given factor.

If "collide" has a value of "all" then all elements of the matrix are
set to one. If it has a value of "none" then all elements are set to
minus one.
If the syntax "species1 species2 <value>" is used, then the
(species1,species2) element of the matrix is set to the factor
"<value>". This may either be a real number, or the special value "on"
or "off". The "collide" parameter may be used multiple times.
The default value is "all" (ie. all elements of the matrix are set to
one).
- `collisional_ionisation` - If this logical flag is set to
"T" then the collisional ionisation model is enabled. This process is
independent of *field_ionisation* (see
[here][Input_deck_species__ionisation]). However, in
order to set up *collisional_ionisation* you must also specify
ionisation energies and electrons in a *species* block (see
[here][Input_deck_species__ionisation]). The default
value is "F".
For example:

```perl
begin:collisions
   use_collisions = T
   use_nanbu = T
   coulomb_log = auto
   collide = all
   collide = spec1 spec2 off
   collide = spec2 spec3 0.1
end:collisions
```

With this block, collisions are turned on, the Nanbu-Pérez scattering
algorithm is used and the Coulomb logarithm is automatically calculated.
All values of the frequency array are set to one except (spec1,spec2) is
set to minus one (and also (spec2,spec1)) and (spec2,spec3) is set to
0.1

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
[Input_deck_species__ionisation]: /documentation/input_deck/input_deck_species/#ionisation
