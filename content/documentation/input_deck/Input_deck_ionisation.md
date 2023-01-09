---
draft: false
toc: true
type: docs

title: ionisation
linktitle: ionisation
weight: 90
menu:
  documentation:
    parent: Input deck
    weight: 20
---

EPOCH now includes both field and collisional ionisation, which can be 
activated by switching on keys in different blocks. Previous versions of
EPOCH forced the user to specify ionisation energies for each ion charge
state, but since EPOCH 4.19, these are set automatically using look-up tables.

Field and collisional ionisation must be switched on in the control block and 
collision block respectively, and species which are to be ionised must be 
specified in their species block. A basic example of using both ionisation 
mechanisms is given below, where non-relevant lines have been omitted.

```perl
begin:control
   use_multi_photon = T
   use_bsi = F
   field_ionisation = T
end:control

begin:collisions
   use_collisional_ionisation = T 
   ci_n_step = 3
end:collisions

begin:species
  name = Carbon
  charge = 0
  atomic_no = 6
  ionise = T
  ionise_limit = 3
  unique_electron_species = T
end:species   

begin:species
  name = Carbon4
  charge = 4
  atomic_no = 6
  ionise = T
  ionisation_electron_species = (Electron4, Electron)
end:species
```
Descriptions of these flags can be found in the documentation for each block, 
but a summary has been provided here for clarity:

-   `field_ionisation` - Switches on field ionisation.
  
-   `use_collisional_ionisation` - Switches on ionisation by collisional
    electron impact.
  
-   `ci_n_step` - Only performs the collisional ionisation calculation
    once every _n_ steps, where _n_ is set by this parameter. This is done 
    to speed up the code, and the default is 1 (every step). When
    this is greater than 1, the assumed time-step for the collisional ionisation
    calculation is _n*dt_. Note that an ion may only be ionised once per calculation,
    so if _n_ is too high, the number of ions will be underestimated.

-   `atomic_no` - Atomic number of the element. When combined with the
    charge, the code can deduce the element and charge-state of the ion, and 
    may use the appropriate ionisation energy and shell binding energies.
  
-   `ionise` - Allows ionisation of this species, and generates additional
    particle species for each ion charge state.

-  `ionise_limit` - This limits the number of additional particle species to
    be generated. In this example, ion macro-particles in the Carbon species can only
    be ionised 3 times - ionisation of Carbon3 will not be considered.

-   `ionisation_electron_species` - Name of the electron species to 
    populate with ejected electrons. This
    can be specified as an array in the event that the user wishes some levels
    to have a different electron species which can be handy for monitoring
    ionisation at specific levels. `electron` and `electron_species` are
    accepted as synonyms. Either one species for **all** ionisation levels, or one species
    for **each** level should be specified. In the Carbon4 example, the user may have
    written `ionisation_electron_species = Electron` to use the Electron species 
    for all ejected electrons.
  
-  `unique_electron_species` - If "T", this generates a
    unique electron species to populate with ejected electrons from each
    ion charge state. The user must use this, or **ionisation_electron_species**.

Ionised states are created automatically and are named according to the
ionising species name with a number appended. For example, with the Carbon
species block, the species named "Carbon1", "Carbon2" and "Carbon3"
are automatically created. Note that for pre-ionised species like the Carbon4 block,
species would be named "Carbon41", "Carbon42". These species will also 
inherit the ``dump''
parameter from their parent species. This behaviour can be overridden by explicitly
adding a species block of the same name with a differing dumpmask.

Field ionisation consists of three distinct regimes; multiphoton in which
ionisation is best described as absorption of multiple photons, tunnelling
in which deformation of the atomic Coulomb potential is the dominant factor,
and barrier suppression ionisation in which the electric field is strong
enough for an electron to escape classically. It is possible to turn off
multiphoton or barrier suppression ionisation through the input deck
using the following control block parameters:

-   `use_multiphoton` - Logical flag which turns on modelling
    ionisation by multiple photon absorption. This should be set to "F" if
    there is no laser attached to a boundary as it relies on laser frequency.
    The default is "T".

-   `use_bsi` - Logical flag which turns on barrier suppression
    ionisation correction to the tunnelling ionisation model for high intensity
    lasers. The default is "T".
