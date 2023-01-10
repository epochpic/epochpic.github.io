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

This block contains information about bremsstrahlung radiation. See [EPOCH
input deck][Input_deck] for more information on the
input deck.

EPOCH is capable of simulating bremsstrahlung radiation using the teqhniques described by Morris _et al_[^1] and Vyskočil _et al_[^2]. 
In order to run this module, the compiler flag **`-DBREMSSTRAHLUNG`** must be
switched on in the Makefile. There should also be a corresponding
"bremsstrahlung" block for the input deck which uses a similar
format to the "qed" block. Bremsstrahlung cross sections may be calculated
for both electrons and positrons. Photons may also undergo pair production
according to the Bethe-Heitler model.

An example bremsstrahlung block is shown below:

```perl
begin:bremsstrahlung
   enable = T
   start_time = 0
   produce_photons = T
   photon_energy_min = 1 * kev
   photon_weight = 1.0
   photon_dynamics = F
   use_plasma_screening = F
   use_brem_scatter = T
   use_bethe_heitler = F
   use_positron_brem = F
end:bremsstrahlung
```

- `enable` - Logical flag to turn bremsstrahlung on or off.
  "use_bremsstrahlung" is accepted as an alias. The default is "F".

- `start_time` - Floating point value specifying the time after
  which bremsstrahlung radiation is modelled. The default is 0.

- `produce_photons` - Logical flag to allow population of a photon
  species. If "F", then only the energy loss of the electrons will be
  simulated. The photon species can be specified by including the line
  `identify:brem_photon` in the corresponding species block. If the
  compiler flag **`-DPHOTONS`** is active, QED and bremsstrahlung will both
  populate the first species with `identify:photon` if no
  _brem\_photon_ species is specified. The default is "F".

- `photon_energy_min` - Floating point value specifying the minimum
  energy of produced photons. Electron energy loss is still calculated for
  lower energy photon emissions, but these photons are not added to the photon
  species. The default is 0.

- `photon_weight` - Floating point value which applies a multiplier to
  the weight of produced macro-photons, in order to increase the number
  of overall emissions and obtain better spectra. Must be less than or equal
  to 1 and greater than 0. For example, 0.1 would make emission 10 times more
  likely, but for macro-photons only 10% the weight of the generating
  macro-electrons. Electron recoil would be reduced accordingly. The default is 1. Note that only one emission is possible
  per macro-electron per timestep, so setting this too low will saturate
  emissions.

- `photon_dynamics` - Logical flag to specify whether or not to push
  photons. If "F", then the generated photons are immobilised at the point of
  emission. The default is "F".

- `use_plasma_screening` - Logical flag to specify whether a cross
  section enhancement due to heated ionised targets is considered, based on
  theory described by Wu _et al_[^3]. It is expected that for high energy
  electrons passing through low density, ionised plasmas with electron
  temperatures over $\sim$100 eV ($\sim8\times 10^{5}$ K), the bremsstrahlung
  emission rate could increase by a factor of 2-3. This has not been tested
  experimentally, and so the default value is set to "F".

- `use_radiation_reaction` - Logical flag to specify whether the
  electrons experience energy loss when emitting photons or not.
  "use_bremsstrahlung_recoil" is accepted as an alias. Debugging
  flag, default "T".

- `table_location` - String specifying the
  location of the emission look-up tables for bremsstrahlung.
  The default path is set to
  `src/physics_packages/TABLES/br`.
  
- `use_brem_scatter` - Samples photon ejection angle from a 
  differential cross section. Default is "F", where photons are emitted in the
  direction of the incident particle (ultra-relativistic approximation).
  
- `use_bethe_heitler` - Allows photons to undergo Bethe-Heitler pair
  production. Default is "F". This requires both electron and positron species
  to be defined.
  
- `use_positron_brem` - Samples bremsstrahlung radiation from positrons. 
  Default is "F". If "T", electrons and positrons share the same 
  parameter values set in the bremsstrahlung block.

Bremsstrahlung, like QED, requires the code to know which species are electrons
and which are photons, so uses the same identify system (with
`identify:brem_photon` for a bremsstrahlung-only photon species).
Additionally, the atomic numbers of the atom/ion species are required in
the species block. For example, atomic aluminium (charge = 0) could be
specified as:

```perl
begin:species
   name = Aluminium
   atomic_number = 13
   charge = 0

   mass = 49218
   number_density = 6.022e28
   fraction = 0.5
   dump = T
end:species
```

If the atomic number is not specified then it will be assumed that the ion is
fully ionised and the atomic number would be set to the charge (the nearest
integer to the ion charge when expressed in units of elementary charge). If
ionisation is considered, the atomic number must be specified once, and all child species will retain the same atomic number.

If Bethe-Heitler pair production is considered, the user may identify specific
species to populate with Bethe-Heitler electrons and positrons using the identity
aliases:

- `identify:bethe_heitler_electron` - bh\_electron is also accepted.

- `identify:bethe_heitler_positron` - bh\_positron is also accepted.

If these species are unspecified, EPOCH will populate the first electron and 
positron species present read in from the input deck. Additional identity aliases are provided in the [QED][input_deck_qed] section.

# References

<references />

[^1] Morris, S., Robinson, A., & Ridgers, C. (2021). Highly efficient conversion of laser energy to hard x-rays in high-intensity laser–solid simulations. Physics of Plasmas, 28(10), 103304.
[1](https://aip.scitation.org/doi/full/10.1063/5.0055398)

[^2] J. Vyskočil, O. Klimo, and S. Weber, “Simulations of bremsstrahlung emission in ultra-intense laser
interactions with foil targets,” Plasma Physics and Controlled Fusion, vol. 60, no. 5, p. 054013, 2018.
[2](https://iopscience.iop.org/article/10.1088/1361-6587/aab4c3/meta)

[^3] Wu, D., He, X. T., Yu, W., & Fritzsche, S. (2018). Particle-in-cell simulations of laser–plasma interactions at solid densities and relativistic intensities: the role of atomic processes. High Power Laser Science and Engineering, 6.
[3](https://www.cambridge.org/core/journals/high-power-laser-science-and-engineering/article/particleincell-simulations-of-laserplasma-interactions-at-solid-densities-and-relativistic-intensities-the-role-of-atomic-processes/82560EBD1E5A4869CC5EC059C47A017A)

<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
[Input_deck_qed]: /documentation/input_deck/input_deck_qed
