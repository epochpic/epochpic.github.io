---
draft: false
toc: true
type: docs

title: species block
linktitle: species block
weight: 110
menu:
  documentation:
    parent: Input deck
    weight: 40
---

This block contains information about the species of particles which are
used in the code. Also details of how these are initialised. See [EPOCH
input deck][Input_deck] for more information on the
input deck.

# Basics

The next section of the input deck describes the particle species used
in the code. An example species block for any EPOCH code is given below.

```perl
begin:species
   name = Electron
   charge = -1.0
   mass = 1.0
   frac = 0.5
   # npart = 2000 * 100
   number_density = 1.e4
   temp = 1e6

   temp_x = 0.0
   temp_y = temp_x(Electron)
   number_density_min = 0.1 * den_max
   number_density = if(abs(x) lt thick, den_max, 0.0)
   number_density = if((x gt -thick) and (abs(y) gt 2e-6), \
                        0.0, number_density(Carbon))
end:species

begin:species
   name = Carbon
   charge = 4.0
   mass = 1836.0*12
   frac = 0.5

   number_density = 0.25*number_density(Electron)
   temp_x = temp_x(Electron)
   temp_y = temp_x(Electron)

   dumpmask = full
end:species
```

Each species block accepts the following parameters:
- `name` - This specifies the name of the particle species
defined in the current block. This name can include any alphanumeric
characters in the basic ASCII set. The name is used to identify the
species in any consequent input block and is also used for labelling
species data in any output dumps. It is a mandatory parameter.
**`NOTE: IT IS IMPOSSIBLE TO SET TWO SPECIES WITH THE SAME
NAME!`
**

-   `charge` - This sets the charge of the species in
    multiples of the electron charge. Negative numbers are used for
    negatively charged particles. This is a mandatory parameter.
-   `mass` - This sets the mass of the species in multiples
    of the electron mass. Cannot be negative. This is a mandatory
    parameter.
-   `npart` - This specifies the number of pseudoparticles
    which should be loaded into the simulation domain for this species
    block. Using this parameter is the most convenient way of loading
    particles for simulations which contain multiple species with
    different number densities. If *npart* is specified in a species
    block then any value given for *npart* in the
    [*control*][Input_deck_control] block is ignored.
    *npart* should not be specified at the same time as *frac* within a
    *species* block.
-   `frac` - This specifies what fraction of *npart* (the
    global number of particles specified in the control block) should be
    assigned to the species.

**`NOTE: frac should not be specified at the same time as npart for
a given species.`
**

-   `npart_per_cell` - Integer parameter which specifies
    the number of particles per cell to use for the initial particle
    loading. At a later stage this may be extended to allow
    "npart_per_cell" to be a spatially varying function.

If per-species weighting is used then the value of "npart_per_cell"
will be the average number of particles per cell. If "npart" or "frac"
have also been specified for a species, then they will be ignored.

To avoid confusion, there is no globally used "npart_per_species". If
you want to have a single value to change in the input deck then this
can be achieved using a
[*constant*][Input_deck_constant] block.
- `dumpmask` - Determines which output dumps will include
this particle species. The dumpmask has the same semantics as those used
by variables in the [*output*][Input_deck_output_block]
block. The actual dumpmask from the output block is applied first and
then this one is applied afterwards. For example, if the species block
contains "dumpmask = full" and the output block contains "vx = always"
then the particle velocity will be only be dumped at full dumps for this
particle species. The default dumpmask is "always".
- `dump` - This logical flag is provided for backwards
compatibility. If set to "F" it has the same meaning as "dumpmask =
never". If set to "T" it has the same meaning as "dumpmask = always".
- `zero_current` - Logical flag switching the particle
species into zero-current particles. Zero-current particles are enabled
if the if the "NO_TRACER_PARTICLES" precompiler option has not been
used and the "zero_current" flag is set to true for a given species.
When set, the species will move correctly for its charge and mass, but
contribute no current. This means that these particles are passive
elements in the simulation. In all other respects they are designed to
behave identically to ordinary particles, so they do take part in
collisions by default. This can be prevented using the [collision
matrices][Input_deck_collisions].
<span style="color: red; font-weight: bold;">WARNING:</span> Since the
particles effectively have zero weight in terms of their numerical
heating properties, they do not always behave in the same way that an
ordinary particle with weight would behave and this can sometimes lead
to unexpected behaviour. If the purpose is merely to track a subset of a
particle species to use as output then a better mechanism to use is
"persistent subsets" (see [here][Input_deck_subset]).
"tracer" is currently accepted as an alias but this will be removed in
version 5.0. "zero_current = F" is the default value.
- `identify` - Used to identify the type of particle.
Originally this was used for the QED routines, but it has since been adopted for other physics packages too. See
[here][Input_deck_qed] for details.
- `immobile` - Logical flag. If this parameter is set to "T"
then the species will be ignored during the particle push. The default
value is "F".
- `background_species` - Logical flag. If set to "T" the species will
be treated as a non evolving continuum background. No particles are loaded. Any
particle-like specifications will be ignored. Background species are currently
only used by the bremsstrahlung radiation model. 
See [here][Input_deck_bremsstrahlung]
for details. Default value is "F". "background" is accepted as an alias.

The species blocks are also used for specifying initial conditions for
the particle species. The initial conditions in EPOCH can be specified
in various ways, but the easiest way is to specify the initial
conditions in the input deck file. This allows any initial condition
which can be specified everywhere in space by a number density and a
drifting Maxwellian distribution function. These are built up using the
normal maths expressions, by setting the density and temperature for
each species which is then used by the autoloader to actually position
the particles.

The elements of the species block used for setting initial conditions
are:
- `number_density` - Particle number density in $m^{-3}$. As
soon as a number_density= line has been read, the values are calculated
for the whole domain and are available for reuse on the right hand side
of an expression. This is seen in the above example in the first two
lines for the Electron species, where the number density is first set
and then corrected. If you wish to specify the number density in parts
per cubic metre then you can divide by the "cc" constant (see
[here][Maths_parser__constants]). This parameter is
mandatory. "density" is accepted as an alias.
- `number_density_min` - Minimum particle number density in
$m^{-3}$. When the number density in a cell falls below
number_density_min the autoloader does not load any pseudoparticles
into that cell to minimise the number of low weight, unimportant
particles. If set to 0 then all cells are loaded with particles. This is
the default. "density_min" is accepted as an alias.
- `number_density_max` - Maximum particle number density in
$m^{-3}$. When the number density in a cell rises above
number_density_max the autoloader clips the number_density to
number_density_max allowing easy implementation of exponential rises
to plateaus. If it is a negative value then no clipping is performed.
This is the default. "density_max" is accepted as an alias.
- `mass_density` - Particle mass density in $kg\,m^{-3}$.
The same as "number_density" but multiplied by the particle mass. If
you wish to use units of $g\,cm^{-3}$ then append the appropriate
multiplication factor. For example: "`mass_density = 2 * 1e3 / cc`".
- `temp_{x,y,z}` - The temperature in each direction for a
thermal distribution in Kelvin.
- `temp` - Sets an isotropic temperature distribution in
Kelvin. If both temp and a specific temp_x, temp_y, temp_z parameter
is specified then the last to appear in the deck has precedence. If
neither are given then the species will have a default temperature of
zero Kelvin.
- `temp_{x,y,z}_ev, temp_ev` - These are the same as the
temperature parameters described above except the units are given in
electronvolts rather than Kelvin, i.e. using 1ev = 11604.5K .
- `drift_{x,y,z}` - Specifies a momentum space offset in
$kg\ ms^{-1}$ to the distribution function for this species. By default,
the drift is zero.
- `offset` - File offset. See below for details.

# Loading data from a file {#loading_data_from_a_file}

It is also possible to set initial conditions for a particle species
using an external file. Instead of specifying the initial conditions
mathematically in the input deck, you specify in quotation marks the
filename of a simple binary file containing the information required.
For more information on what is meant by a "simple binary file", see
[here][Binary_files].

```perl
begin:species
   name = Electron
   number_density = 'Data/ic.dat'
   offset = 80000
   temp_x = 'Data/ic.dat'
end:species
```

The sizes of the variables to be filled do not need to be provided: the
code will continue reading until the given variable is filled. Note that
ghost or guard cells should not be included in the file as they cannot
be set this way.

An additional element is also introduced, the offset element. This is
the offset in bytes from the start of the file to where the data should
be read from. As a given line in the block executes, the file is opened,
the file handle is moved to the point specified by the offset parameter,
the data is read and the file is then closed. Therefore, unless the
offset value is changed between data reading lines the same data will be
read into all the variables. The data is read in as soon as a line is
executed, and so it is perfectly possible to load data from a file and
then modify the data using a mathematical expression.
The example block above is for 10,000 values at double precision, i.e.
8-bytes each. The density data is the first 80,000 bytes of "ic.dat".
Bytes 80,000 to 160,000 are the temp_x data.

The file should be a simple binary file consisting of floating point
numbers of the same precision as **_num** in the core EPOCH code. For
multidimensional arrays, the data is assumed to be written according to
FORTRAN array ordering rules (i.e. column-major order).
**`NOTE: The files that are expected by this block are SIMPLE
BINARY files, NOT FORTRAN unformatted files. It is possible to read
FORTRAN unformatted files using the offset element, but care must be
taken!`**

# Delta-f parameters {#delta_f_parameters}

The following entries are used for configuring the [Delta-f
method][Using_delta_f]

-  `number_density_back`
-  `drift_{x,y,z}_back`
-  `temp_{x,y,z}_back`
-  `temp_{x,y,z}_back_ev`
-  `temp_back`
-  `temp_back_ev`

These all have the same meanings as the parameters listed above that
don't include the "_back" text, except that they specify the values to
use for the background distribution function.

# Particle migration between species {#particle_migration_between_species}

It is sometimes useful to separate particle species into separate energy
bands and to migrate particles between species when they become more or
less energetic. A method to achieve this functionality has been
implemented. It is specified using two parameters to the "control"
block:
- `use_migration` - Logical flag which determines whether or
not to use particle migration. The default is "F".
- `migration_interval` - The number of timesteps between
each migration event. The default is 1 (migrate at every timestep).
The following parameters are added to the "species" block:
- `migrate` - Logical flag which determines whether or not to
consider this species for migration. The default is "F".
- `promote_to` - The name of the species to promote
particles to.
- `demote_to` - The name of the species to demote particles
to.
- `promote_multiplier` - The particle is promoted when its
energy is greater than "promote_multiplier" times the local average.
The default value is 1.
- `demote_multiplier` - The particle is demoted when its
energy is less than "demote_multiplier" times the local average. The
default value is 1.
- `promote_number_density` - The particle is only
considered for promotion when the local number density is less than
"promote_number_density". The default value is the largest floating
point number.
- `demote_number_density` - The particle is only considered
for demotion when the local number density is greater than
"demote_number_density". The default value is 0.

# Ionisation

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
A full summary of the keys used in ionisation has been provided below:

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

When collisional ionisation is switched on, ionisation between all electron species and all species which may be ionised is considered - the `collide` parameter used in the collisions block has no effect on collisional ionisation. Species which may be ionised include any species with `ionise=T` set, and the ionised variants of this species up to the fully ionised state, or `ionise_limit`. For electrons, EPOCH will identify any species set as a destination for ejected electrons as an electron species, which can trigger further collisional ionisation. To mark other species as electrons for collisional ionisaiton, the `identify` key must be used. All electron aliases may be used for `identify`, including electrons created from pair production.

# Species Boundary Conditions {#species_boundary_conditions}

-   `bc_x_min` - Boundary condition to be applied to this
    species only on the lower x boundary. Can be any normal boundary
    condition apart from periodic. If not specified then the global
    boundary condition is applied.
-   `bc_x_max` - Boundary condition to be applied to this
    species only on the upper x boundary. Can be any normal boundary
    condition apart from periodic. If not specified then the global
    boundary condition is applied.
-   `bc_y_min` - Boundary condition to be applied to this
    species only on the lower y boundary. Can be any normal boundary
    condition apart from periodic. If not specified then the global
    boundary condition is applied.
-   `bc_y_max` - Boundary condition to be applied to this
    species only on the upper y boundary. Can be any normal boundary
    condition apart from periodic. If not specified then the global
    boundary condition is applied.
-   `bc_z_min` - Boundary condition to be applied to this
    species only on the lower z boundary. Can be any normal boundary
    condition apart from periodic. If not specified then the global
    boundary condition is applied.
-   `bc_z_max` - Boundary condition to be applied to this
    species only on the upper z boundary. Can be any normal boundary
    condition apart from periodic. If not specified then the global
    boundary condition is applied.
-   `meet_injectors` - Logical flag determining whether the
    background plasma should be extended to meet the point where
    particle injectors operate from. This means that plasma is loaded
    one particle shape function length outside the boundary. This means
    that it is possible to use an injector to "continue" an existing
    drifting plasma. NOT COMPATIBLE WITH PERIODIC BOUNDARY CONDITIONS!

# Maxwell Juttner distributions {#maxwell_juttner_distributions}

As of version 4.15, EPOCH allows the user to request a Maxwell-Jüttner
distribution rather than a Maxwellian distribution when sampling the
particle momentum for a species.

This feature does not at present work with the delta_f loader and is
not available for particle injectors. It does work correctly with the
moving window.

-   `use_maxwell_juttner` - Logical flag determining
    whether to sample from the Maxwell-Jüttner distribution when loading
    the particle species. If "T" then Maxwell-Jüttner is used and if
    "F" Maxwellian is used. The default value is "F".

-   `fractional_tail_cutoff` - The sampling is carried out using a
    rejection method with an arbitrary cut-off. This parameter takes a
    floating-point argument which specifies the fraction of maximum
    value at which the sampling should be cut off. Smaller values lead
    to distortion nearer the peak of the distribution but are faster to
    sample. Larger values lead to a better approximation of the
    distribution function but are slower to sample. The default value is
    0.0001.

If drifts are specified with the Maxwell-Jüttner distribution then the
distribution is calculated in the rest frame and then Lorentz
transformed to the specified drifting frame.

# Arbitrary Distribution functions {#arbitrary_distribution_functions}

As of version 4.15, EPOCH also allows the user to request an arbitrary
non-Maxwellian distribution function to use when sampling the particle
momentum for a species. If combined with a specified drift then the
distribution function is calculated first and the drift is applied to
the resulting particles by Lorentz transform.

This feature does not at present work with the delta_f loader and is
not available for particle injectors. It does work correctly with the
moving window.

-   `dist_fn` - Specifies the functional form of the
    distribution function, normalised to have a maximum value of 1. The
    variables "px", "py" and "pz" should be used to parameterise
    the x, y and z components of momentum. This may freely vary in space
    but temporal variation will be ignored since this is only evaluated
    at the start of the simulation.

<!-- -->

-   `dist_fn_p{x,y,z}_range` - Comma separated pair of
    numbers to specify the range of momentum for p_{x,y,z} in SI units.
    Should be of the form "<lower_range>, <upper_range>"

If a range for a momentum direction is not specified then that momentum
is assumed to be zero. It is up to the user to ensure that the range is
large enough to correctly capture their desired distribution function.
Sampling is by a simple rejection sampling and may be much slower than
the existing Maxwellian sampler. EPOCH will print a warning if a large
number of samples are needed to complete the sampling. If this occurs
then you might need to reduce the range of momentum over which sampling
is considered.

If the "dist_fn" key is supplied then any supplied temperature keys
are ignored. An example of setting up a truncated power law distribution
in px would be

```perl
begin:constant
  dens = 10
  v0 = 0.05 * c
  vmax = 0.5 * c
  p0 = v0 * me * (1.0 + 4.0 * x/x_max)
  pmax = vmax * me
  alpha = -2.0
end:constant

begin:species
  name = Electron_pl
  charge = -1
  mass = 1.0
  frac = 0.5
  number_density = dens
  #Truncated power law distribution in px
  dist_fn = exp(-p0/px) * (px/p0)^(alpha)
  dist_fn_px_range = (0, pmax)
end:species
```



<!-- ########################  Cross references  ######################## -->


[Binary_files]: /documentation/input_deck/binary_files
[Input_deck]: /documentation/input_deck/input_deck
[Input_deck_bremsstrahlung]: /documentation/input_deck/input_deck_bremsstrahlung
[Input_deck_collisions]: /documentation/input_deck/input_deck_collisions
[Input_deck_constant]: /documentation/input_deck/input_deck_constant
[Input_deck_control]: /documentation/input_deck/input_deck_control
[Input_deck_output_block]: /documentation/input_deck/input_deck_output_block
[Input_deck_qed]: /documentation/input_deck/input_deck_qed
[Input_deck_subset]: /documentation/input_deck/input_deck_subset
[Maths_parser__constants]: /documentation/code_details/maths_parser#constants
[Using_delta_f]: /documentation/code_details/using_delta_f
