+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "Workshop examples continued"
[menu.tutorial]
  parent = "Content"
  weight = 600
+++

# Other Laser-Plasma example decks {#other_laser_plasma_example_decks}

Now that you have a basic understanding of how the input decks work, you
should be able to work through the remaining example decks by referring
to the [User manual][Landing_Page] for a description of
any new parameters used. Several experienced users of the code will be
available throughout the duration of the workshop, so if you want help
with anything please don't hesitate to ask. The decks are:

-   01-1d_laser.deck Described in notes above
    [(here)][Workshop_examples__a_basic_em-field_simulation]
-   02-2d_laser.deck Described in notes above
    [(here)][Workshop_examples__a_2d_laser]
-   03-1d_two_stream.deck Described in notes above
    [(here)][Workshop_examples__specifying_particle_species]
-   04-1d_two_stream_io.deck This is the same as the previous deck
    but with the addition of more sophisticated output diagnostics
-   05-2d_moving_window.deck This deck contains an example of firing a
    laser into a plasma and then using the moving window facility to
    track the wave front as it moves beyond the edge of the original
    domain.
-   06-2d_ramp.deck This deck contains an example of firing a laser at
    a plasma with a ramped density profile.
-   07-1d_heating.deck This deck contains a setup for investigating the
    anomalous heating of a plasma that occurs for purely resolved
    systems.

### Other things to try {#other_things_to_try}

1.  Landau damping predicts collisionless damping of electrostatic
    waves. Setup a 1D problem with an electrostatic wave and check for a
    range of wavelengths. Points to note:
    1.  Does the answer depend on whether the initial condition is a
        travelling wave or standing wave? How are these setup?
    2.  Look for trapping in the Langmuir wave
    3.  Check the damping rate against published formulae. Try for a
        range of $k\lambda_D$ as the most commonly reported formulae
        assume $kλ_D≪1$
    4.  The answer is more accurate, assuming you have enough
        grid-points and particles to get a good answer, if you ignore
        the first maxima or two -- why?
2.  A more realistic instability than the two cold beams tested above is
    the bump-on-tail instability. Setup a 1D bump-on-tail distribution
    and check that the simple formula for the growth-rates is correctly
    reproduced. The main problem with the initial conditions is how to
    setup a suitable initial distribution.
3.  Try setting up the initial conditions for a problem of direct
    relevance to your research. This may be too computationally
    demanding to run on the workshop computers but it is a good exercise
    as you can get some help on trickier input decks and diagnostic
    planning than the simple exercises so far.
4.  Check that EPOCH works as expected on your host institution
    computer. If not we may be able to help before you leave.

# Copies of the decks {#copies_of_the_decks}

The decks can be downloaded [
here][Workshop_examples__getting_the_example_decks_for_this_workshop]
and viewed or copied from here:\
**04-1d_two_stream_io.deck**

<div class="mw-collapsible mw-collapsed" style="border-style: solid; border-width: 2px; border-color:#0059A1">

Click "Expand" to view or copy the deck code:

<div class="mw-collapsible-content">

```perl
begin:control
  nx = 400

  # Size of domain
  x_min = 0
  x_max = 5.0e5

  # Final time of simulation
  t_end = 1.5e-1

  stdout_frequency = 400
end:control


begin:boundaries
  bc_x_min = periodic
  bc_x_max = periodic
end:boundaries


begin:constant
  drift_p = 2.5e-24
  temp = 273
  dens = 10
end:constant


begin:species
  # Rightwards travelling electrons
  name = Right
  charge = -1
  mass = 1.0
  temp = temp
  drift_x = drift_p
  number_density = dens
  npart = 4 * nx
end:species


begin:species
  # Leftwards travelling electrons
  name = Left
  charge = -1
  mass = 1.0
  temp = temp
  drift_x = -drift_p
  number_density = dens
  npart = 4 * nx
end:species


begin:output
  name = normal

  # Number of timesteps between output dumps
  dt_snapshot = 1.5e-3

  # Properties at particle positions
  particles = always
  px = always

  # Properties on grid
  grid = always
  ex = always
  ey = always
  ez = always
  bx = always
  by = always
  bz = always
  jx = always
  ekbar = always
  mass_density = never + species
  charge_density = always
  number_density = always + species
  temperature = always + species

  distribution_functions = always
end:output


begin:output
  name = restart

  # Number of timesteps between output dumps
  dt_snapshot = 0.15

  restartable = T
end:output


begin:dist_fn
  name = x_px
  ndims = 2

  direction1 = dir_x
  direction2 = dir_px

  # Range is ignored for spatial coordinates
  range1 = (1, 1)
  range2 = (-5e-24, 5e-24)

  # Resolution is ignored for spatial coordinates
  resolution1 = 1
  resolution2 = 200

  include_species:Left
  include_species:Right
end:dist_fn
```

</div>
</div>

**05-2d_moving_window.deck**

<div class="mw-collapsible mw-collapsed" style="border-style: solid; border-width: 2px; border-color:#0059A1">

Click "Expand" to view or copy the deck code:

<div class="mw-collapsible-content">

```perl
begin:constant
  x0 = 20 * micron
  lambda = 10 * micron

  t_laser = 120 * femto
  sigma_t = t_laser / 2 / sqrt(loge(2))

  w0_laser = 30 * micron
  sigma_w0 = w0_laser / 2 / sqrt(loge(2))

  den_peak = 5.0e19 * 1.0e6
  win_start = 340 * femto
end:constant


begin:control
  nx = 1550 / 8
  ny = 600 / 8
  npart = (60e6) / 8

  # Size of domain
  x_min = 0
  x_max = 155 * micron
  y_min = -30 * micron
  y_max = -y_min

  # Final time of simulation
  t_end = 1600 * femto

  stdout_frequency = 1
  print_eta_string = T
end:control


begin:boundaries
  bc_x_min = simple_laser
  bc_x_max = simple_outflow
  bc_y_min = simple_outflow
  bc_y_max = simple_outflow
end:boundaries


begin:species
  name = electron
  charge = -1.0
  mass = 1.0
  number_density = if((x lt x0), 0.0, den_peak)
  frac = 0.5
end:species


begin:species
  name = proton
  charge = 1.0
  mass = 1836.2
  number_density = number_density(electron)
  frac = 0.5
end:species


begin:output
  name = normal
  dt_snapshot = 50 * femto

  grid = always
  ex = always
  ey = always
  ez = always
  bx = always
  by = always
  bz = always
  jx = always
  jy = always
  ekbar = always
  mass_density = never + species
  charge_density = always
  number_density = always + species
  temperature = always + species
end:output


begin:output
  name = large
  dt_snapshot = 500 * femto

  particles = always
  particle_weight = always
end:output


begin:laser
  boundary = x_min
  intensity_w_cm2 = 1.9e18
  lambda = lambda
  t_profile = gauss(time, 2*sigma_t, sigma_t)
  profile = gauss(y, 0, sigma_w0)
end:laser


begin:window
  move_window = T
  window_v_x = c * 0.87
  window_start_time = win_start
  bc_x_min_after_move = simple_outflow
  bc_x_max_after_move = simple_outflow
end:window
```

</div>
</div>

**06-2d_ramp.deck**

<div class="mw-collapsible mw-collapsed" style="border-style: solid; border-width: 2px; border-color:#0059A1">

Click "Expand" to view or copy the deck code:

<div class="mw-collapsible-content">

```perl
begin:constant
  # Particles per cell
  part = 32

  las_lambda = 1 * micron
  las_omega = 2.0 * pi * c / las_lambda
  las_time = 2.0 * pi / las_omega

  n_crit = critical(las_omega)

  max_dens = 0.8 * n_crit

  scale_x = 20 * micron

  las_scale_y = 8 * micron

  xmin = -4 * micron

  # Gaussian Beam stuff
  w0 = las_scale_y
  rayleigh_range = pi * w0^2 / las_lambda
  wz = w0 * sqrt(1 + (x_start / rayleigh_range)^2)
  radius_of_curvature = x_start * (1.0 + (rayleigh_range / x_start)^2)
end:constant


begin:control
  nx = 1024 / 4
  ny = 512 / 4

  # Final time of simulation
  t_end = 0.4 * pico

  # Size of domain
  x_min = xmin
  x_end = scale_x + 20 * micron

  y_min = -20 * micron
  y_max = -y_min

  stdout_frequency = 10
end:control


begin:laser
  boundary = x_min
  intensity_w_cm2 = 1.0e16
  omega = las_omega
  t_profile = if (time lt 2*las_time, gauss(time, 2*las_time, 2*las_time), 1)
  profile = (1.0 + 0.05 * sin(32.0*pi*y/lengthy)) * gauss(y, 0, las_scale_y)
end:laser


begin:boundaries
  bc_x_min = simple_laser
  bc_x_max = simple_outflow
  bc_y_min = periodic
  bc_y_max = periodic
end:boundaries


begin:species
  # Electron
  name = electron
  charge = -1.0
  mass = 1.0
  npart = nx * ny * part

  number_density = max_dens * (exp(x/scale_x) - 1) / (exp(1) - 1)
  number_density = if(x lt 0, 0.0, number_density(electron))
  number_density = if(number_density(electron) gt max_dens, max_dens, \
                      number_density(electron))
  number_density = if(x gt 75*micron, 0.0, number_density(electron))
  #number_density = number_density(electron) \
                    * (0.8 + 0.2 * gauss(y, 0, 0.5*las_scale_y))
  number_density_min = 0.0001 * n_crit
  number_density_max = n_crit

  temp_ev = 10^3
end:species


begin:species
  # Protons
  name = proton
  charge = 1.0
  mass = 1836.2
  npart = nx * ny * part

  number_density = number_density(electron)
  number_density_min = 0.0001 * n_crit
  number_density_max = 1.2 * n_crit

  temp_ev = 40
end:species


begin:output
  name = normal

  # Number of timesteps between output dumps
  dt_snapshot = 5 * femto

  # Properties at particle positions
  particles = always
  px = always
  particle_weight = always

  # Properties on grid
  grid = always
  ex = always
  ey = always
  ez = always
  bx = always
  by = always
  bz = always
  jx = always
  jy = always
  jz = always
  ekbar = always + species
  mass_density = never + species
  charge_density = always # + average + snapshot
  number_density = always + species
  temperature = never + species

  # Extended io
  distribution_functions = always
end:output


begin:dist_fn
  name = en
  ndims = 1

  direction1 = dir_en

  range1 = (0, 15*kev)

  resolution1 = 5000

  include_species:electron
end:dist_fn


begin:dist_fn
  name = x_en
  ndims = 2

  direction1 = dir_x
  direction2 = dir_en

  # Range is ignored for spatial coordinates
  #range1 = (1, 1)
  range2 = (0, 15*kev)

  # Resolution is ignored for spatial coordinates
  #resolution1 = 1
  resolution2 = 1500

  include_species:electron
end:dist_fn


begin:dist_fn
  name = x_px
  ndims = 2

  direction1 = dir_x
  direction2 = dir_px

  # Range is ignored for spatial coordinates
  #range1 = (1, 1)
  range2 = (-5e-23, 5e-23)

  # Resolution is ignored for spatial coordinates
  #resolution1 = 1
  resolution2 = 1500

  include_species:electron
end:dist_fn


begin:probe
  name = electron_probe

  point = (0.5 * (x_max + x_min), y_min)
  normal = (1, 0)

  include_species:electron
  include_species:proton
end:probe
```

</div>
</div>

**07-1d_heating.deck**

<div class="mw-collapsible mw-collapsed" style="border-style: solid; border-width: 2px; border-color:#0059A1">

Click "Expand" to view or copy the deck code:

<div class="mw-collapsible-content">

```perl
begin:constant
  dl = 74.33942 * micron
end:constant


begin:control
  nx = 10

  # Size of domain
  x_min = 0
  x_max = 14000 * dl

  # Final time of simulation
  t_end = 1.5e-2

  stdout_frequency = 10000
  print_eta_string = T
end:control


begin:boundaries
  bc_x_min = periodic
  bc_x_max = periodic
end:boundaries


begin:species
  name = electron
  charge = -1
  mass = 1.0
  temp_x_ev = 1
  number_density = 1e16
  npart = nx * 5
end:species


begin:output
  name = normal

  # Number of timesteps between output dumps
  dt_snapshot = 1.5e-3

  # Properties on grid
  grid = always
  ekbar = always
  temperature = always
end:output


begin:output
  name = large

  # Number of timesteps between output dumps
  dt_snapshot = 75e-3

  # Properties at particle positions
  particles = always
  px = always
  py = always
  pz = always
end:output
```

</div>
</div>

# Remote Visualisation with VisIt {#remote_visualisation_with_visit}

If the local workstation you are using isn't big enough for your test
problems you may also use a your host institutes HPC cluster.

### Remote Visualisation with VisIt {#remote_visualisation_with_visit_1}

Most large simulations are carried out on a remotely located machine.
Often this machine is located many miles away, perhaps even in a
different country. Viewing data on remote systems can be awkward and
poor network speeds can often make it nearly impossible. The VisIt
visualisation tool solves this problem by using a client-server model.
The program which reads, processes and renders the data is completely
separated from the program which displays the results on the screen. It
is therefore possible to run VisIt on your local machine and look at
data located on a different machine. The method of setting this up
varies depending on the configuration of the remote machine so we will
not go into details here. However, the desktop machines have been setup
to be able to view data located on remote clusters so you can try it
out.

In the VisIt control window, click the "Open" button which launches a
file browser window. The first entry is called "Host" and contains a
drop-down list of all configure remote machines.

If you want to know more about how to set up remote visualisation in
VisIt, you can ask one of the Warwick staff members.

When viewing data across a slow network connection, there is one more
useful thing to know. VisIt has two methods of drawing plots generated
on a remote machine. The first method is to construct the polygons used
in drawing the plot on the remote machine and send them across the
network. The local machine then turns these into a plot image. This
makes manipulating the figure very fast (zooming, rotating, etc), since
all the polygons that generate the image are on the local machine.
However, if there are a lot of polygons then they can be slow to
transfer across the network. They can also use up a lot of memory. For
these cases, the alternative is to render the image on the remote
machine and just transfer the image across the network. The downside of
this approach is that whenever you manipulate the plot, it must be
re-drawn on the remote machine and then transferred across the network
again. The options controlling this behaviour are to be found under
"Options-\>Rendering" in the "Advanced" tab. The feature is called
"scalable rendering".

# Collisions in EPOCH {#collisions_in_epoch}

EPOCH now contains a collision routine based on the technique outlined
in Sentoku & Kemp[^1]

Collisions are enabled using the output block named
[collisions][Input_deck_collisions] which accepts the
following three parameters.

-   use_collisions -- This is a logical flag which determines whether
    or not to call the collision routine. If omitted, the default is
    "true" if any of the frequency factors are non-zero (see below)
    and "false" otherwise.

<!-- -->

-   coulomb_log -- This may either be set to a real value, specifying
    the Coulomb logarithm to use when scattering the particles or to the
    special value "auto". If "auto" is used then the routine will
    calculate a value based on the properties of the two species being
    scattered. If omitted, the default value is "auto".

<!-- -->

-   collide -- This sets up a symmetric square matrix of size
    nspecies\*nspecies containing the collision frequency factors to use
    between particle species. The element (s1,s2) gives the frequency
    factor used when colliding species s1 with species s2. If the factor
    is less than zero, no collisions are performed. If it is equal to
    one, collisions are performed normally. For any value between zero
    and one, the collisions are performed using a frequency multiplied
    by the given factor. If "collide" has a value of "all" then all
    elements of the matrix are set to one. If it has a value of "none"
    then all elements are set to minus one. If the syntax "species1
    species2 <value>" is used, then the (species1,species2) element of
    the matrix is set to the factor "<value>". This may either be a
    real number, or the special value "on" or "off". The "collide"
    parameter may be used multiple times. The default value is "all"
    (ie. all elements of the matrix are set to one).

For example:

```perl
begin:collisions
  use_collisions = T
  coulomb_log = auto
  collide = all
  collide = spec1 spec2 off
  collide = spec2 spec3 0.1
end:collisions
```

With this block, collisions are turned on and the Coulomb logarithm is
automatically calculated. All values of the frequency array are set to
one except (spec1,spec2) is set to minus one (and also (spec2,spec1))
and (spec2,spec3) is set to 0.1

# Ionisation in EPOCH {#ionisation_in_epoch}

EPOCH includes field ionization which can be activated by defining
"field_ionisation = T" in the
[control][Input_deck_control] block along with
ionisation energies and an electron for the ionising species in one of
the [species][Input_deck_species] blocks. This is done
via the species block in the "ionisation_energies" and
"electron_species" parameter respectively. "ionisation_energies"
should be given as a list in joules, and "electron_species" should be
the name of the species to be used as the electron species. For example,
ionising carbon species might appear in the input deck as:

```perl
begin:species
  charge = 0.0
  mass = 1837.2
  name = carbon
  ionisation_energies = \
    (11.26*ev, 24.38*ev, 47.89*ev, 64.49*ev, 392.1*ev, 490.0*ev)
  electron_species = electron
  number_density = den_gas
end:species

begin:species
  charge = -1.0
  mass = 1.0
  name = electron
  number_density = 0.0
end:species
```

It is possible to define different electron species for each ionisation
level, which is particularly useful in monitoring specific ionisation
levels. If we wished to monitor the fourth ionisation level of carbon in
the above example, the above example might appear:

```perl
begin:species
  charge = 0.0
  mass = 1837.2
  name = carbon
  ionisation_energies = \
    (11.26*ev, 24.38*ev, 47.89*ev, 64.49*ev, 392.1*ev, 490.0*ev)
  electron_species = (electron, electron, electron, fourth, electron, electron)
  number_density = den_gas
end:species

begin:species
  charge = -1.0
  mass = 1.0
  name = electron
  number_density = 0.0
end:species

begin:species
  charge = -1.0
  mass = 1.0
  name = fourth
  number_density = 0.0
end:species
```

Field ionisation consists of three distinct regimes; multiphoton in
which ionisation is best described as absorption of multiple photons,
tunneling in which deformation of the atomic coulomb potential is the
dominant factor, and barrier suppression ionisation in which the
electric field is strong enough for an electron to escape classically.
It is possible to turn off multiphoton or barrier suppression ionisation
through the input deck by adding "use_multiphoton=F" and/or
"use_bsi=F" to the control block.

# QED Effects in EPOCH {#qed_effects_in_epoch}

EPOCH has recently been extended to include some quantum electrodynamic
effects that are important for high intensity (\>) lasers. The two
processes that are included are

-   Gamma ray production by QED corrected synchrotron emission (Also
    called magnetic bremsstrahlung or nonlinear Compton scattering).
-   Electron positron pair production by the Breit-Wheeler process from
    these gamma ray photons.

For more information on the theory see Duclous et al. [^2]

Simulating the QED effects increases EPOCH's memory requirements and so
the code has to be compiled with the correct compilation options to turn
the module on. To turn the module on, open "Makefile" in an editor and
find the commented out line `#DEFINES += $(D)PHOTONS`. Uncomment this
line, then type "make clean" and then "make" (remember to include the
`COMPILER=` if you haven't specified the environment variable) to
rebuild the code with QED support.

Once the code is built with QED support, actually turning on QED for a
specific simulation requires the addition of a new block into the input
deck. This block is simply called [qed][Input_deck_qed]
and starts with the usual "begin:qed" and "end:qed" markers of the other
blocks. The parameters which can go into the block are:

-   use_qed - Turns QED on or off. If you don't want QED effects at all
    then compile the code without the "-DPHOTONS" lines in the makefile.
-   qed_start_time - Specifies the time after which QED effects should
    be turned on. For example you can turn off the routines until a
    laser has crossed the vacuum region in front of the target.
-   produce_photons - Specifies whether you're interested in the
    photons generated by synchrotron emission. If this is F then the
    radiation reaction force is calculated but the properties of the
    emitted photons are not tracked.
-   photon_energy_min - Minimum energy of produced photons. Radiation
    reaction is calculated for photons of all energies, but photons with
    energy below this cutoff are not tracked.
-   photon_dynamics - If F then photons are generated, but their motion
    through the domain is not simulated and they stay where they were
    generated. Photon motion is often less interesting than photon
    generation unless you want to simulate pair production. In these
    cases set this to F.
-   produce_pairs - Whether or not to simulate the process of pair
    generation from gamma ray photons. Both produce_photons and
    photon_dynamics must be T for this to work.
-   qed_table_location - EPOCH's QED routines use lookup tables to
    calculate gamma ray emission and pair production. If you want to use
    tables in a different location from the default put the location in
    this parameter.

QED also requires that the code now know which species are electrons,
positrons and photons. Rather than try to do this automatically the user
has to specify the type of a species. This is done by using a single
"identify" tag in a species block. To specify an electron the block in
the deck would look like

```perl
begin:species
  name = electron
  frac = 0.5
  number_density = 7.7e29
  identify:electron
end:species
```

Once the identity of a species is set then the code automatically
assigns mass and charge states for the species. At present, the user
cannot override these. Possible identities are

-   electron : A normal electron species. All species of electrons in
    the simulation must be identified in this way or they will not
    generate photons.
-   positron : A normal positron species. All species of positron in the
    simulation must be identified in this way or they will not generate
    photons.
-   photon : A normal photon species. One species of this type is needed
    for photon production to work. If multiple species are present then
    generated photons will appear in the first species of this type.
-   bw_electron : The electron species for pair production. If a
    species of this type exists then electrons from the pair production
    module will be created in this species. If no species of this type
    is specified then pair electrons will be generated in the first
    electron species.
-   bw_positron : The positron species for pair production. If a
    species of this type exists then positrons from the pair production
    module will be created in this species. If no species of this type
    is specified then pair positrons will be generated in the first
    positron species.

A species should be identified only once, so a "bw_electron" species
does not need to also be identified as an "electron" species. If the
code is running with "produce_photons=T" then a photon species must be
created by user and identified. If the code is running with
"produce_pairs=T" then the code must specify at least one electron (or
bw_electron) species and one positron (or bw_positron) species. The
code will fail to run if the needed species are not specified.

# Other Useful Info {#other_useful_info}

### Bug reports, feature requests and questions {#bug_reports_feature_requests_and_questions}

All questions and requests after the workshop should be posted on the
GitLab EPOCH project [web page](https://cfsa-pmw.warwick.ac.uk/).

### The VisIt programme {#the_visit_programme}

The VisIt programme is free. It can be downloaded from
<https://wci.llnl.gov/simulation/computer-codes/visit/> There are many
pre-compiled binaries so this ought to be easy. If you have any problems
post a question on the GitLab EPOCH project.

### GDL not IDL {#gdl_not_idl}

If you don't have IDL, or don't want to pay for it!, then the free GDL
is available from <http://gnudatalanguage.sourceforge.net/>

### Updating EPOCH {#updating_epoch}

To update to the latest version of EPOCH simple cd into your Epoch
directory and enter 'git pull'. This will work fine provided you haven't
edited any of the Fortran source code. If you have edited the source
code then you need to learn git.

### Getting Old Copies of EPOCH {#getting_old_copies_of_epoch}

You can also checkout an old version of EPOCH, you may want to get the
version used 18 months ago to reproduce some previous simulations
exactly for example. In this case it is best to checkout a new branch in
the EPOCH repository. If you wanted the version from 10 February 2010
for example you would first enter\
`git log --before=2010-02-11`\
This will give you the log of commits in reverse order, starting on the
11th of February. Identify the commit you want and copy the commit hash
(the long string of numbers and letters following the word "commit"). To
checkout a copy of this version of the code, type\
`git checkout -b old-code `<hash>\
After this your repository will reflect the state of the code at that
point in time. To get back to the current version, just type\
`git checkout master`\

# References

<references />

[^1]: Y. Sentoku and A. J. Kemp, "Numerical methods for particle
    simulations at extreme densities and temperatures: Weighted
    particles, relativistic collisions and reduced currents," J. Comput.
    Phys., 2008.
    [link](http://www.sciencedirect.com/science/article/pii/S0021999108001988)

[^2]: R. Duclous, J. G. Kirk, and A. R. Bell, "Monte carlo calculations
    of pair production in high-intensity laser plasma interactions,"
    Plasma Phys. Contr. F., vol. 53, no. 1, p. 015009,
    2011[1](http://iopscience.iop.org/article/10.1088/0741-3335/53/1/015009).


<!-- ########################  Cross references  ######################## -->


[Acknowledging_EPOCH]: /tutorial/acknowledging_epoch
[Basic_examples]: /tutorial/basic_examples
[Basic_examples__focussing_a_gaussian_beam]: /tutorial/basic_examples/#focussing_a_gaussian_beam
[Binary_files]: /tutorial/binary_files
[Calculable_particle_properties]: /tutorial/calculable_particle_properties
[Compiler_Flags]: /tutorial/compiler_flags
[Compiling]: /tutorial/compiling
[FAQ]: /tutorial/faq
[FAQ__how_do_i_obtain_the_code]: /tutorial/faq/#how_do_i_obtain_the_code
[Input_deck]: /tutorial/input_deck
[Input_deck_adf]: /tutorial/input_deck_adf
[Input_deck_boundaries]: /tutorial/input_deck_boundaries
[Input_deck_boundaries__cpml_boundary_conditions]: /tutorial/input_deck_boundaries/#cpml_boundary_conditions
[Input_deck_boundaries__thermal_boundary_conditions]: /tutorial/input_deck_boundaries/#thermal_boundary_conditions
[Input_deck_collisions]: /tutorial/input_deck_collisions
[Input_deck_constant]: /tutorial/input_deck_constant
[Input_deck_control]: /tutorial/input_deck_control
[Input_deck_control__basics]: /tutorial/input_deck_control/#basics
[Input_deck_control__maxwell_solvers]: /tutorial/input_deck_control/#maxwell_solvers
[Input_deck_control__requesting_output_dumps_at_run_time]: /tutorial/input_deck_control/#requesting_output_dumps_at_run_time
[Input_deck_control__stencil_block]: /tutorial/input_deck_control/#stencil_block
[Input_deck_control__strided_current_filtering]: /tutorial/input_deck_control/#strided_current_filtering
[Input_deck_dist_fn]: /tutorial/input_deck_dist_fn
[Input_deck_fields]: /tutorial/input_deck_fields
[Input_deck_injector]: /tutorial/input_deck_injector
[Input_deck_injector__keys]: /tutorial/input_deck_injector/#keys
[Input_deck_laser]: /tutorial/input_deck_laser
[Input_deck_operator]: /tutorial/input_deck_operator
[Input_deck_output__directives]: /tutorial/input_deck_output/#directives
[Input_deck_output_block]: /tutorial/input_deck_output_block
[Input_deck_output_block__derived_variables]: /tutorial/input_deck_output_block/#derived_variables
[Input_deck_output_block__directives]: /tutorial/input_deck_output_block/#directives
[Input_deck_output_block__dumpmask]: /tutorial/input_deck_output_block/#dumpmask
[Input_deck_output_block__multiple_output_blocks]: /tutorial/input_deck_output_block/#multiple_output_blocks
[Input_deck_output_block__particle_variables]: /tutorial/input_deck_output_block/#particle_variables
[Input_deck_output_block__single-precision_output]: /tutorial/input_deck_output_block/#single-precision_output
[Input_deck_output_global]: /tutorial/input_deck_output_global
[Input_deck_particle_file]: /tutorial/input_deck_particle_file
[Input_deck_probe]: /tutorial/input_deck_probe
[Input_deck_qed]: /tutorial/input_deck_qed
[Input_deck_species]: /tutorial/input_deck_species
[Input_deck_species__arbitrary_distribution_functions]: /tutorial/input_deck_species/#arbitrary_distribution_functions
[Input_deck_species__ionisation]: /tutorial/input_deck_species/#ionisation
[Input_deck_species__maxwell_juttner_distributions]: /tutorial/input_deck_species/#maxwell_juttner_distributions
[Input_deck_species__particle_migration_between_species]: /tutorial/input_deck_species/#particle_migration_between_species
[Input_deck_species__species_boundary_conditions]: /tutorial/input_deck_species/#species_boundary_conditions
[Input_deck_subset]: /tutorial/input_deck_subset
[Input_deck_window]: /tutorial/input_deck_window
[Landing]: /tutorial/landing
[Landing_Page]: /tutorial/landing_page
[Libraries]: /tutorial/libraries
[Links]: /tutorial/links
[Maths_parser__functions]: /tutorial/maths_parser/#functions
[Non-thermal_initial_conditions]: /tutorial/non-thermal_initial_conditions
[Previous_versions]: /tutorial/previous_versions
[Python]: /tutorial/python
[Running]: /tutorial/running
[SDF_Landing_Page]: /tutorial/sdf_landing_page
[Structure]: /tutorial/structure
[Using_EPOCH_in_practice]: /tutorial/using_epoch_in_practice
[Using_EPOCH_in_practice__manually_overriding_particle_parameters_set_by_the_autoloader]: /tutorial/using_epoch_in_practice/#manually_overriding_particle_parameters_set_by_the_autoloader
[Using_EPOCH_in_practice__parameterising_input_decks]: /tutorial/using_epoch_in_practice/#parameterising_input_decks
[Using_delta_f]: /tutorial/using_delta_f
[Visualising_SDF_files_with_IDL_or_GDL]: /tutorial/visualising_sdf_files_with_idl_or_gdl
[Visualising_SDF_files_with_LLNL_VisIt]: /tutorial/visualising_sdf_files_with_llnl_visit
[Workshop_examples]: /tutorial/workshop_examples
[Workshop_examples__a_2d_laser]: /tutorial/workshop_examples/#a_2d_laser
[Workshop_examples__a_basic_em-field_simulation]: /tutorial/workshop_examples/#a_basic_em-field_simulation
[Workshop_examples__getting_the_example_decks_for_this_workshop]: /tutorial/workshop_examples/#getting_the_example_decks_for_this_workshop
[Workshop_examples__specifying_particle_species]: /tutorial/workshop_examples/#specifying_particle_species
[Workshop_examples_continued]: /tutorial/workshop_examples_continued
