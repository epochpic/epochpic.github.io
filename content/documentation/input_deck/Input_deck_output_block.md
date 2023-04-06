---
draft: false
toc: true
type: docs

title: Output block
linktitle: Output block
weight: 150
menu:
  documentation:
    parent: Input deck
    weight: 10
---

This block contains information about when and how to dump output files.
See [EPOCH input deck][Input_deck] for more information
on the input deck.

# Basics

Output in EPOCH is handled using the custom designed SDF file format
(*Self Describing Format*). A detailed specification of this format is
available elsewhere, although this is only of interest to developers
wishing to write new libraries. EPOCH comes with readers for ITT IDL,
LLNL VisIt, Mathworks MatLab and Python. The IDL reader is also
compatible with the open source GDL tool.

There are two styles of output block supported by EPOCH. The first
style, which will be referred to as the "traditional" style, is the
method that has been supported by EPOCH since its inception. With this
method, a single output block governs all the output dumps which are to
be performed. There are a few levels of output which give some small
amount of flexibility over what gets dumped but these do not allow for a
very fine-grained control.

In version 4.0 of EPOCH, a new style was introduced in which multiple
named output blocks may be specified allowing for much greater
flexibility. The existence of a "name" parameter is what determines that
an output block is the new style rather than the traditional style.

Most of the parameters are shared by both styles. The following sections
document the traditional style of output block and any differences
between the two styles are described
[below](#multiple_output_blocks) .

What the code should output and when it should output it is specified in
the "output" block of the input deck. An example output block is shown
below:

```perl
begin:output
   # If use_offset_grid is true then the code dumps a grid which
   # displays positions relative to the left hand edge of the window
   use_offset_grid = F
   # number of timesteps between output dumps
   dt_snapshot = 1.0e-14
   # Number of dt_snapshot between full dumps
   full_dump_every = 10
   restart_dump_every = -1
   force_final_to_be_restartable = T

   # Properties at particle positions
   particles = never
   px = never
   py = never
   pz = never
   vx = never
   vy = never
   vz = never
   charge = never
   mass = never
   particle_weight = never

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
   ekbar = always + species
   mass_density = never + species
   charge_density = always
   number_density = always + species
   temperature = always + species

   distribution_functions = always
   particle_probes = never
end:output
```

There are three types of output dump in EPOCH which are used for
different purposes. These types are:

-   `normal` - The most frequent type of output dump in EPOCH is a normal
    dump.
-   `full` - A full dump is usually written every 10 or so normal dumps. A
    full dump contains all the data that a normal dump contains and
    should also contain any information which is needed only
    infrequently, whether this is the full particle information or a
    large distribution function. It is possible to turn off full dumps
    completely.
-   `restart` - A restart dump is a dump where the code guarantees to
    write enough data to allow the code to restart from the output.
    Output dumps are guaranteed to contain all the information in a
    normal dump and, if they coincide with the timing for a full dump,
    will also contain the full dump information.

Information will never be written into a file twice, even if two
conditions for it being written are satisfied (i.e even if px should be
dumped both because it is a full dump and a restart dump, px will only
be written once).

Note that these dump levels only really make sense for the traditional
style of output block and are not really required when the new style is
used.

# Dumpmask

When specifying which type of output dump to write a variable to there
are eight options which can be specified for each variable and can be
combined by addition. Some combinations make no sense but are formally
valid. The first four options specify at which output types the variable
is to be dumped:
- `never` - If the variable is not a required restart
variable then it will never be written. If it is a required restart
variable then it will be written only at restart dumps.
- `full` - This variable will be written at full dumps only.
- `always` - This variable will be written at full, normal
and restart dumps.
- `restart` - This variable will be written at restart dumps
only. Note that variables required for restarting the code are always
written to restart dumps. This flag is to enable the writing of
additional variables into such dump files.
For grid variables derived from summing over particles (ie. "ekbar",
"mass_density", "charge_density", "number_density", "temperature")
the following two parameters also apply.
- `species` - The derived variable should be output on a
species by species basis. It is combined with a dumpmask code by
addition as in:
**charge_density = always + species** .
- `no_sum` - The output for this derived variable should not
be summed over all species. By default, derived variables are summed
over all species. If you don't want to include this sum, you must use
the "no_sum" flag. It is combined with a dumpmask code by addition as
in:
**charge_density = always + species + no_sum** .
Most grid variables may be averaged over time. A more detailed
description of this is given in [\#Data
Averaging](#data_averaging). Data averaging is specified
using the following dumpmask parameters.
- `average` - The output for this variable should be averaged
over time. The time span over which the variable will be averaged is
controlled using flags described [below](#directives).
- `snapshot` - By default, the "average" parameter replaces
the variable with an averaged version of the data. Adding this flag
specifies that the non-averaged variable should also be dumped to file.
When applied to a variable, these codes are referred to as a *dumpmask*.

# Directives

The first set of options control the type and frequency of output dumps.
They are used as follows
- `disabled` - Logical flag. If this is set to "T" then the
block is ignored and never generates any output. The default value is
"F".
- `dt_snapshot` - Sets the interval between normal output
dumps in simulation seconds. Setting zero or negative means that the
code will not output based on this condition. The code does NOT
guarantee that outputs will be exactly *dt_snapshot* apart, what is
guaranteed is that the next output will be after the first iteration
which takes the simulation to a time $\ge$ *dt_snapshot* from the last
output. As with other variables which specify a unit of time, it can be
specified in more convenient unit by using a multiplication factor (see
[here][Maths_parser__constants]). For example,
"dt_snapshot = 5 \* femto" will set it to be 5 femtoseconds. The
default value is a large number which will never trigger an output.
- `nstep_snapshot` - Sets the number of timesteps between
normal output dumps. Setting zero or negative means that the code will
not output based on this condition. If *dt_snapshot* is also specified
then both conditions are considered and output will be generated when
either condition is met. The default value is a large integer which will
never trigger an output.
- `full_dump_every` - The number of normal output dumps
between full output dumps. Setting to zero makes every dump a full dump.
Setting to a negative number stops the code from producing any full
dumps. This is the default.
- `restart_dump_every` - The number of normal output dumps
between restart dumps. Setting to zero makes every dump a restart dump.
Setting to a negative number stops the code from producing any restart
dumps. This is the default.
- `force_first_to_be_restartable` - Logical flag which
determines whether the file written at time zero is a restart dump. The
default value is "F".
- `force_last_to_be_restartable` - Force the code to
override other output settings and make the last output dump it writes
be a restart dump. Any internal condition which causes the code to
terminate will make the code write a restart dump, but code crashes or
scheduler terminations will not cause the code to write a restart dump.
"force_final_to_be_restartable" is accepted as a synonym. The
default value is "T".
- `dump_first` - Logical flag which determines whether to
write an output file immediately after initialising the simulation. The
default is "T".
- `dump_last` - Logical flag which determines whether to
write an output file just before ending the simulation. The default is
"T" if an output block exists in the input deck and "F" otherwise.
"dump_final" is accepted as a synonym.
- `time_start` - Floating point parameter which specifies
the simulation time at which to start considering output for the block.
Note that if "dump_first" or "dump_last" are set to true for this
block then dumps will occur at the first or last timestep regardless of
the value of the *time_start* parameter. This also applies to the three
following parameters. The default value is 0.
- `time_stop` - Floating point parameter which specifies the
simulation time at which to stop considering output for the block. The
default value is the largest possible float.
- `nstep_start` - Integer parameter which specifies the step
number at which to start considering output for the block. The default
value is 0.
- `nstep_stop` - Integer parameter which specifies the step
number at which to stop considering output for the block. The default
value is the largest possible integer.
- `walltime_start` - Floating point parameter which specifies the elapsed walltime in
seconds at which to start considering output for the block. Note that if
**dump_first** or **dump_last** are set to true for this block then
dumps will occur at the first or last timestep regardless of the value
of the **walltime_start** parameter. The default value is 0.
- `walltime_stop` - Floating point parameter which specifies the elapsed walltime in
seconds at which to stop considering output for the block. The default
value is the largest possible float.
- `dump_cycle` - If this is set to a positive integer then
the output file number will be reset to zero after the specified cycle
number is reached. eg. if "dump_cycle = 2" then the sequence of output
dumps will be 0000.sdf, 0001.sdf, 0002.sdf, 0000.sdf, 0001.sdf, etc. The
default is 0, so dump cycling never occurs.
- `dump_cycle_first_index` - If this is set to a positive
integer then the value is used as the first index to use when cycling
output dumps due to the "dump_cycle" parameter. For example, if
"dump_cycle = 2" and "dump_cycle_first_index = 1" then the sequence
of output dumps will be 0000.sdf, 0001.sdf, 0002.sdf, 0001.sdf,
0002.sdf, 0001.sdf, etc. The default is 0.
- `dump_source_code` - EPOCH has the ability to write its
own source code into restart dumps. This is generated at compile time
and embedded into the binary and so is guaranteed to match that
corresponding to the running code. EPOCH comes with a script called
*unpack_source_from_restart* which can be used to unpack the source
code from a restart dump. To use this script, just type
**unpack_source_from_restart <sdf_filename>** at the command-line. If
this logical flag is set to false then the feature will be disabled. The
default value is "T".
- `dump_input_decks` - If this logical flag is set to true
then a copy of the input decks for the currently running simulation is
written into the restart dumps. The default value is "T".
- `dt_average` - When averaged variables are being output to
file, this parameter specifies the simulation time period over which
averaging is to occur. "averaging_period" is accepted as a synonym.
- `nstep_average` - When averaged variables are being output
to file, this parameter specifies the number of time steps over which
averaging is to occur. "min_cycles_per_average" is accepted as a
synonym. If both *dt_average* and *nstep_average* are specified, the
code will use the one which gives the longest simulation time-span.
- `use_offset_grid` - When using moving windows some
visualisation programs (notably VisIt) show the motion of the window by
moving the visualisation window rather than by changing the x-axis.
Setting this option to "T" causes the code to write another grid which
always gives the offset relative to the left hand edge of the window
rather than the true origin. Performs no function when not using the
moving window. The default value is "F".
- `filesystem` - String parameter. Some filesystems can be
unreliable when performing parallel I/O. Often this is fixable by
prefixing the filename with 'ufs' or 'nfs'. This parameter supplies the
prefix to be used. The default value is an empty string.
- `file_prefix` - Although this parameter is supported by
the traditional style of output block, its primary purpose is for use
with multiple output blocks so it is documented in .
A few additional parameters have been added for use with the new style
of output block. These are documented
[below](#multiple_output_blocks).

# Particle Variables {#particle_variables}

The next set are per particle properties. If you wish to plot these
according to their spatial positions, you must include the
"particle_grid" in your output variables. All entries have a default
dumpmask of "never".
- `particle_grid` - Requests the output of particle
positions. This is a restart variable. No particle variables can be
plotted in VisIt unless this is dumped. If any particle variables are
written then the "particle_grid" is automatically written unless
"particle_grid = never" is specified. The synonym "particles" may also
be used.
- `px,py,pz` - The dumpmasks for the particle momenta.
Restart variable.
- `vx,vy,vz` - The dumpmasks for the particle velocities.
- `charge` - The dumpmask for the charge of a given particle.
This has no effect if the code is not compiled with the flag
"-DPER_PARTICLE_CHARGE_MASS" (see
[here][Compiler_Flags] ).
- `mass` - The dumpmask for the mass of a given particles.
This has no effect if the code is not compiled with the flag
"-DPER_PARTICLE_CHARGE_MASS" (see
[here][Compiler_Flags]). The synonym
"rest_mass" may also be used.
- `particle_weight` - The dumpmask for the weighting
function which describes how many real particles each pseudoparticle
represents. Restart variable. The synonym "weight" may also be used.
- `ejected_particles` - If requested then all the particles
which have left the simulation domain since the last output dump of this
type are included in the output. The list of ejected particles is
treated as if it were a separate species and the particle variables
which get written are requested using the other particle variable flags
(ie. "particle_grid", etc). Once the data has been written, the ejected
particle lists are reset and will accumulate particles until the next
requested output dump.
- `particle_energy` - The dumpmask for per-particle kinetic
energy.
- `relativistic_mass` - The dumpmask for per-particle
relativistic mass (ie. not rest mass).
- `gamma` - The dumpmask for per-particle relativistic gamma
(ie. $[1-(v/c)^2]^{-1/2}$).
- `optical_depth` - The dumpmask for per-particle optical
depth. Restart variable. This option is only supplied for debugging
purposes and should not be required by most users.
- `trident_optical_depth` - The dumpmask for per-particle
optical depth used by the Trident model. Restart variable. This option
is only supplied for debugging purposes and should not be required by
most users.
- `qed_energy` - The dumpmask for per-particle QED-related
particle energy. Restart variable. This option is only supplied for
debugging purposes and should not be required by most users.
- `work_{x,y,z}` - The dumpmask for the work exerted by the fields on each particle
during the last time step. The work is divided into its three spatial
components. The output is in numbers of $mc^2$ corresponding to the
particle's $\gamma$-factor. Requires compiler flag
"WORK_DONE_INTEGRATED".
- `work_{x,y,z}_total` - Same as above, but the work is integrated over the entire simulation
duration. The sum of all three components equals the particle's
$\gamma$-factor. Requires compiler flag "WORK_DONE_INTEGRATED".
- `id` - Global particle ID. See below for details.
Particle IDs are useful if you want to track the progress of each
particle throughout the simulation. Since they increase the size of each
particle data structure, they are disabled by default and must be
enabled using a compiler flag. The "PARTICLE_ID" flag will use an
8-byte integer to represent the ID and "PARTICLE_ID4" uses a 4-byte
integer. They are written to file using the "id" flag.

Note: In the current implementation, the particle IDs are passed between
processors and written to file using REAL numbers. This means that in
double precision the maximum particle ID is $2^{53} \sim 10^{16}$. This
should be ample for the foreseeable future. However, if the code is
compiled for single precision then the maximum ID is
$2^{24} = 16777216$. Probably not big enough.

# Grid Variables {#grid_variables}

The next set of parameters specify properties which are defined on a
regular cartesian mesh. All entries have a default dumpmask of "never".
- `grid` - The dumpmask for the Cartesian grid which defines
the locations of the grid variables. No grid variables can be plotted in
VisIt unless this variable is output. If any grid variables are written
then the "grid" is automatically written unless "grid = never" is
specified. The synonym "field_grid" may also be used.
- `ex,ey,ez` - The electric field vectors pointing in all
three directions. Restart variables.
- `bx,by,bz` - The magnetic field vectors pointing in all
three directions. Restart variables. In 1D bx is a trivial variable
because of the Solenoidal condition. It is included simply for symmetry
with higher dimension codes.
- `jx,jy,jz` - The current densities pointing in all three
directions. Restart variables. Can have species dumpmask.

# Derived Variables {#derived_variables}

The final set of parameters specify properties which are not variables
used in the code but are derived from them. The first six variables are
derived by summing properties of all the particles in each grid cell.
The resulting quantities are defined on the regular cartesian mesh used
for grid variables. All entries have a default dumpmask of "never".
- `ekbar` - Mean kinetic energy on grid in $J$. Can have
species dumpmask.
- `ekflux` - Mean kinetic energy flux in each direction on
the grid in $W/m^2$. Can have species dumpmask.
- `mass_density` - Mass density on grid in $kg/m^3$. Can
have species dumpmask.
- `charge_density` - Charge density on grid in $C/m^3$. Can
have species dumpmask.
- `number_density` - Number density on grid in $m^{-3}$. Can
have species dumpmask.
- `particles per cell` - Number of particles per cell. Can
have species dumpmask. The synonym "ppc" may also be used.
- `average weight` - Average of weight of the particles in
each cell. Can have species dumpmask.
- `average_p{x,y,z}` - Average momentum in each direction of the particles in each cell. Can have species dumpmask.
- `temperature` - Isotropic temperature on grid in $K$.
Calculated from standard deviation of particle momenta, so in general
matches mean kinetic energy only for isotropic plasma with no net drift.
The synonym "temp" may also be used. Can have species dump mask.
- `temperature_{x,y,z}` - The temperature in each of the {x,y,z} directions, respectively, in
$K$. The synonyms "temp_{x,y,z}" and "t{x,y,z}" may also be used.
Can have species dumpmask.
- `poynt_flux` - Poynting flux in each direction in $W/m^2$.
- `coulomb_logarithm` - An estimate for the averaged Coulomb logarithm in each cell between
the first listed species, and all other species. If the “species” dumpmask is given, then this calculation
is repeated for the remaining particle species.

# Other Variables {#other_variables}

-   `distribution_functions` - Dumpmask for outputting
    distribution functions specified in the input deck. Each individual
    distribution function can have its own dumpmask and these will be
    applied after the value of "distribution_functions" has been
    considered. For example, if the output block contains
    "distribution_functions = full" and the dist_fn block (see
    [here][Input_deck_dist_fn])
    contains "dumpmask = always" then the distribution function will
    only be output at full dumps.
-   `particle_probes` - Dumpmask for outputting particle
    probes specified in the input deck. Each individual particle probe
    can have its own dumpmask and these will be applied after the value
    of "particle_probes" has been considered. For example, if the
    output block contains "particle_probes = always" and the dist_fn
    block contains "dumpmask = full" then the particle probe will only
    be output at full dumps.
-   `absorption` - This is a two-valued output variable. It
    accepts a dumpmask in the same manner as other output variables.
    When selected, two numbers will be calculated and written to file:

1.  "Absorption/Laser_enTotal" - The total amount of energy injected
    into the simulation by laser boundaries.
2.  "Absorption/Abs_frac" - The fraction of the total laser energy
    being absorbed by the open boundaries.

-   `total_energy_sum` - This is also a two-valued output
    variable. It accepts a dumpmask in the same manner as other output
    variables. When selected, the following two numbers will be
    calculated and written to file:

1.  "Total Particle Energy in Simulation (J)"
2.  "Total Field Energy in Simulation (J)"

# Data Averaging {#data_averaging}

EPOCH can accumulate an average value for field variables to be written
to output dumps. These may be requested by using the "average" keyword
when specifying a dump variable. The non-averaged variable will still be
written to restart dumps where required for restarting the code but not
full or normal dumps. If you also want the non-averaged variable to be
written then you can add the "snapshot" option.

The period of time over which averaging occurs can be specified using
the "dt_average" keyword. Alternatively, you may specify the number of
cycles over which to perform the averaging using the "nstep_average"
keyword. If both "dt_average" and "nstep_average" are specified then
the averaging will be performed over the longest of the two intervals.

Note that previous versions of the code would alter the time step to
ensure that there were enough cycles between output dumps to satisfy the
"nstep_average" parameter. However, since it affects the accuracy of
the result, this is no longer the case and only a warning message is
issued.

The following shows an example use of averaging in the output block.

```perl
begin:output
   dt_snapshot = 1.0e-15
   full_dump_every = 10
   dt_average = 1.0e-17

   charge_density = always + average + snapshot
   mass_density = full + average + snapshot
   ekbar = full + average
end:output
```

With this configuration, "charge_density" will be written in both
normal and averaged form at normal, full and restart dumps.
"mass_density" will be written in both forms at full dumps. Only the
average value of "ekbar" will be written at full dumps.

Only field and derived variables can be averaged currently in EPOCH.
Particle properties, distribution functions and particle probes cannot
currently be averaged.

# Single-precision output {#single_precision_output}

By default, EPOCH is compiled and run using double precision arithmetic.
This is the only method which has been fully tested and the method that
we recommend to other users of the code. However, this also means that
data files can get very large.

To avoid this problem, it is possible to run the code in double
precision but convert the data to single precision when writing to disk.
This is done by adding the "single" field the the dumpmask of an output
variable. It can be specified on a per-variable basis.

```perl
begin:output
   dt_snapshot = 8 * femto

   grid = always
   ex = always
   ey = always + single
end:output
```

In this example, the grid variable "ex" will be written as a double
precision array and "ey" will be converted to single precision.

Dumping variable averages adds an extra field variable for each average
requested. These take up memory during runtime but do not influence the
simulation behaviour in any way. For this reason, if the average is to
be written out in single precision then it may as well be stored in a
single precision variable. This behaviour can be requested using the
"average_single" dumpmask flag.

# Multiple output blocks {#multiple_output_blocks}

In more recent versions of EPOCH, it is now possible to have multiple
"output" blocks in the input deck, each with their own "dt_snapshot" or
"nstep_snapshot" and their own set of output variables.

The syntax remains the same as the original "output" block syntax with
the addition of "name" and "restartable" fields.

The "name" field specifies the file name to use for the output list.
Each time EPOCH generates an output dump, it writes an entry into the
file "`<name>.visit`". This can be used to find all the output dumps of
a specific output block. It is named with a ".visit" suffix to enable
its use as a file grouping list in the VisIt data analysis tool, but it
is just a plain text file so it can equally be used by any other
program.

If two output blocks are written at the same time, the output will be
combined into a single file.

The "restartable" field specifies that the output block should generate
output dumps containing all the information necessary to restart a
simulation.

The following parameters are supported by the new style of output block
in addition to those for the traditional style:

-   `name` - Identifies the output block with a name which is
    required when multiple output blocks are used.
-   `restartable` - Specifies whether or not the output for
    this block is a restartable dump.
-   `dump_at_times` - Floating point parameter which
    specifies a set of simulation times at which to write the current
    output block. This can only be used with named output blocks. The
    values are given as a comma separated list. eg. "dump_at_times =
    0, 0.15, 1.1". The name "times_dump" is accepted as a synonym. By
    default the list is empty.
-   `dump_at_nsteps` - Integer parameter which specifies a
    set of step numbers at which to write the current output block. This
    can only be used with named output blocks. The values are given as a
    comma separated list. eg. "dump_at_nsteps = 5, 11, 15". The name
    "nsteps_dump" is accepted as a synonym. By default the list is
    empty.
-   `dump_at_walltimes` - Floating point parameter which specifies a set of elapsed walltimes
at which to write the current output block. This can only be used with
named output blocks. The values are given as a comma separated list. eg.
"dump_at_walltimes = 10, 100.1, 250.5". These times are the total
elapsed time in seconds since the start of the simulation. Note that if
the simulation has been restarted then the total elapsed time will
include the accumulated walltime of all previous runs that were used to
produce the restart dump. The name **walltimes_dump** is accepted as a
synonym. By default the list is empty.
-   `walltime_interval` - Floating point parameter which specifies the interval between output
dumps in elapsed walltime seconds. Setting zero or negative means that
the code will not output based on this condition. The default value is -1.0.
- `file_prefix` - String parameter. It is sometimes useful
to distinguish between dumps generated by the different output blocks.
This parameter allows the user to supply a file prefix to be prepended
to all dumps generated by the current output block. See below for
further details. The default value is an empty string.
- `rolling_restart` - Logical flag. If set to "T", this sets
the parameters required for performing rolling restarts on the current
block. It is a shorthand for setting the following flags: "dump_cycle =
1", "restartable = T" and "file_prefix = roll". With rolling restarts
enabled the first file will be named "roll0000.sdf" and the second will
be "roll0001.sdf". The third dump will again be named "roll0000.sdf",
overwriting the first one. In this way, restart dumps can be generated
throughout the duration of the simulation whilst limiting the amount of
disk space used.

The following parameters cannot be used in conjunction with the new
style of output block:

-  `full_dump_every`
-  `restart_dump_every`
-  `force_first_to_be_restartable`
-  `force_last_to_be_restartable`
-  `use_offset_grid`

The "file_prefix" parameter warrants some further discussion. This
parameter prepends the given prefix to all files generated by the output
block in which it is specified. For example, if "file_prefix = aa" is
set then files generated by the output block will be named "aa0000.sdf",
etc. instead of just "0000.sdf".

This also allows different variables to different files at the same time
step. For example, here are two output blocks which do not use file
prefixes:

```perl
begin:output
   name = o1
   nstep_snapshot = 1
   charge_density = always
end:output

begin:output
   name = o2
   dump_at_nsteps = 10
   restartable = T
end:output
```

With this input deck, we want to have the "charge_density" derived
variable at every snapshot and then periodically write a restart dump.
The problem is that the dump file "0010.sdf" contains both the restart
information and the "charge_density" variable. At the end of the run we
can't just delete the large restart dumps without losing the smaller
variables at that time step.

With the new version we would add a prefix to one or both blocks:

```perl
begin:output
   name = o1
   file_prefix = small
   nstep_snapshot = 1
   charge_density = always
end:output

begin:output
   name = o2
   nstep_snapshot = 10
   restartable = T
end:output
```

Now the "charge_density" will be written to "small0000.sdf", etc. At
step 10, two files will be written: "small0010.sdf" containing just the
charge_density and "0000.sdf" containing all the restart variables.

Note that some care must be taken, since if the same variable is in the
output block for multiple file prefixes then multiple copies will be
written to file. This obviously uses more disk space and is more time
consuming than necessary.

It should also be noted that if multiple output blocks use the same file
stem then their output will be combined. eg:

```perl
begin:output
   name = o1
   file_prefix = a
   dump_at_nsteps = 2,4
   ex = always
end:output

begin:output
   name = o2
   file_prefix = a
   dump_at_nsteps = 3,4
   ey = always
end:output

begin:output
   name = o3
   file_prefix = b
   dump_at_nsteps = 4
   ez = always
end:output
```

In this example, at step 2 a0000.sdf contains ex, step 3 a0001.sdf
contains ey, step 4 a0002.sdf contains ex, ey and b0000.sdf contains ez.



<!-- ########################  Cross references  ######################## -->


[Compiler_Flags]: /documentation/basic_usage/compiler_flags
[Input_deck]: /documentation/input_deck/input_deck
[Input_deck_dist_fn]: /documentation/input_deck/input_deck_dist_fn
[Maths_parser__constants]: /documentation/code_details/maths_parser#constants
