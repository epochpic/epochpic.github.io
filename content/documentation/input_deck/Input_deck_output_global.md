---
draft: false
toc: true
type: docs

title: Output_global block
linktitle: Output_global block
weight: 160
menu:
  documentation:
    parent: Input deck
    weight: 10
---

This block contains parameters which should be applied to all output
blocks. See [EPOCH input deck][Input_deck] for more
information on the input deck.

With the introduction of multiple output blocks, there are now a few
parameters that only make sense to be applied globally across all output
blocks. To accommodate this, a new block named "output_global" has been
added. Most of the parameters accepted by this block have the same
meaning as those in the "output" block except that they are applied to
all "output" blocks.

The parameters that can be specified in the "output_global" block are
as follows:
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
default is "F".
- `dump_last` - Logical flag which determines whether to
write an output file just before ending the simulation. The default is
"T" if an output block exists in the input deck and "F" otherwise.
"dump_final" is accepted as a synonym.
- `time_start` - Floating point parameter which specifies
the simulation time at which to start considering output for all output
blocks. Note that if "dump_first" or "dump_last" are set to true for
any block then dumps will occur at the first or last timestep regardless
of the value of this parameter. This also applies to the three following
parameters. The default value is 0.
- `time_stop` - Floating point parameter which specifies the
simulation time at which to stop considering output for all output
blocks. The default value is the largest possible float.
- `nstep_start` - Integer parameter which specifies the step
number at which to start considering output for the block. The default
value is 0.
- `nstep_stop` - Integer parameter which specifies the step
number at which to stop considering output for the block. The default
value is the largest possible integer.
- `walltime_start` - Floating point parameter which specifies the elapsed walltime in
seconds at which to start considering output for all output blocks. Note
that if **dump_first** or **dump_last** are set to true for any blocks
then dumps will occur at the first or last timestep regardless of the
value of the **walltime_start** parameter. The default value is 0.
- `walltime_stop` - Floating point parameter which specifies the elapsed walltime in
seconds at which to stop considering output all output blocks. The
default value is the largest possible float.
- `sdf_buffer_size` - Integer parameter. When writing
particle data to an SDF file, the data is first transferred into an
output buffer. The size of this buffer can have a big impact on the
overall speed of writing dump files. This parameter allows the size of
the buffer to be specified in bytes. The default value is 67108864 (64
MB).
- `filesystem` - String parameter. Some filesystems can be
unreliable when performing parallel I/O. Often this is fixable by
prefixing the filename with 'ufs' or 'nfs'. This parameter supplies the
prefix to be used. The default value is an empty string.
- `use_offset_grid` - When using moving windows some
visualisation programs (notably VisIt) show the motion of the window by
moving the visualisation window rather than by changing the x-axis.
Setting this option to "T" causes the code to write another grid which
always gives the offset relative to the left hand edge of the window
rather than the true origin. Performs no function when not using the
moving window. The default value is "F".
- ` dump_first_after_restart` - Logical flag to enable a
dump to occur immediately after restart. In the past, a `dump_first`
flag in the output block would cause an output dump immediately after
restarting. Since this is rarely the desired behaviour, the flag is now
ignored when restarting. To force a dump to occur immediately after
restart, set `dump_first_after_restart = T` in the output block. The
default value is "F".



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
