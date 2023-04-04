---
title: Basic Output

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Input Output
    weight: 10
---

EPOCH uses a file format called _SDF_ which was custom developed for use
with codes developed by CFSA at the University of Warwick. However, it isn't necessary 
to have a full understanding of the file format
to add the output of new variables to EPOCH. To add a new variable to
EPOCH's output, you simply have to use the supplied subroutines of the SDF
library which is part of EPOCH. The file output from EPOCH takes place in
_diagnostics.F90_, so to add new variables to the output you must
add additional code there. Looking through the listings, you will see two lines:
 ```perl
  CALL sdf_open(sdf_handle, filename, rank, comm, c_sdf_write)

  CALL sdf_close(sdf_handle)
 ```

These, as may be expected, are the commands which open and close the SDF file.
It is perfectly possible to create new SDF files containing only your own data.
There are various commands in-between which actually write the data into the
file. These commands start with `sdf_` to ensure that the don't
conflict with any other subroutine names in the code.
Some more complex areas of I/O, such as the particle probes
and the distribution function routines call other subroutines in their
respective source files, but these too make use of the SDF routines to actually
write data. A user should never try to write data directly to the output file,
since this will cause problems with internal parts of the SDF format and
generate a nonsensical file.

## The dumpmask

Looking through `diagnostics.F90` there are many lines with commands which
begin `sdf_`, but are all prepended with a command which looks
like:
 ```perl
  IF (IAND(dumpmask(c_dump_id), code) .NE. 0) THEN
 ```
This is the method by which EPOCH allows the end user to specify whether a
variable should be dumped, and whether it should only be dumped at
full/partial/restart dumps. **dumpmask** is an integer array, the length of
which is defined by the variable `num_vars_to_dump` in
`shared_data.F90` and contains the bitmask representing all the
types of output which should be written for the associated variable. The
possible values in the bitmask are:

 
-  `c_io_never` - Never dump this variable.
-  `c_io_always` - Dump this variable at every output dump.
-  `c_io_full` - Dump this variable at full dumps.
-  `c_io_restartable` - Dump this variable for restart dumps.
-  `c_io_species` - If meaningful for this variable, write
  information for each species rather than integrated over all species.
-  `c_io_no_sum` - If meaningful for this variable, do not
  write information integrated over all species.
-  `c_io_averaged` - If meaningful for this variable, write
  this variable averaged over time.
-  `c_io_snapshot` - If meaningful for this variable, write
  the non-averaged value of the variable.
  

The `c_dump_id` entry is a constant defined in
`shared_data.F90` which identifies the variable's index within
this dumpmask.

When adding a new variable to be written to disk, the value of
`num_vars_to_dump` should be increased to match the number of new
written variables. Next, open the file `src/deck/deck_io_block.F90`
and find the line:
 ```perl
  CHARACTER(LEN=entry_length), DIMENSION(io_block_elements) :: &
      io_block_name = ...
 ```

Simply add new strings for your new variables to the end of the
definitions along with its `c_dump_id` value. These new variable
names should then be placed in your input decks
in the same place as the existing I/O information and take the same parameters.

## Precompiler directives and the input deck
In theory, it is possible for someone to request a feature of the code in the
input deck which this version hasn't been compiled with. In this
case, there is a special error code `c_err_pp_options_wrong`
which causes the input deck parser to give a meaningful error. You should also
set the string `extended_error_string` to be the define
command for the missing preprocessor directive i.e
`extended_error_string = "-DMY_PRECOMPILER_DIRECTIVE"`
