---
title: Pre-compilation Flags

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Core Structure
    weight: 100
---
EPOCH uses precompiler directives to switch certain features of the code on
or off. The precompiler directives all begin with a "#" character and look
like:
 ```perl
#ifdef MY_PRECOMPILER_DIRECTIVE
  some_fortran_of_some_kind
#else
  some_other_fortran
#endif
 ```
They behave in a very simple manner. The precompiler runs BEFORE the
Fortran compiler and, until it reaches a precompiler directive, it just creates
a temporary file which is an exact copy of the source file. When it reaches a
precompiler directive of this kind it treats the #ifdef commands as
if/then/else statements. If
`MY_PRECOMPILER_DIRECTIVE` was defined in the makefile then
`some_fortran_of_some_kind` is pushed out to the temporary
file. Otherwise `some_other_fortran` is written instead.
The precompiler directives themselves are never output to the temporary
file. Once then preprocessor has finished, it passes this temporary file
to the Fortran compiler which can then compile it just like any other
standard Fortran file.

## When to use precompiler directives
 
-  When adding properties to the `particle` structure.
-  When adding time consuming calculations to the particle pusher.
  
Precompiler directives should be avoided when there is no significant
performance gain or memory reduction to be made. Wherever possible, optional
features should be controlled by parameters in the input deck.

## The directive printing routine on code startup
When EPOCH starts it prints the precompiler directives that it was built with
and what they mean. This isn't required, but has proved very useful and is
implemented in a very simple way. Just open the file
`src/housekeeping/welcome.F90` and find the subroutine
`compiler_directives`. There are a large block of precompiler
directives which read:
 ```perl
#ifdef TRACER_PARTICLES
    defines = IOR(defines, c_def_tracer_particles)
    WRITE(*, *) "Tracer particle support -DTRACER_PARTICLES"
#endif
 ```

Simply add a new element to the end of the list.
 ```perl
#ifdef MY_PRECOMPILER_DIRECTIVE
    defines = IOR(defines, c_def_my_precompiler_directive)
    WRITE(*,*) "My new physics -DMY_PRECOMPILER_DIRECTIVE"
#endif
 ```

You will also need to add `c_def_my_precompiler_directive` to the list of
constants in `src/shared_data.F90`.
