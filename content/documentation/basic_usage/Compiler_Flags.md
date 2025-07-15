---
draft: false
toc: true
type: docs

title: Compiler flags and preprocessor defines
linktitle: Compiler Flags
weight: 50
menu:
  documentation:
    parent: Basic usage
    weight: 50
---

As already stated, some features of the code are controlled by compiler
preprocessor directives. The flags for these preprocessor directives are
specified in "Makefile" and are placed on lines which look like the
following:

    DEFINES += $(D)PER_SPECIES_WEIGHT

On most machines `$(D)` just means `-D` but the variable is required to
accommodate more exotic setups.

Most of the flags provided in the "Makefile" are commented out by
prepending them with a "`#`" symbol (the "make" system's comment
character). To turn on the effect controlled by a given preprocessor
directive, just uncomment the appropriate "DEFINES" line by deleting
this "`#`" symbol. The options currently controlled by the preprocessor
are:

-   PER_SPECIES_WEIGHT - By default, each pseudoparticle in the code
    can represent a different number of real particles. Memory can be
    saved by disabling this feature and have all of the pseudoparticles
    in a species use the same particle weight. Many of the codes more
    advanced features require per-particle weighting so it is enabled by
    default. Use this flag to disable per- particle weighting if you
    need to save on memory, but it this option is recommended only for
    advanced users.
-   NO_TRACER_PARTICLES - This flag will disable the option to specify
    one or more species as zero-current particles. Zero-current
    particles move about as would a normal particle with the same charge
    and mass, but they do not generate any current and are therefore
    passive elements in the simulation. Zero-current particles should be
    included in collisions to ensure they move identically to ordinary
    particles. The implementation of zero-current particles requires an
    additional "IF" clause in the particle push, so it has a slight
    performance impact. If you do not require the feature then setting
    this flag will give a slight performance improvement.
    <span style="color: red; font-weight: bold;">WARNING:</span> Since
    the particles effectively have zero weight in terms of their
    numerical heating properties, they do not always behave in the same
    way that an ordinary particle with weight would behave and this can
    sometimes lead to unexpected behaviour. If the purpose is merely to
    track a subset of a particle species to use as output then a better
    mechanism to use is "persistent subsets" (see
    [here][Input_deck_subset]). In version 5.0, this
    flag will be and replaced with "ZERO_CURRENT_PARTICLES".
-   NO_PARTICLE_PROBES - For laser plasma interaction studies it can
    sometimes be useful to be able to record information about particles
    which cross a plane in the simulation. Since this requires the code
    to check whether each particles has crossed the plane in the
    particles pusher and also to store copies of particles until the
    next output dump, it is a heavyweight diagnostic. If you don't
    require the diagnostic you can set this flag to disable it.
-   PROBE_TIME - Paticle probes also output the time particles pass the probe.
    Without this key, only the time of the SDF output dump will be available.
-   PARTICLE_SHAPE_TOPHAT - By default, the code uses a first order
    b-spline (triangle) shape function to represent particles giving
    third order particle weighting. Using this flag changes the particle
    representation to that of a top-hat function (0th order b-spline
    yielding a second order weighting).
-   PARTICLE_SHAPE_BSPLINE3 - This flag changes the particle
    representation to that of a 3rd order b-spline shape function (5th
    order weighting).
-   PARTICLE_ID - When this option is enabled, all particles are
    assigned a unique identification number when writing particle data
    to file. This number can then be used to track the progress of a
    particle during the simulation.
-   PARTICLE_ID4 - This does the same as the previous option except it
    uses a 4-byte integer instead of an 8-byte one. Whilst this saves
    storage space, care must be taken that the number does not overflow.

-   PHOTONS - This enables support for photon particle types in the
    code. These are a pre-requisite for modelling synchrotron emission,
    radiation reaction and pair production (see
    [here][Input_deck_qed]).
-   TRIDENT_PHOTONS - This enables support for virtual photons which
    are used by the Trident process for pair production.
-   BREMSSTRAHLUNG - Similar to photons, but for modelling bremsstrahlung
    radiation instead of QED emission.
-   PREFETCH - This enables an Intel-specific code optimisation.
-   PARSER_DEBUG - The code outputs more detailed information whilst
    parsing the input deck. This is a debug mode for code development.
-   PARTICLE_DEBUG - Each particle is additionally tagged with
    information about which processor it is currently on, and which
    processor it started on. This is a debug mode for code development.
-   MPI_DEBUG - This option installs an error handler for MPI calls
    which should aid tracking down some MPI related errors.
-   SIMPLIFY_DEBUG - This option enables debugging code related to the
    deck parser simplification routine.
-   NO_IO - This option disables all file I/O which can be useful when
    doing benchmarking.
-   COLLISIONS_TEST - This enables some routines for debugging the
    collision routines. It completely alters the behaviour of the code.
    This flag should never be enabled by the end user. Removed in
    v4.19.7.
-   PER_PARTICLE_CHARGE_MASS - By default, the particle charge and
    mass are specified on a per-species basis. With this flag enabled,
    charge and mass become a per-particle property. This is a legacy
    flag which will be removed soon.
-   PARSER_CHECKING - Setting this flag adds code which checks for
    valid values on evaluated deck expressions. This slows down the code
    but may be required if floating point exceptions are enabled.
-   WORK_DONE_INTEGRATED - This enables support for tracking the work
    done on each particle by the electric field. Note that this
    increases the size of each particle by 48 bytes. The information
    gathered can be output to file using the "work_{x,y,z}" and
    "work_{x,y,z}_total" dumpmasks. See
    [here][Input_deck_output_block__particle_variables]
-   DELTAF_METHOD - Compile the code to use the delta-f method to
    represent particles rather than standard PIC. Note that this
    completely changes the code behaviour and should not be enabled for
    normal use. See [here][Using_delta_f].
-   DELTAF_DEBUG - Add debug code for the delta-f method.

-   HC_PUSH - Use the push from [Higuera and
    Cary](https://doi.org/10.1063/1.4979989) rather than the Boris push.
    This is slightly slower than the Boris push but gives the correct
    $\mathbf{E} \times \mathbf{B}$ velocity, improving performance for
    highly relativistic simulations.

-   NO_USE_ISATTY - When printing the initial welcome message, EPOCH
    makes use of the C-library's isatty function. This requires
    Fortran2003 features that might not be available on all platforms.
    The flag allows this functionality to be disabled on platforms that
    don't support it.
-   NO_MPI3 - This compiler flag allows the user to disable MPI-3
    features such as the "MPI_TYPE_SIZE_X" routine. This allows the
    code to be compiled against older versions of the MPI library. The
    flag should only be enabled if the code fails to compile without it.

# Changing precompiler directives

If a user has already compiled EPOCH, and would like to change the active
compiler flags, then the user may comment and uncomment different flags in the
Makefile (by adding or removing # at the start of the line), and recompiling the
code using the commands:

```perl
make clean
make COMPILER=gfortran -j4
```

Where in this example, the gfortran compiler has been chosen.

# Errors for unspecified precompiler directives {#errors_for_unspecified_precompiler_directives}

If a user requests an option which the code has not been compiled to
support then the code will give an error or warning message as follows:

     *** ERROR ***
     Unable to set "use_qed=T" in the "qed" block.
     Please recompile with the -DPHOTONS preprocessor flag.

# Other Makefile flags {#other_makefile_flags}

It is also possible to pass other flags to the compiler. In "Makefile"
there is a line which reads

    FFLAGS = -O3 -fast

The two commands to the right are compiler flags and are passed
unaltered to the FORTRAN compiler. Change this line to add any
additional flags required by your compiler.

By default, EPOCH will write a copy of the source code and input decks
into each restart dump. This can be very useful since a restart dump
contains an exact copy of the code which was used to generate it,
ensuring that you can always regenerate the data or continue running
from a restart. The output can be prevented by using "dump_source_code
= F" and "dump_input_deck = F" in the output block. However, the
functionality is difficult to build on some platforms so the Makefile
contains a line for bypassing this section of the build process. Just
below all the DEFINE flags there is the following line:

    #ENCODED_SOURCE = epoch_source_info_dummy.o

Just uncomment this line and source code in restart dumps will be
permanently disabled.



<!-- ########################  Cross references  ######################## -->


[Input_deck_output_block__particle_variables]: /documentation/input_deck/input_deck_output_block#particle_variables
[Input_deck_qed]: /documentation/input_deck/input_deck_qed
[Input_deck_subset]: /documentation/input_deck/input_deck_subset
[Using_delta_f]: /documentation/code_details/using_delta_f
