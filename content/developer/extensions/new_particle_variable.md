---
title: New Particle Variable

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Extending EPOCH
    weight: 10
---

Sometimes you may wish to output a new variable associated with a
particle. Such extensions may be specific to a 
particular task, and may be too niche to be added to the core EPOCH code. 
Alternatively, users may wish to quickly add new particle variables, without
submitting a request to the developers and waiting for a new release. This
section will detail how to define a new particle variable, and how to dump it 
to SDF files.

There are multiple parts of the code which need to be modified to create and print 
a new particle variable, these are:
- `Makefile`: New pre-compiler option to deactivate new variable when not needed (for speed)
- `shared_data.F90`: Give the particle datatype a new variable
- `housekeeping/welcome.F90`: Alert the user when the code is compiled with your new option
- `housekeeping/partlist.F90`: As particles move between MPI ranks, move your new variable with them
- `constants.F90`: Create tags for the new output and compiler flag
- `deck/deck_io_block.F90`: Lets the output block see the new output variable
- `io/diagnostics.F90`: Writes the new variable to the SDF file
- `io/iterators.F90`: Gives diagnostics.F90 variables to write to file
- **Extra**: The user should set the value of the new variable to something

On this page, we provide an example of how to add and output a new particle
variable, which has actually been requested by an EPOCH user.
When electrons emit photons in the QED package, the photon macro-particle
contains no information about the electron which generated it. In this
example, let us output the Lorentz $\gamma$ factor of the radiating electron 
for each emitted photon, at the time of photon emission. We will use EPOCH2D, 
although this methodology would work for any dimension of the code. In these 
examples, we will include some of the neighbouring lines of code, so you can 
get a sense of where to insert the new code for your variable.

## Makefile

We want a new pre-compiler flag for the new addition. As macro-particles move 
around the simulation, they are passed between MPI ranks. The more particle
variables to transfer, the slower the code runs. Hence, if we don't need to use
the new particle variables, the code should be compiled without them. This
is achieved with new pre-compiler variables.

In the Makefile, there is a section with multiple commented-out pre-compiler
flags. Let us add a new one called "extended_io". To do this, insert a line
such that the Makefile reads like:

```
# Use second order particle weighting (default is third order).
#DEFINES += $(D)PARTICLE_SHAPE_TOPHAT

# Use fifth order particle weighting (default is third order).
#DEFINES += $(D)PARTICLE_SHAPE_BSPLINE3

# Include a unique global particle ID. The first flag defines the ID using
# an 8-byte integer, the second uses 4-bytes.
#DEFINES += $(D)PARTICLE_ID
#DEFINES += $(D)PARTICLE_ID4

# Include QED routines
DEFINES += $(D)PHOTONS

# Extended I/O for QED routines
DEFINES += $(D)EXTENDED_IO

# Use the Trident process for pair production
#DEFINES += $(D)TRIDENT_PHOTONS

# Include bremsstrahlung routines
#DEFINES += $(D)BREMSSTRAHLUNG
```

Note that we have un-commented the new extended_io flag, and the photons flag,
as we will have to run the code with both for the new output in this example.

## Constants.F90

Let us make new global integer flags for both the pre-compiler flag, and the 
new output.

For the `EXTENDED_IO` pre-compiler flag, find the `c_def...` list and append a 
new `c_def_extended_io` variable to the end:
```
  INTEGER(i8), PARAMETER :: c_def_use_mpi3 = 2**25
  INTEGER(i8), PARAMETER :: c_def_bremsstrahlung = 2**26
  INTEGER(i8), PARAMETER :: c_def_probe_time = 2**27
  INTEGER(i8), PARAMETER :: c_def_extended_io = 2**28  ! New line
```

Next, let us make a new output flag: `c_dump_qed_el_gamma`. Find the list of 
`c_dump...` parameters, and add our new one to the end. Remember to update the
`num_vars_to_dump` variable with the new number of dump flags.

```
  INTEGER, PARAMETER :: c_dump_probe_time        = 72
  INTEGER, PARAMETER :: c_dump_cou_log           = 73
  INTEGER, PARAMETER :: c_dump_qed_el_gamma      = 74 ! New line
  INTEGER, PARAMETER :: num_vars_to_dump         = 74 ! Modified line
```

Note, you may wish to introduce multiple outputs with one pre-compiler flag.
For example, the `WORK_DONE_INTEGRATED` pre-compiler flag introduces _six_ new
outputs.

## Shared_data.F90

Here we declare a new variable for the particle data-type. Be sure to wrap this 
inside your new pre-compiler flag using `#ifdef` commands (note these cannot be
indented - the first character of these lines must be `#`). In our example, we 
have called the new variable `electron_gamma`.

```
  ! Object representing a particle
  ! If you add or remove from this section then you *must* update the
  ! particle pack and unpack routines
  TYPE particle
    REAL(num), DIMENSION(3) :: part_p
    REAL(num), DIMENSION(c_ndims) :: part_pos
#ifdef EXTENDED_IO 
    REAL(num) :: electron_gamma ! New line, and the surrounding ifdef, endif
#endif
```

## Welcome.F90

For consistency with the other EPOCH pre-compiler options, we should print a 
message when the code has been compiled with our new `EXTENDED_IO`.

The first part of the module scans through all possible pre-compiler variables,
and checks if any are present. Add new lines for the new pre-compiler flag.
```
#ifdef PHOTONS
    found = .TRUE.
#ifdef TRIDENT_PHOTONS
    found = .TRUE.
#endif
#endif
#ifdef EXTENDED_IO 
    found = .TRUE. ! New line
#endif
```

Next, a message is printed for each present module. Here we have written a
generic message for our extended_io pre-compiler flag.
```
#ifdef PHOTONS
    defines = IOR(defines, c_def_photons)
    WRITE(*,*) 'QED Effects -DPHOTONS'
#ifdef TRIDENT_PHOTONS
    defines = IOR(defines, c_def_trident_photons)
    WRITE(*,*) 'Pair production by Trident process -DTRIDENT_PHOTONS'
#endif
#endif
#ifdef EXTENDED_IO
    defines = IOR(defines, c_def_extended_io)                           ! New line
    WRITE(*,*) 'Additional particle variables for output -DEXTENDED_IO' ! New line
#endif
```

## Partlist.F90

Each MPI rank only tracks particles present within its section of the domain. 
As macro-particles move around the simulation, they can pass between cells 
controlled by different ranks. When this happens, a new particle is created on 
the new rank, its properties are set to match the properties on the old rank, 
and the particle on the old rank is destroyed. New particle variables must 
also be transferred when particles move between ranks, and this is done here.

There are 4 steps here:
- Tell the code how many variables are associated with the particle
- Copy the variables on the old rank to an array for transfer
- Set variables on the new rank from the transferred array
- For new particles, initialise the particle variable

To tell the code how many variables are present, find the `set_partlist_size` 
subroutine. The count of particle variables is stored in the `nvar` integer.
In this example, we only introduce one more particle variable when the code 
is compiled with `EXTENDED_IO`, so add 1 to `nvar` if we have compiled with
`EXTENEDED_IO`:

```
#ifdef BREMSSTRAHLUNG
    nvar = nvar+1
#endif
#ifdef EXTENDED_IO 
    nvar = nvar+1 ! New line
#endif
#ifdef PROBE_TIME
    nvar = nvar+1
#endif
```

Next, navigate to `pack_particle` - the subroutine responsible for collecting
particle variables to transfer to neighbouring ranks. Now we can add our new
variable to the transfer array. Make a note of the position here - variables 
must be read in the same order they are stored.

```
#ifdef BREMSSTRAHLUNG
    array(cpos) = a_particle%optical_depth_bremsstrahlung
    cpos = cpos+1
#endif
#ifdef EXTENDED_IO
    array(cpos) = a_particle%electron_gamma ! New line
    cpos = cpos+1                           ! Only 1 new variable, so add 1
#endif
#ifdef PROBE_TIME
    array(cpos) = a_particle%probe_time
    cpos = cpos+1
#endif
```

Once the particle variables are packed into an array, this is sent to the 
neighbouring ranks. These call `unpack_particle` to set the properties of new 
particles. The syntax for this with our new variable would be:

```
#ifdef BREMSSTRAHLUNG
    a_particle%optical_depth_bremsstrahlung = array(cpos)
    cpos = cpos+1
#endif
#ifdef EXTENDED_IO
    a_particle%electron_gamma = array(cpos) ! New line
    cpos = cpos+1  ! Note this is also between optical_depth_bremsstrahlung and probe_time
#endif
#ifdef PROBE_TIME
    a_particle%probe_time = array(cpos)
    cpos = cpos+1
#endif
```

Finally, we must set the initial values for these variables. If we don't do
this, the code can act unpredictably. In the `init_particle` subroutine, add 
the lines:

```
#ifdef EXTENDED_IO 
    new_particle%electron_gamma = 0.0_num    ! New line
#endif
#ifdef PROBE_TIME
    new_particle%probe_time = 0.0_num
#endif
```

## Deck_io_block.F90

Unless you want to make a new physics package, a new particle variable is only
useful if you can output it. This is done using the standard EPOCH output block.
Recall we've already defined a `c_dump_qed_el_gamma` flag - now we just need
to tell EPOCH how it will appear in the input deck.

Navigate to `io_block_handle_element`, find the `str_cmp` lines which interpret
the the deck lines, and add:

```
#ifdef BREMSSTRAHLUNG
    ELSE IF (str_cmp(element, 'bremsstrahlung_optical_depth')) THEN
      elementselected = c_dump_part_opdepth_brem
#endif

#ifdef EXTENDED_IO
    ELSE IF (str_cmp(element, 'electron_gamma')) THEN  ! New line
      elementselected = c_dump_qed_el_gamma            ! New line
#endif
```

Whatever you write as the second argument to `str_cmp` is how you will 
have to write the variable in the EPOCH output block. As we are keeping things 
simple in this example, we will not bother with restart dumps. The value of 
`electron_gamma` will not be re-loaded when you restart the code from a
restart dump.

## Diagnostics.F90

At this point we have declared a new particle variable, wrapped it in
pre-compiler flags, made it visible to MPI routines, and told EPOCH it can be
dumped. Now we actually have to write the variable to file.

There is a section in the `output_routines` subroutine which contains lines
with `write_particle_variable` calls. Add a new one for our variable:

```
#ifdef BREMSSTRAHLUNG
        CALL write_particle_variable(c_dump_part_opdepth_brem, code, &
            'Bremsstrahlung Depth', '', it_output_real)
#endif
#ifdef EXTENDED_IO
        CALL write_particle_variable(c_dump_qed_el_gamma, code, &
            'Emitting Electron Gamma', '', it_output_real)
#endif
```

The `'Emitting Electron Gamma'` string controls how the variable will be
labelled in the SDF file.

## Iterators.F90

One of the arguments of the `write_particle_variable` subroutine is a call to 
the `it_output_real` function. We need to modify this function to give our new
variable, when requested. To do this, add the following case to the 
`it_output_real` function:

```
#ifdef EXTENDED_IO
      CASE (c_dump_qed_el_gamma)
        DO WHILE (ASSOCIATED(cur) .AND. (part_count < npoint_it))
          part_count = part_count + 1
          array(part_count) = cur%electron_gamma
          cur => cur%next
        END DO
#endif
```

## Setting your variable

All the previous steps are required whatever your particle
variable is. All we need to do now is figure out how to set it. This may be 
straightforward or complicated, depending on what the variable is. We can't
cover every possible particle variable here, but you may find it useful if we 
finish our example, to see how one would go about it.

Here, we want to save the electron gamma factor when a photon has been emitted.
Photon emission occurs in the `generate_photon` subroutine of `photons.F90`. 
When in this subroutine, the generating electron information can be accessed 
through the `generating_electron` particle pointer. Fortunately for us, the
electron $\gamma$ factor has already been calculated in this subroutine, and is
given by `generating_gamma`. Let us set our new particle variable to this
value, as soon as the photon macro-particle has been created.

```
      CALL create_particle(new_photon)
      new_photon%part_pos = generating_electron%part_pos

#ifdef EXTENDED_IO 
      new_photon%electron_gamma = generating_gamma     ! New line
#endif
```

## Testing

If all has gone well, we have a new particle variable ready for printing.
Compile EPOCH (you may need to run `make clean` first if you have changed the 
pre-compiler flags).

It's best to run with a small input deck for code testing. We have shrank the
qed_rese.deck found in the source code, such that it can run in a few seconds.
This tiny deck has been included at the end of this section.

When running, the code correctly generates the compiler-flag messages:

```
 Welcome to EPOCH2D version 4.19.1   (commit v4.19.1-7-g6a1bfa99-dirty)

 The code was compiled with the following compile time options
 *************************************************************
 QED Effects -DPHOTONS
 Additional particle variables for output -DEXTENDED_IO
 *************************************************************
```

When we inspect `0001.sdf`, we find that 21798 macro-photons have been created,
and all of them have an emitting electron gamma value. These range from 20 to 
1896 in this example. The input deck used for this example is given below. A 
new particle variable has been successfully created!

```
begin:control
  nx = 50 # in x
  ny = 50
  nparticles = nx * ny * 2
  t_end = 100e-15
  x_min = 0
  x_max = 50 * 10.0e-9
  y_min = 0
  y_max = 50 * 10.0e-9
  dt_multiplier = 0.8
end:control

begin:qed
  use_qed = T 
  qed_start_time = 0 
  produce_photons = T
  photon_energy_min = 50 * kev 
  produce_pairs = F 
  photon_dynamics = F 
end:qed

begin:boundaries
  bc_x_min = simple_laser
  bc_x_max = reflect
  bc_y_max = reflect
  bc_y_min = reflect
end:boundaries

begin:species
  name = Electron
  fraction = 0.5
  dump = T
  temperature = 0
  number_density = if (x gt 0.1*x_max, 1.0e20, 0)
  identify:electron
end:species

begin:species
  name = Ion
  fraction = 0.5
  dump = T
  number_density = number_density(Electron)
  temperature = 0
  identify:proton
end:species

begin:species
  name = Photon
  frac = 0
  dump = T
  identify:photon
end:species

begin:output
  dt_snapshot = t_end
  electron_gamma = always
end:output

begin:laser
  boundary = x_min
  intensity = 1.0e21 * 1.0e4
  lambda = 1.0e-6
  polarisation = 0.0
  phase = 0.0
  t_profile = 1
  profile = 1
end:laser
```
