---
title: Custom Maths Parser

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Input Output
    weight: 30
---

Sometimes the complexity in changing the input.deck file is due to the fact that
a function which must be used is fairly complex in form and is not supplied
with the core code. It must therefore be represented in the input deck maths
parser. This can be a significant cause of complexity for some problems, and
in this case, there are three options: 
1. Put up with it and implement in the deck
2. Use the internal initial conditions rather than the deck
3. Extend the maths parser to include your function 

Extending the maths parser can either
be permanent (described in extensions) or temporary (described
here). All of the routines used in extending the maths parser are in the file
`user_interaction/custom_parser.f90`. Temporarily adding elements to the parser 
is much easier than a permanent addition. It is
possible to add new constants and functions to the maths parser. It is hoped
that in a future release of EPOCH this will be extended to allow custom
operators as well. 

As an example, lets look at adding a new function (lorentz) for a
Lorentzian distribution, and adding a new constant, phi.

## Registering your new constant/function

Before a new constant or function
can be defined it must be registered. In the registration phase the text
representation of the function or constant is given to the parser subroutines
and the user is returned an integer handle for the registered object. The
numerical handle must be stored so that that all of the functions in this
module can access it, so they should be placed after the `IMPLICIT
NONE` statement at the top of the file and defined as:
 ```perl
INTEGER :: c_func_lorentz
INTEGER :: c_const_phi
 ```

Note that the names given to the constants is obviously at the developers
discretion, but these names comply with the EPOCH style guide.
Actually registering the objects is done in the `register_objects`
subroutine which should include lines to register functions and constants.
An example is given below.
 ```perl
SUBROUTINE register_objects

  c_func_lorentz = register_function("lorentz")
  c_const_phi = register_constant("phi")

END SUBROUTINE register_objects
 ```

Note that the input deck parser is case sensitive, so the strings which are
given to `register_function` and `register_constant`
should be in the case that they will appear in the input deck. To follow the
EPOCH style guide this should be all lowercase. At this point, the maths
parser would start to recognise the new function/constant, but would still
give error messages since they haven't yet been implemented.

## Setting up new constants

Once a new constant has been registered it must be described using the
`custom_constant` function. In 2D this function looks like:
 ```perl
FUNCTION custom_constant(opcode, ix, iy, errcode)

  INTEGER, INTENT(IN) :: opcode, ix, iy
  INTEGER, INTENT(INOUT) :: errcode
  REAL(num) :: custom_constant

  ! Leave these lines in place. They cause the code to throw an error if
  ! The opcode is unknown
  custom_constant = 0.0_num
  errcode = IOR(errcode, c_err_unknown_element)

END FUNCTION custom_constant
 ```

The parameters are

 
-  `opcode` - The operator code of the constant requested. This will be the
  integer handle returned from `register_constant`.
-  `ix`, `iy`, `iz` - Some constants are actually evaluated at specific points in
  space and ix, iy, iz are the gridpoint number of the location currently being
  evaluated. If you are specifying a simple constant then just ignore
  these. If your constant does depend upon space then directly subscript your
  array with ix, iy, iz as needed to read the correct location.
-  `errcode` - The error code which should be passed back to the
  parser. If for some reason you cannot evaluate your constant then you should
  `IOR` errcode with the appropriate error code (all the error
  codes are listed in appendix A). Note that errcode should never be SET to
  any specific error code when extending the parser, since this might
  overwrite errors put in place earlier in the parsing sequence. This is
  different to extending the input deck where the error code is set.
  

The function should just return the evaluated value of the constant requested
by `opcode`. This might look like:
 ```perl
FUNCTION custom_constant(opcode, ix, iy, errcode)

  INTEGER, INTENT(IN) :: opcode, ix, iy
  INTEGER, INTENT(INOUT) :: errcode
  REAL(num) :: custom_constant

  IF (opcode .EQ. c_const_phi) THEN
    custom_constant = pi
    RETURN
  ENDIF

  ! Leave these lines in place. They cause the code to throw an error if
  ! The opcode is unknown
  custom_constant = 0.0_num
  errcode = IOR(errcode, c_err_unknown_element)

END FUNCTION custom_constant
 ```

Note that when `opcode` is successfully recognised, the code sets
the return value and returns straight away. This is how all constants should
work, since the last line forces the function to return an error code. This
last line is in place to trap people registering constants but never defining
them. Without this line, it is possible to define a constant which is
never specified and have the code complete OK with a random value for that
constant.

The constant "phi" should now work fine when used anywhere in the input deck
and will return a value of $\pi$.

## Setting up new functions

Setting up the new function `lorentz` is very similar to setting up
the new constant. The relevant function is `custom_function` and
when empty looks like:
 ```perl
FUNCTION custom_function(opcode, ix, iy, errcode)

  INTEGER, INTENT(IN) :: opcode, ix, iy
  INTEGER, INTENT(INOUT) :: errcode
  REAL(num) :: custom_function
  REAL(num) :: values(5)

  ! Leave these lines in place. They cause the code to throw an error if
  ! The opcode is unknown
  custom_function = 0.0_num
  errcode = IOR(errcode, c_err_unknown_element)

END FUNCTION custom_function
 ```

The parameters are
 
-  `opcode` - The operator code of the constant requested. This will be the
  integer handle returned from \inlinecode {register_function}.
-  `ix`, `iy`, `iz` - Some functions are evaluated differently at specific points
  in space and ix, iy, iz are the gridpoint number of the location currently
  being evaluated. If you are specifying a simple function then just
  ignore these. If your function does depend upon space then directly
  subscript your array with ix, iy, iz as needed to read the correct location.
-  `errcode` - The error code which should be passed back to the
  parser. If for some reason you cannot evaluate your function then you should
  `IOR` errcode with the appropriate error code. Note that errcode
  should never be SET to any specific error code, since this might overwrite
  errors put in place earlier in the parsing sequence.
  
The function should return the value of your evaluated constant. The
parameters which are passed to the function can be retrieved by the function
`get_values(n, values)`, where `n` is the number of
parameters to be returned and `values` is a `REAL(num)`
array of length `n` which will hold the returned values .  In this
implementation of the Lorentzian function there are three parameters: The
dependent variable, the location parameter and the scale parameter. The code
to implement the function therefore looks like:
 ```perl
FUNCTION custom_function(opcode, ix, iy, errcode)

  INTEGER, INTENT(IN) :: opcode, ix, iy
  INTEGER, INTENT(INOUT) :: errcode
  REAL(num) :: custom_function
  REAL(num) :: values(5)

  IF (opcode .EQ. c_func_lorentz) THEN
    CALL get_values(3, values(1:3))
    ! values(1) - Dependent variable
    ! values(2) - location parameter
    ! values(3) - scale parameter
    custom_function = values(3)**2/((values(1)-values(2))**2+values(3)**2)
    RETURN
  ENDIF

  ! Leave these lines in place. They cause the code to throw an error if
  ! The opcode is unknown
  custom_function = 0.0_num
  errcode = IOR(errcode, c_err_unknown_element)

END FUNCTION custom_function
 ```

This function is then available at any point in the input deck and if I return
to the previous example ic.deck file, it would be used as follows:
 ```perl
begin:constant
   particle_density = 100.0 # Particle number density
   profile_x = lorentz(x,0.0,1.0)
   profile_y = lorentz(y,0.0,1.0)
end:constant

begin:species
   name = s1

   # multiply density by real particle density
   density = particle_density * profile_x * profile_y

   # Set the temperature to be zero
   temp_x = 0.0
   temp_y = temp_x(s1)

   # Set the minimum density for this species
   density_min = 0.3*particle_density
end:species

begin:species
   name = s2

   # Just copy the density for species s1
   density = density(s1)

   # Just copy the temperature from species s1
   temp_x = temp_x(s1)
   temp_y = temp_y(s1)

   # Set the minimum density for this species
   density_min = 0.3*particle_density
end:species
 ```

It is therefore clear that the new lorentz function is essentially the same as
the built in gauss function. Note that due to the way that the parser works,
the end user is not required to deal with parameters which are themselves
maths expressions. They have been fully evaluated by the time they are
returned by `get_values`. Note that the parser is not guaranteed to
be bulletproof. If a user calls `get_values` requesting more
parameters than have been passed to the function then it will scramble the
stack which is used by the parser and cause the code to fail. Note that
calling `get_values(2, values)` is not the same as calling
`get_values(1, values)` twice, in fact calling
`get_values(1, values)` multiple times will return the parameters in
_reverse_ order. This is normal and is a feature of how the maths parser
operates. It is possible to use this property to write functions which have a
variable number of parameters, but this is not recommended.