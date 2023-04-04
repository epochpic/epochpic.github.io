---
title: Custom Deck

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Input Output
    weight: 40
---
For some types of changes to the code it is more convenient to have the end
user pass new parameters into the code. This can be for several reasons, and
the section on permanent additions to the input deck is given in extensions. At 
this stage, we will describe how to temporarily add new
elements to the input deck parser routines, allowing parameterising of
internal and manual initial conditions.

Custom input deck elements are setup in the file
`src/user_interaction/custom_deck.f90`. The function
`custom_blocks_handle_element` is called when a new block is
started which the core parser is not familiar with, and once for each element
of a block. The function `custom_blocks_check` is called once the
entire deck has been parsed and is used to check that all the elements which
are required for the code to run have been specified.

## custom_blocks_handle_element
There are three parameters passed to
`custom_blocks_handle_element`, which are:
 
-  block_name - The name of the block specified in the
  `begin:blockname` part of the input deck.
-  element - The name of the element in an input deck
  `element = value` pair.
-  value - The string representation of the value in an input deck
  `element = value` pair.
  

`custom_blocks_handle_element` is first called when a new block
is begun
using `begin:blockname` and "blockname" is not recognised by the
core input deck parser. The first thing that it does is test whether or not it
is a valid custom block. The code does this by passing in the blockname with
`element` and `value` set to the special constant called
"blank". When extending the input deck, an end user should check if either
`element` or `value` are set to the special constant
"blank", and if they are then test to see whether the blockname is known or
not. If the blockname is known then the code should return the error code
`c_err_none` (No error). If the blockname is not known then the
code should return `c_err_unknown_block` and the deck parser will
just skip the block. In operation, this looks like:
 ```perl
FUNCTION custom_blocks_handle_element(block_name, element, value) &
    RESULT(errcode)

  CHARACTER(LEN=string_length), INTENT(IN) :: block_name, element, value
  INTEGER :: errcode

  IF (str_cmp(block_name, "custom")) THEN
    IF (element .EQ. blank .OR. value .EQ. blank) THEN
       ! If element or value are blank then just testing block so
       ! return c_err_none
       errcode = c_err_none
       RETURN
     ENDIF
   ENDIF

  ! The following line must always be present
  errcode = c_err_unknown_block

END FUNCTION custom_blocks_handle_element
 ```

In order to simplify Fortran's rather annoying string handling behaviour,
several helper functions have been defined and the most used one is
`str_cmp(string1, string2)`. This is a simple routine which returns
true if string1 == string2 and false otherwise. It is case sensitive but can
deal with differing string lengths etc. The next stage is to deal with the
actual `element = value` pairs in the deck. Each time that a new pair
is read from the deck, `custom_blocks_handle_element` is called
with `element` and `value` having the values read from the
deck. To test for known elements they should just be checked against a known
list of names using `str_cmp` and return the error code
`c_err_unknown_element` if the element isn't a known element. This
looks like:
 ```perl
FUNCTION custom_blocks_handle_element(block_name, element, value) &
    RESULT(errcode)

  CHARACTER(LEN=string_length), INTENT(IN) :: block_name, element, value
  INTEGER :: errcode

  IF (str_cmp(block_name, "custom")) THEN
    IF (element .EQ. blank .OR. value .EQ. blank) THEN
      ! If element or value are blank then just testing block so
      ! return c_err_none
      errcode = c_err_none
      RETURN
    ENDIF
    errcode = c_err_unknown_element
    ! Now test for the real elements
    IF (str_cmp(element, "int_element")) THEN
      errcode = c_err_none
    ENDIF
    IF (str_cmp(element, "real_element")) THEN
      errcode = c_err_none
    ENDIF
    IF (str_cmp(element, "logical_element")) THEN
      errcode = c_err_none
    ENDIF
    RETURN
  ENDIF

  ! The following line must always be present
  errcode = c_err_unknown_block

END FUNCTION custom_blocks_handle_element
 ```

This version of the code will allow you to add a new block called "custom"
with elements
"int_element", "real_element" and "logical_element" and the
code will parse them successfully, while any other block or any other element
in the block "custom" will throw errors. However, at this stage the code
doesn't actually read any of the values from the deck. To make it useful, any
variable which is read from the input deck must be stored in a global
variable. Defining global variables are explained in more detail in the
relevant section of the manual, but in short, any variable defined in the
module `shared_data` in the file `src/shared_data.F90`
will be a global variable. After the variables have been setup, there are once
again helper functions to make converting the text from the deck into a normal
Fortran90 variable. These helper functions are:

 
-  as_integer - Attempts to convert a string to an integer. Invokes the
  maths parser.
-  as_real - Attempts to convert a string to a REAL(num). Invokes the maths
  parser.
-  as_logical - Attempts to convert a string to a logical. Does not invoke
  the maths parser (must be either "T" or "F").
  

They are used pretty much as expected, except that the return value is passed
to the functions so that they can report errors while trying to parse the
string. An example would then be:
 ```perl
FUNCTION custom_blocks_handle_element(block_name, element, value) &
    RESULT(errcode)

  CHARACTER(LEN=string_length), INTENT(IN) :: block_name, element, value
  INTEGER :: errcode, int_element
  REAL(num) :: real_element
  LOGICAL :: logical_element

  IF (str_cmp(block_name, "custom")) THEN
    IF (element .EQ. blank .OR. value .EQ. blank) THEN
      ! If element or value are blank then just testing block so
      ! return c_err_none
      errcode = c_err_none
      RETURN
    ENDIF
    errcode = c_err_unknown_element
    ! Now test for the real elements
    IF (str_cmp(element, "int_element")) THEN
      errcode = c_err_none
      int_element = as_integer(value, errcode)
    ENDIF
    IF (str_cmp(element, "real_element")) THEN
      errcode = c_err_none
      real_element = as_real(value, errcode)
    ENDIF
    IF (str_cmp(element, "logical_element")) THEN
      errcode = c_err_none
      logical_element = as_logical(value, errcode)
    ENDIF
    RETURN
  ENDIF

  ! The following line must always be present
  errcode = c_err_unknown_block

END FUNCTION custom_blocks_handle_element
 ```

It is possible to perform more advanced types of evaluation of maths
expressions such as reading arrays etc. but this is beyond the scope of this
manual at present.

## custom_blocks_check
This function is called when all the blocks in the input deck have been
evaluated and is used to check that all required parameters have been set. If
all required elements have been set then you should just return
`c_err_none`, otherwise return
`c_err_missing_elements`. How you test that required elements have
been set is up to the developer, and for testing and personal use (which is
all that the custom deck parts of the code should be used for) it is
acceptable to just not check and always return `c_err_none`. If
permanently expanding the deck, error trapping should always be written.