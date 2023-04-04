---
title: String Handling

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Input Output
    weight: 20
---

Fortran is not a language famous for its string handling capabilities, but due
to the presence of the input deck EPOCH has fairly extensive string handling
routines. Strings used are all of the standard Fortran `CHARACTER`
type and are defined as:
 ```perl
CHARACTER(LEN=string_length) :: string
 ```
`string_length` is a global constant defined in
`src/constants.F90` which can be increased to allow EPOCH to
handle longer strings. There may be reasons to increase this length if you wish
to use long complex expressions in the input deck. Note that many Fortran
compilers do not allow strings to exceed 512 characters in length.

Listed here are the string handling routines (other than those in the core
maths parser routine which are documented elsewhere) which are currently used
in EPOCH. These functions can be found in `src/deck/strings.f90` and 
`src/deck/strings_advanced.f90` 

## str_cmp

```perl
FUNCTION str_cmp(str_in, str_test)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in, str_test
LOGICAL :: str_cmp
```
  
`str_cmp` is the routine which does all the string comparisons in
EPOCH. It deals with leading and trailing whitespace automatically and
tests for length differences. It does not test for strings being
valid substrings of each other, only for full equality.
  
A developer should always use `str_cmp` rather than doing their own
string testing to ensure consistent behaviour across the entire EPOCH code
base.

## as_real_simple

```perl
FUNCTION as_real_simple(str_in, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
REAL(num) :: as_real_simple
```
  
`as_real_simple` is a routine to convert a string into a real
number without invoking the maths parser. It can cope with standard form as
well as simple decimal reals. It is significantly faster than the maths parser,
but should only be used when the user explicitly _shouldn't_ be able to use
a mathematical expression. If the string cannot be parsed then the routine sets
the bitmask `c_err_bad_value` on the parameter `err`
  
If you have a string which has to be converted into a real quickly then this is
the routine to use. You probably shouldn't use it when parsing a string from
the input deck, since there is no reason to restrict the user from specifying a
mathematical expression. The routine is used inside the maths parser to parse
simple numbers.


## as_integer_simple

```perl
FUNCTION as_integer_simple(str_in, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
INTEGER :: as_integer_simple
```

`as_integer_simple` is a routine to convert a string into an
integer without invoking the maths parser. It can cope with standard form as
well as simple decimal integers. It is significantly faster than the maths
parser, but should only be used when the user explicitly _shouldn't_ be
able to use a mathematical expression. If the string cannot be parsed then the
routine sets the bitmask `c_err_bad_value` on the parameter
`err`
   
This routine is used internally in several parts of the code when parsing
things like numbers which are parts of strings (i.e. the 1 in
`direction1` for distribution functions). It probably shouldn't
be used to directly parse input deck parameters, since there is no reason to
restrict the user from specifying mathematical expressions.

## as_long_integer_simple

```perl
FUNCTION as_long_integer_simple(str_in, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
INTEGER(KIND=8) :: as_long_integer_simple
```
 
`as_long_integer_simple` is equivalent to
`as_integer_simple`, but returns the larger
`INTEGER(KIND=8)` rather than a normal `INTEGER(KIND=4)`.
   
## as_boundary

```perl
FUNCTION as_boundary(str_in, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
INTEGER :: as_boundary
```
  
`as_direction` is used when assigning a laser to a boundary and
recognises the strings
 
-  x_min or left - c_bd_x_min.
-  x_max or right - c_bd_x_max.
-  y_min or down - c_bd_y_min.
-  y_max or up - c_bd_y_max.
-  z_min or back - c_bd_z_min.
-  z_max or front - c_bd_z_max.
  
It returns the associated direction code (given after the dash in the
definition).
   
  
If you're writing code which requires attaching something to a boundary,
whether a boundary condition, a diagnostic or some other routine, then this is
the routine that should be used. Note that in order to prevent confusion when
moving input decks between different dimension versions of EPOCH, each code
only recognises the strings for boundaries that it actually has.

## as_logical

```perl
FUNCTION as_logical(str_in, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
Logical :: as_logical
```
  
`as_logical` simply tests for the strings "T" and "F" to
determine a boolean value. The default behaviour of `as_logical` is
to treat any string that isn't "T" as a false value.
   
You should use this rather than using a 0/1 boolean flag in the input deck for
consistency.

## split_off_int

```perl
SUBROUTINE split_off_int(str_in, str_out, int_out, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
CHARACTER(LEN=*), INTENT(OUT) :: str_out
INTEGER, INTENT(OUT) :: int_out
INTEGER, INTENT(INOUT) :: err
```
  
`split_off_int` is a routine which splits a string of the format
`string n` into a string `string` and an integer `n` which are
returned separately in the `str_out` and `int_out`
parameters respectively.  If it can't split the string successfully then it
sets the `c_err_bad_value` bitfield of the err parameter.
   
This is used in the core of the deck parser to deal with blocks like the
numbered species blocks in the initial conditions, and also in some of the
specific block parsers. Again, this routine should be used to split strings
like this rather than coding a new routine.

## split_range

```perl
SUBROUTINE split_range(str_in, real1, real2, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
REAL(num), INTENT(OUT) :: real1, real2
INTEGER, INTENT(INOUT) :: err
```
  
`split_range` is a routine which splits a string of the format
`(n, m)` into two reals `n` and `m` which are returned
separately in the `real1` and `real2` parameters
respectively.  If it can't split the string successfully then it sets the
`c_err_bad_value` bitfield of the err parameter.
   
This is used when specifying ranges in the input decks at present. Any
ranges which should be specified in a single parameter should be specified in
this form and this routine used to split the string.

## as_integer

```perl
FUNCTION as_integer(str_in, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
INTEGER :: as_integer
```
  
`as_integer` is the routine which returns integers from strings
using the maths parser. If a mathematical expression resolves to a non-integer
result then this routine rounds to the NEAREST integer. There are explicit
rounding routines in the maths parser to force other behaviour.
   
This routine should be used when evaluating most strings into integers. Note
that this routine evaluates spatially dependent quantities at (0,0) on each
processor, so will give unpredictable results when spatially dependent
quantities are given to it (like density, bx etc.). To evaluate a spatially
varying quantity use `evaluate_string_in_space`.

## as_long_integer

```perl
FUNCTION as_long_integer(str_in, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
INTEGER(KIND=8) :: as_long_integer
```

`as_long_integer` is the routine which returns long integers from
strings using the maths parser. If a mathematical expression resolves to a
non-integer result then this routine rounds to the NEAREST integer. There are
explicit rounding routines in the maths parser to force other behaviour.
   
This routine should be used when evaluating strings which are likely to be too
large to be stored in an INTEGER(KIND=4).

## as_real

```perl
FUNCTION as_real(str_in, err)
  
CHARACTER(LEN=*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
REAL(num) :: as_real
```

`as_real` is the routine which returns reals from strings using the
maths parser.
   
  
This routine should be used when evaluating most strings into reals. Note that
this routine evaluates spatially dependent quantities at (0,0) on each
processor, so will give unpredictable results when spatially dependent
quantities are given to it (like density, bx etc.). To evaluate a spatially
varying quantity use `evaluate_string_in_space`.

## evaluate_string_in_space

```perl
SUBROUTINE evaluate_string_in_space(str_in, data_out, &
    x1, x2, \{y1, y2, z1, z2,\} err)
  
CHARACTER(*), INTENT(IN) :: str_in
INTEGER, INTENT(INOUT) :: err
INTEGER, INTENT(IN) :: x1, x2\{, y1, y2, z1, z2\}
REAL(num), DIMENSION(1:,1:,1:), INTENT(OUT) :: data_out
```

`evaluate_string_in_space` is a routine which is used to evaluate
a tokenized maths expression over a region of the domain. The dimensionality of
`data_out`, and the presence or absence of `y1, y2` and
`z1, z2` depend on the dimensionality of the code being used. The
`x1, x2, ...` parameters represent the indices in that
direction over which the expression should be evaluated. For example, in 2D to
evaluate an expression over the entire domain, the code would look like:
 ```perl
REAL(num), DIMENSION(:,:), ALLOCATABLE :: data
ALLOCATE(data(-2:nx+3,-2:ny+3))
CALL evaluate_string_in_space(string, data, -2, nx+3, -2, ny+3, err)
 ```
  
This routine is suitable to evaluate expressions over a subsection or all of
the domain, and is used in this way in the initial condition deck parser
routines. However, the routine does have one significant weakness, which is
that it tokenizes the string each time it is called. Tokenizing the string is a
time consuming process, so if the string is to be evaluated several times for
different reasons (for example, the time profile for the laser) then a
different procedure should be followed using the lower level parser routines.
