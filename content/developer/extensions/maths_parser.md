---
title: Maths Parser

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Extending EPOCH
    weight: 10
---

When an extension is intended for a new release, the temporary custom extensions
are not appropriate. In these cases, it is possible to permanently add functions
and constants to
EPOCH's maths parser. Although adding new operators is possible, it is
sufficiently likely to cause problems with the operation of the maths parser
that it is formally not recommended by the author of the program, and hence is
not documented here.

## Adding the new tokenizer handle
When adding a new function or constant to the maths parser using the temporary
routines, there are two calls (`register_function` and
`register_constant`) which give a numerical handle. This is the
token used to represent that function or constant after the text has been parsed
(remember that EPOCH's maths parser tokenizes before evaluation!). When
permanently adding objects to the maths parser, the tokenizer handles have to
be set up manually. This takes place in `src/shared_data.F90` in
the module `shared_parser_data`. There are several lines which
look like:
 ```perl
  INTEGER, PARAMETER :: c_const_ix = 40
  INTEGER, PARAMETER :: c_const_iy = 41
  .
  .
  .
  INTEGER, PARAMETER :: c_func_interpolate = 22
  INTEGER, PARAMETER :: c_func_tanh = 23
 ```
Constants beginning with `c_const_` are tokenizer handles for
constants, and those beginning with `c_func_` are tokenizer handles
for functions. Each number must be unique and has to be less than
the lower bound of values reserved for temporary or deck
specified values. This means that any tokenizer handle for a function has to be
less than the value of the variable `c_func_custom_lowbound` and
any handle for a constant must be less than
`c_const_deck_lowbound`. It is acceptable to simply increase the
value of `c_func_custom_lowbound` and
`c_const_deck_lowbound` to
allow the use of more values for internal constants and functions, although
care should be taken since this could lead to performance issues.
If `c_const_deck_lowbound` is increased then the constant
`c_constant_custom_lowbound` should be increased by the same
amount (the values between `c_const_deck_lowbound` and
`c_constant_custom_lowbound-1` are used for constants specified
inside the input deck while values greater than or equal to
`c_constant_custom_lowbound` are used for constants specified
by `register_constant`.

Once the tokenizer handle is specified in `shared_parser_data`, it
is now possible to extend the main areas of the maths parser. Note that from
here on, you MUST always use the constant named handle, NEVER the numerical
value that you specified for the value of the handle. If this is not done
then combining functions and constants from several sources becomes much harder.

## Adding the new function or constant to the tokenizer
The next stage is to add the string representation of your constant or function
to the tokenizer routines in
`src/parser/tokenizer_blocks.f90`. This is very simple to do, just
find either the function `as_constant` or `as_function`
and look at the existing code. These functions are just long lists of
`str_cmp` commands followed by code to deal with custom
functions/constants. To add the new code, create an additional line such as:
 ```perl
  IF (str_cmp(name, "my_const")) as_constant = c_const_my_const
  .
  .
  .
  IF (str_cmp(name, "my_func"))  as_function = c_func_my_func
 ```
Note that neither routine returns immediately after recognising the name of the
function/constant. This allows users to override built in constants or
functions with custom versions using `register_constant}`
and `register_function`. This is not significant, since tokenizing
should never be used in a speed critical part of the code.

## Implementing the function or constant in the evaluator
The evaluator is the part of the code that actually takes the streams of tokens
produced by the tokenizer and evaluates them into a number. The relevant parts
of the evaluator for adding new constants or functions are in
`src/parser/evaluator_blocks.f90` and the functions which may need
changing are `do_constant` and `do_functions`. These are
both passed up to five parameters:
 
-  `INTEGER :: opcode` - The operation code, this is the tokenizer handle
  which was defined in `shared_parser_data`.
-  `INTEGER :: ix, iy, iz` - The position of the current evaluation in the
  domain. If your function or constant behaves differently at different points
  in space then you should use these parameters to reference the correct point
  of an array.
-  `INTEGER :: errcode` - This should be set to an error code, usually 
  `c_err_bad_value` if for some reason it is not possible to evaluate your
   constant or function.
  

The rest of the routine to set a constant is as simple as testing for the
tokenizer handle already set up in `shared_parser_data` and then
calling the subroutine `push_on_eval`. This pushes the final
constant onto the evaluation stack which is used by the RPN parser. The basic
sequence for functions is similar except for the addition of a code to read
the values that the function takes. This is again the subroutine
`get_values` which is also used in custom_function. The calling
sequence in `do_function` looks like:
 ```perl
  IF (opcode .EQ. c_func_gauss) THEN
    CALL get_values(3, values)
    CALL push_on_eval(EXP(-((values(1)-values(2))/values(3))**2))
    RETURN
  ENDIF
 ```
Simply call the `get_values` subroutine, passing the number of
required parameters and an array of type `REAL(num)` which is at
least as long as the number of required parameters. The array is populated
by the values passed into the function. Constants and maths expressions are
already evaluated by the time that this section of code is reached, so there is
no need to deal with further parsing. Next, simply call
`push_on_eval` to push the result of your function onto the
evaluation stack.