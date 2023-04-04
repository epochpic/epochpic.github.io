---
title: Error Codes

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Input Output
    weight: 22
---

The input deck and maths parser in EPOCH use various named error codes to
report on errors which occur during the evaluation of the input deck. These
codes are
 
-  `c_err_none` - No error. Set an error code to c_err_none to state that
  no error has occurred.
-  `c_err_unknown_block` - In the input deck a block has been found which is
  not known. This should be returned in
  `custom_blocks_handle_element` if it is passed any block that it has not been written to handle.
-  `c_err_unknown_element` - In the input deck an element of a valid block
  has been found which is not known. This should be returned in
  `custom_blocks_handle_element` if an element is requested which is
  unknown.
-  `c_err_preset_element` - An element of the input deck has already been
  set and is being set again. Usually this is an indication of a malformed input
  deck file, so `custom_blocks_handle_element` should try to
  identify such situations and return this error message if
  subsequent attempts to set the variable are being ignored.
-  `c_err_preset_element_use_later` - An element of the input deck has
  already been set and is being set again. Usually this is an indication of a
  malformed input deck file, so `custom_blocks_handle_element` should try to
  identify such situations and return this error message if
  the subsequent attempts to set the variable override previous ones.
-  `c_err_bad_value` - A value which is being evaluated for the right
  hand side of an element assignment is in some way invalid. Internally to the
  code this usually means that a string which must be interpreted as a maths
  expression or numerical constant is in some way malformed. It is also
  acceptable to return this error code when a value has been passed which is
  invalid for some other reason (the value is outside an acceptable range, etc.)
-  `c_err_missing_elements` - This is an error code returned when the
  code is testing to make sure that all necessary elements of an input deck
  file have been specified. It should be returned when some required parameter
  is missing in the subroutine `custom_blocks_check`.
-  `c_err_terminate` - This error code means that the code is in a state
  where execution is impossible and the code must terminate once the input deck
  has been read. Some other error codes automatically set c_err_terminate,
  but it can always be IOR'ed with any error code to force the code to exit.
  Note that just returning c_err_terminate will cause the code to
  silently quit.
-  `c_err_required_element_not_set` - This means that the code
  cannot parse an input deck element since another element which must be known
  beforehand has not been set. This is intended for things like setting the
  species information where the number of species must be known in
  advance. This error code uses the extended error string to give user friendly
  feedback. If you return this error code then you should set
  extended_error_string to be equal to the name of the required element which
  has not been set. If multiple previous elements are required then the code
  should be set up so that it checks for the presence of the required elements
  in order and reports on missing elements so that the end user can fix them
  one by one.
-  `c_err_pp_options_wrong` - If you've written a section of the code that
  is controlled by preprocessor options then you should return this
  error message if someone attempts to set input deck elements which refer to
  that part when the correct preprocessor options are not used. This means that
  the user is aware of the fact that the requested feature will not be
  active. This error code also uses the extended error string to give user
  friendly feedback. If you return this error code, you should set the string
  extended_error_string to the define command that would turn on the
  requested feature of the code ("-DPER_PARTICLE_WEIGHT", for example).
-  `c_err_other` - This error code is a catch all error which causes
  the code to quit with a sarcastic error message. It's mainly intended for
  debugging and is used before the final error code is implemented.