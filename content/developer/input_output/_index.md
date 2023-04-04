---
title: Input Output

draft: false
toc: true
type: docs

menu:
  developer:
    name: Input Output
    weight: 40
---

EPOCH initialises the simulation by reading the input.deck text file, and writes 
an output to SDF files. These subpages provide documentation on the
string-handling in EPOCH, and give a description on how output scripts work.
EPOCH also provides an interface to allow users to easily add new blocks for the
input deck, or new functions for the maths parser, which will also be discussed 
here. These user-interface methods are appropriate for quick jobs, but any 
extensions intended for a general release should be properly added to the code.

Specifically, the pages contain:
- [Basic output](/developer/input_output/basic_output.html) - Provides a brief 
introduction to how the output routines work.
- [String-handling](/developer/input_output/string_handling.html) - Detailed
documentation on the functions and subroutines which handle string reading.
- [Error-codes](/developer/input_output/error_codes.html) - Codes used in the 
EPOCH source-code to identify errors.
- [Adding outputs](/developer/input_output/adding_outputs.html) - How to output 
a new derived variable and particle variable. Note that this does not explain 
how to _create_ a new particle variable, which is a more involved process.
- [Custom maths parser](/developer/input_output/custom_maths_parser.html) - How 
to use the user-interface to quickly create a new function or constant for the 
maths parser.
- [Custom deck blocks](/developer/input_output/custom_deck.html) - How to use
another user-interface to quickly add new blocks to the input deck.
