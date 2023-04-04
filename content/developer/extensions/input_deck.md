---
title: Input Deck

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Extending EPOCH
    weight: 10
---

While using the `custom_deck` subroutine is a good way of passing
parameters into the code or for temporary additions, it is not suitable for
permanent additions to the code. Adding new blocks to the code permanently is
very similar to doing it temporarily, but requires changes to some of the
subroutines in `deck.F90`. 

There are six subroutines which may need to be changed to add new blocks to
the deck. These are
 
-  `deck_initialise` - Called before parsing of the input
  deck is begun.
-  `deck_finalise` - Called after parsing of the input
  deck is complete.
-  `start_block(block_name)` - Called when the deck directive
  `begin:block_name` appears in a deck file.
-  `end_block(block_name)` - Called when the deck directive
  `end:block_name` appears in a deck file.
-  `handle_block(block_name, block_element, block_value)` -
  Called once for each element in a block.
-  `check_compulsory_blocks(errcode_deck)` - Called once when
  the deck file has been read to check that all necessary blocks have been
  populated.

There is one final variable which is important for modifying the input deck,
`deck_state`. The input deck parser routine used to read the main
input deck uses the variable `deck_state` to
determine which stage of parsing the deck is required. The possible values of
`deck_state` are
 
-  c_ds_first - The first pass through the deck, before memory has been
  allocated.
-  c_ds_last - After the initial deck pass, all arrays and lists are
  allocated. The deck is then parsed a final time so that allocated memory
  can be populated with initial conditions.
  
These constants are defined in `shared_data.F90`.

The layout of `deck_initialise` and `deck_finalise` is
extremely simple. They just call `*_deck_initialise` or
`*_deck_finalise` for each of the possible block types.
`start_block` and `end_block` are also fairly
straightforward. They examine the block name and call the
`*_block_start` or `*_block_end` routine
corresponding to the current block.

The `handle_block` routine acts in a similar manner except
that it also does some error handling.
At the simplest level the routine simply calls another function which
takes the block_element and block_value as
parameters and returns an error code
determining the success or failure of reading the element.

The final routine is `check_compulsory_blocks` which is used to
check that all the needed elements of the input deck have been set. A single
parameter `errcode_deck` is passed in. The routine
checks `deck_state` to make sure that it is the last pass
through the deck. It then goes through and calls functions to check that
all the necessary parts of a block have been set. The subroutines are contained
in the same file as the routine which is called in `handle_block` to
handle elements of the block. The error handler functions should return an
error code, usually
`c_err_missing_elements`. The
return code from the error handler function should then be `IOR`-ed
with `errcode_deck` to allow error codes to be returned from
several different checks with errors occurring.

## The element handler routines for deck elements
The exact form of the handler routines is up to the end user. The only
_requirements_ are that the routine should return an error code detailing
whether or not there are any problems with reading the block and that the error
code should be `c_err_none` if either the element name or element
value are the special constant `blank`. The typical implementation
of an element handler routine is shown in the file
`src/deck/deck_control_block.f90`, and this general layout should
be copied for compatibility if possible. 

Sometimes, it is useful to have each new block correspond to a new instance of
an object in the code. An example of this in EPOCH is in
`src/deck/deck_laser_block.f90` where each new laser block in
the input deck corresponds to a new laser being attached to a boundary. This is
accomplished by implementing the lasers as a linked list on each boundary,
with a new laser object being created when a laser block is started, the laser
information being set during the main reader routine, and then the laser being
attached to the linked list by a call to `attach_laser` in
`src/laser.f90` when the block is ended. When a new laser block is
started the process simply repeats allowing the end user to have as many lasers
as desired.

## Adding elements to existing blocks
The existing blocks in the code are read in the files listed in \sect{src_deck}

The existing structure of the blocks is simple enough in most cases that it
should be fairly easy to add new elements if needed. The most likely change
needed is to change the list of variables to dump in the `output`
block. How to do this is detailed in \sect{io}.