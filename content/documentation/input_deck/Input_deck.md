---
draft: false
toc: true
type: docs

title: The EPOCH input deck
linktitle: Input deck
weight: 80
menu:
  documentation:
    name: Input deck
    weight: 20
---

Most of the control of EPOCH is through a text file called `input.deck`.
The input deck file must be in the output directory which is passed to
the code at runtime and contains all the basic information which is
needed to set up the code, including the size and subdivision of the
domain, the boundary conditions, the species of particles to simulate
and the output settings for the code. For most users this will be
capable of specifying all the initial conditions and output options they
need. More complicated initial conditions will be handled in later
sections.

The input deck is a structured file which is split into separate blocks,
with each block containing several "parameter" = "value" pairs. The
pairs can be present in any order, and not all possible pairs must be
present in any given input deck. If a required pair is missing the code
will exit with an error message. The blocks themselves can also appear
in any order. The input deck is case sensitive, so true is always "T",
false is always "F" and the names of the parameters are always lower
case. Parameter values are evaluated using a maths parser which is
described in [EPOCH maths parser][Maths_parser].
If the deck contains a "`\`" character then the rest of the line is
ignored and the next line becomes a continuation of the current one.
Also, the comment character is "`#`"; if the "`#`" character is used
anywhere on a line then the remainder of that line is ignored.
There are three *`input deck directive`* commands, which are:

-   begin:*`block`* - Begin the block named
    *`block`*.
-   end:*`block`* - Ends the block named
    *`block`*.
-   import:*`filename`* - Includes another file (called
    *`filename`*) into the input deck at the point where the
    directive is encountered. The input deck parser reads the included
    file exactly as if the contents of the included file were pasted
    directly at the position of the import directive.

Each block must be surrounded by valid *`begin:`* and
*`end:`* directives or the input deck will fail. There are
currently fourteen valid blocks hard coded into the input deck reader,
but it is possible for end users to extend the input deck. The fourteen
built in blocks are:

-   control - Contains information about the general code setup. See
    [here][Input_deck_control]
-   boundaries - Contains information about the boundary conditions for
    this run. See [here][Input_deck_boundaries]
-   species - Contains information about the species of particles which
    are used in the code. Also details of how these are initialised. See
    [here][Input_deck_species]
-   laser - Contains information about laser boundary sources. See
    [here][Input_deck_laser].
-   fields - Contains information about the EM fields specified at the
    start of the simulation. See
    [here][Input_deck_fields].
-   particles_from_file - Contains information about files used to
    load particle data. See
    [here][Input_deck_particle_file].
-   window - Contains information about the moving window if the code is
    used in that fashion. See
    [here][Input_deck_window].
-   output - Contains information about when and how to dump output
    files. See [here][Input_deck_output_block].
-   output_global - Contains parameters which should be applied to all
    output blocks. See
    [here][Input_deck_output_global].
-   dist_fn - Contains information about distribution functions that
    should be calculated for output. See
    [here][Input_deck_dist_fn].
-   probe - Contains information about particle probes used for output.
    See [here][Input_deck_probe].
-   collisions - Contains information about particle collisions. See
    [here][Input_deck_collisions].
-   qed - Contains information about QED pair production. See
    [here][Input_deck_qed].
-   subset - Contains configuration for filters which can be used to
    modify the data to be output. See
    [here][Input_deck_subset].
-   constant - Contains information about user defined constants and
    expressions. These are designed to simplify the initial condition
    setup. See [here][Input_deck_constant].



<!-- ########################  Cross references  ######################## -->


[Input_deck_boundaries]: /documentation/input_deck/input_deck_boundaries
[Input_deck_collisions]: /documentation/input_deck/input_deck_collisions
[Input_deck_constant]: /documentation/input_deck/input_deck_constant
[Input_deck_control]: /documentation/input_deck/input_deck_control
[Input_deck_dist_fn]: /documentation/input_deck/input_deck_dist_fn
[Input_deck_fields]: /documentation/input_deck/input_deck_fields
[Input_deck_laser]: /documentation/input_deck/input_deck_laser
[Input_deck_output_block]: /documentation/input_deck/input_deck_output_block
[Input_deck_output_global]: /documentation/input_deck/input_deck_output_global
[Input_deck_particle_file]: /documentation/input_deck/input_deck_particle_file
[Input_deck_probe]: /documentation/input_deck/input_deck_probe
[Input_deck_qed]: /documentation/input_deck/input_deck_qed
[Input_deck_species]: /documentation/input_deck/input_deck_species
[Input_deck_subset]: /documentation/input_deck/input_deck_subset
[Input_deck_window]: /documentation/input_deck/input_deck_window
[Maths_parser]: /documentation/code_details/maths_parser
