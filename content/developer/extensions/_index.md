---
title: Extending EPOCH

draft: false
toc: true
type: docs

menu:
  developer:
    name: Extending EPOCH
    weight: 50
---

Exactly how to extend EPOCH depends heavily upon what you intend to add. The
simplest things to add are new diagnostics, but other places where changes are 
likely to be include:
 
-  The field solver - Changing the field solver to add new laser-like
  boundaries, add spatial smoothing to remove noise, add high order field
  solvers etc.
-  The particle pusher - Change the basic physical model of EPOCH by
  modifying the particle pusher.
-  The boundary routines - Add new boundary conditions or modify existing
  boundary conditions.
-  The laser boundary routines - Add new features to the laser boundaries in
  this routine.
-  Physics packages - EPOCH includes many existing physics packages, but the
  list is not complete. For example, the user may wish to add support for 
  nuclear or particle physics processes, or electron-ion recombination.
-  The main driver (`epochnd.F90`) - This is the routine
  where the main calling sequence of EPOCH is setup, and totally new
  extensions to EPOCH should be placed in here.

Changing the field solver, the particle pusher or boundary routines
is fairly easy to accomplish by reading the section of this manual that
details the relevant code. The general sequence for writing an extension
would be:
 
-  Add any new global variables needed to `shared_data.F90`.
-  Add the meat of your change to the code.
-  Test the changes to your code. Make absolutely sure that you can turn your
  change to the code off.
-  Add controls for your extension to the input deck reader.

## Examples

The subpages here detail a few examples on how to make direct changes to the 
EPOCH source-code, without going through user-interaction interfaces. These 
include:

- [Input deck](/developer/extensions/input_deck.html) - Instructions on creating 
a new input deck block.
- [Maths parser](/developer/extensions/maths_parser.html) - How to create a new
function for the maths parser.
- [New module](/developer/extensions/new_modules.html) - 
If you already have the 
source-code for a new module or physics package, this will tell you how to add
it to the EPOCH PIC loop.