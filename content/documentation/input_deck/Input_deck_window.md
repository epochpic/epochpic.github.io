---
draft: false
toc: true
type: docs

title: window block
linktitle: window block
weight: 140
menu:
  documentation:
    parent: Input deck
    weight: 70
---

This block contains information about the moving window if the code is
used in that fashion. See [EPOCH input
deck][Input_deck] for more information on the input
deck.

EPOCH can include an optional block which causes the simulation domain
to operate as a moving window. At present, it is only possible to have
the window moving at a speed parallel to the x direction, although the
window does not have to start moving at t = 0. When the window moves,
the code removes particles from the left hand edge of the domain and
introduces new particles at the right hand edge. The new particles are
placed by re-evaluating the species density, temperature and drift using
the new time and spatial coordinates. The block looks like:

```perl
begin:window
   move_window = T
   window_v_x = 3.0e8
   window_start_time = 7.0e-13
   bc_x_min_after_move = simple_outflow
   bc_x_max_after_move = simple_outflow
end:window
```

-   `move_window` - Logical flag determining whether or not
    to move the window. If the window block is absent then this is the
    same as setting move_window to "F".
-   `window_v_x` - The speed in m/s of the window.
-   `window_start_time` - The time in seconds at which the
    window should start moving.
-   `window_stop_time` - The time in seconds at which the window should stop moving.
-   `bc_x_min_after_move` - The boundary condition which
should apply to the left boundary after the window has started moving.
This is to allow the swapping of a laser boundary to a simple outflow
boundary. Boundary codes are the same as when just specifying normal
boundaries. If a boundary value isn't specified then it is assumed that
the boundary isn't changed when the window starts moving.
"xbc_left_after_move" is accepted as a synonym.
- `bc_x_max_after_move` - The boundary condition which
should apply to the right boundary after the window has started moving.
"xbc_right_after_move" is accepted as a synonym.
- `bc_{y,z}_{min,max}_after_move` - "y" and "z"
versions of the previous two parameters. **ybc_down_after_move**,
**ybc_up_after_move**, **zbc_back_after_move** and
**zbc_front_after_move** are accepted as synonyms.

# Compatibility

Because of how the moving window must work, there are some compatibility
issues with certain features. In particular:

-   lasers attached to an X boundary which remain in place after the
    window moves, or attached to Y or Z boundaries:
    -   The laser will behave as though it is attached to the window
        itself: for Y or Z boundaries with spatial variations this may
        not give the expected result
    -   For X boundaries, the moving emitter will result in a form of
        numerical Doppler shifting. In addition to this the boundary
        used to drive the field will shift discontinuously, yielding
        noisy and erratic changes in the electromagnetic field.
-   Injectors attached to an X boundary will not work. Those on a Y or Z
    boundary may appear to work, but the rates will be incorrect.
-   CPML boundary conditions:
    -   in X these cannot work as they rely on time-history which is
        simply missing.
    -   On Y or Z boundaries they will approximately work, but the
        history will be truncated and so they will generally require
        more tuning. We can't help with this in general.
-   Load of particles from file is not supported since it can't be made
    to work in general.



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
