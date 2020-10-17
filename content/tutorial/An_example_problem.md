+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "An example problem"
[menu.tutorial]
  parent = "Content"
  weight = 560
+++

Brief description of problem

# Introduction

Describe the problem you are addressing here. What physical situations
does it model? What are the limitations?

# Running notes {#running_notes}

-   **Number of processors**: n
-   **Wall time**: n minutes
-   **Preprocessor directives**: None
-   **Physics packages**: Collisions
-   **Code version**: 4.8.0

# Input deck {#input_deck}

    my_sample.deck
    begin:constants
    ...

# Output

![My caption](/tutorial/HRBeamRelaxation.png)

-   End time : 10ms
-   Plot : IDL contour of x_px_electron
-   Plot command : contour, data.x_px_electron,
    data.grid_x_px_electron.x, data.grid_x_px_electron.y,
    nlevels=40, /fill,/xsty,/ysty

# Summary

# Further reading {#further_reading}
