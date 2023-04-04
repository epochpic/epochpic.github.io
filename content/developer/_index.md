---
title: EPOCH Developer Documentation

date: 2023-Mar-10

draft: false
toc: true
type: docs

menu:
  developer:
    name: Overview
    weight: 2

---

This section of the manual is aimed at people who intend to edit the EPOCH
source code to extend or modify existing features, add new diagnostics or
develop new physics packages.

EPOCH updates the electric and magnetic fields of the system by solving 
Maxwell's equations, and these fields are used to update the positions of 
macro-particles using the Lorentz force law. The motion of charges gives the 
current densities in the system, which in turn are used to update the fields. 
Quantities are evaluated at different time-steps, and the order of opertations 
is given in this figure:

![core_code](/developer/coreblock.png)

