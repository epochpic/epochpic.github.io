---
draft: false
toc: true
type: docs

title: Constant block
linktitle: Constant block
weight: 220
menu:
  documentation:
    parent: Input deck
    weight: 10
---

This block contains information about user defined constants and
expressions. These are designed to simplify the initial condition setup.
See [EPOCH input deck][Input_deck] for more information
on the input deck.

The *constant* block type helps to make the input deck more flexible and
maintainable. It allows you to define constants and maths parser
expressions (see [EPOCH maths parser][Maths_parser])
which can be used by name later in the deck.
Constants are simply maths parser expressions which are assigned to a
name as shown above. When the name is used on the right hand side of a
deck expression it is replaced by the expression it was assigned with.
This expression may be a simple numerical constant, a mathematical
expression or a function. Constants may contain spatially varying
information without having to pre-calculate them at every location in
the domain. To those familiar with FORTRAN codes which use statement
functions, parameters appearing in the "constant" block are fairly
similar.
If a constant name is reused in a constant block then the old constant
is deleted and replaced with the new one. This happens without warning.

```perl
begin:constant
   lambda = 1.06 * micron
   omega = 2.0 * pi * c / lambda
   den_crit = critical(omega)
   scale = 3.5 * micron
   den_max = 5.0 * den_crit
   thick = 300e-9
   pplength = 6000e-9
   widscale = 5.0e-6

   t_wid = (10.0e-6) / c
   amax = 1.0
   wy = 1e-6
   y = 0.0

   slope = exp(-2.0 * (y/wy)^2)
   blob = gauss(sqrt(x^2 + y^2), 0.0, 1.0e-6)
end:constant
```

Using constants can be very helpful when dealing with long, complicated
expressions since they allow the expression to be broken down into much
simpler parts. They can also be used to get around the FORTRAN string
length limitation built into many compilers which prevents deck lines
being longer then 512 characters long. As a general rule, it is a good
idea to break down complicated expressions using constants or by other
means, in order to make the deck look more readable.
Constants are persistent for the entire runtime of the code, allowing
them to be used when specifying time profiles for lasers, and also
allowing developers to use maths parser expressions for other internal
parts of the code where needed.
In the above example, several pre-defined constants have been used (*pi*
and *c*) and also several functions (*critical*, *exp*, *gauss* and
*sqrt*). These are described
[here][Maths_parser__constants] and
[here][Maths_parser__functions].



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
[Maths_parser]: /documentation/code_details/maths_parser
[Maths_parser__constants]: /documentation/code_details/maths_parser#constants
[Maths_parser__functions]: /documentation/code_details/maths_parser#functions
