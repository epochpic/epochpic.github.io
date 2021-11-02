---
draft: false
toc: true
type: docs

title: fields block
linktitle: fields block
weight: 130
menu:
  documentation:
    parent: Input deck
    weight: 60
---

This block contains information about the EM fields specified at the
start of the simulation. See [EPOCH input
deck][Input_deck] for more information on the input
deck.

This block allows you to specify the electric and magnetic fields at any
point in the domain. An example block is shown below:

```perl
begin:fields
   ex = sin(pi * x / length_x)
   ey = cos(pi * x / length_x)
   ez = 0
   bx = 1.0
   by = -1.0
   bz = 0
end:fields
```

Once again, this is a very simple block needing only limited
explanation. All field variables are accessible by name and can be read
back using the appropriate commands from the maths parser (see
[here][Maths_parser__constants]). The possible
parameters are as follows:
- `ex,ey,ez` - The electric field vectors pointing in all
three directions. The default value is zero.
- `bx,by,bz` - The magnetic field vectors pointing in all
three directions. The default value is zero.
- `offset` - File offset. The field values may also be
specified using a binary file in a similar way to that used for species
variables. See [the species block][Input_deck_species]
for more details.
Any valid maths parser expression can be used to set up the fields, and
no check is made to ensure that the $\nabla.B = 0$ is satisfied.



<!-- ########################  Cross references  ######################## -->


[Input_deck]: /documentation/input_deck/input_deck
[Input_deck_species]: /documentation/input_deck/input_deck_species
[Maths_parser__constants]: /documentation/code_details/maths_parser#constants
