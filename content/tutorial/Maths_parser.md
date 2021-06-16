---
draft: false
toc: true
type: docs

title: Maths parser
linktitle: Maths parser
weight: 270
menu:
  tutorial:
    parent: Code details
    weight: 10
---

A discussion of the input deck for EPOCH would not be complete without
consideration of the maths parser. The maths parser is the code which
reads the input decks. The parser makes it possible that any parameter
taking a numerical value (integer or real) can be input as a
mathematical expression rather than as a numerical constant. The maths
parser is fairly extensive and includes a range of mathematical
functions, physical and simulation constants and appropriately
prioritised mathematical operators.

### Constants

The maths parser in EPOCH has the following constants

-   pi - The ratio of the circumference of a circle to its diameter.
-   kb - Boltzmann's constant.
-   me - Mass of an electron.
-   qe - Charge of an electron.
-   c - Speed of light.
-   epsilon0 - Permeability of free space.
-   mu0 - Permittivity of free space.
-   ev - Electronvolt.
-   kev - Kilo-Electronvolt.
-   mev - Mega-Electronvolt.
-   micron - A convenience symbol for specifying wavelength in microns
    rather than metres.
-   milli - $10^{-3}$
-   micro - $10^{-6}$
-   nano - $10^{-9}$
-   pico - $10^{-12}$
-   femto - $10^{-15}$
-   atto - $10^{-18}$
-   cc - A convenience symbol for converting from cubic metres to cubic
    centimetres (ie. $10^{-6}$)
-   time - Initial simulation time.
-   x,y,z - Grid coordinates in the x,y,z direction.
-   ix,iy,iz - Grid index in the x,y,z direction.
-   nx,ny,nz - Number of grid points in the x,y,z direction.
-   dx,dy,dz - Grid spacing in the x,y,z direction.
-   {x,y,z}_min - Grid coordinate of the minimum x,y,z boundary.
-   {x,y,z}_max - Grid coordinate of the maximum x,y,z boundary.
-   length_{x,y,z} - The length of the simulation box in the x,y,z
    direction.
-   nproc_{x,y,z} - The number of processes in the x,y,z directions.
-   nsteps - The number of timesteps requested.
-   t_end - The end time of the simulation.
-   p{x,y,z} - Momentum in the x, y, z directions. Used in specifying
    arbitrary distribution functions. EPOCH 4.15 or later required

It is also possible for an end user to specify custom constants both
within the code and from the input deck. These topics are covered later
in this subsection. An example of using a constant would be:
**length_x = pi**

### Functions

The maths parser in EPOCH has the following functions

-   abs(a) - Absolute value.
-   floor(a) - Convert real to integer rounding down.
-   ceil(a) - Convert real to integer rounding up.
-   nint(a) - Convert real to integer rounding to nearest integer.
-   sqrt(a) - Square root.
-   sin(a) - Sine.
-   cos(a) - Cosine.
-   tan(a) - Tangent.
-   asin(a) - Arcsine.
-   acos(a) - Arccosine.
-   atan(a) - Arctangent.

\- Arctangent using the Fortran ATAN2 function. This computes the
principal value of the argument function of the complex number
$X + i Y$. This function can be used to transform from Cartesian into
polar coordinates and allows to determine the angle in the correct
quadrant.

-   sinh(a) - Hyperbolic sine.
-   cosh(a) - Hyperbolic cosine.
-   tanh(a) - Hyperbolic tangent.
-   exp(a) - Exponential.
-   loge(a) - Natural logarithm.
-   log10(a) - Base-10 logarithm.
-   log_base(a,b) - Base-b logarithm.
-   gauss($x,x_0,w$) - Calculate a Gaussian profile in variable
    *`x`* centred on *`$x_0$`* with a
    characteristic width *`w`*.
    $f(x) = \exp{(-((x-x_0)/w)^2)}$. In this expression the full width
    at half maximum is given by $fwhm = 2 w \sqrt{\ln{2}}$
-   supergauss($x,x_0,w,n$) - This is identical to "gauss" except it
    takes a fourth parameter, *`n`*, which is the power to
    raise the exponential argument to.
-   semigauss($t,A,A_0,w$) - Calculate a semi Gaussian profile in
    variable $t$ with maximum amplitude $A$, amplitude at $t=0$ $A_0$
    and width $w$. The parameter $A_0$ is used to calculate $t_0$, the
    point at which the Gaussian reaches its maximum value. For $t$ less
    than $t_0$ the profile is Gaussian and for $t$ greater than $t_0$ it
    is the constant $A$. $t_0 = w\sqrt{-\ln{(A_0/A)}}$<math>f(t) =

\\begin{cases} A \\exp{(-((t-t_0)/w)\^2)}, & t \< t_0 \\\\ A, &
\\mbox{otherwise} \\end{cases}</math>

-   interpolate(interp_var,\....,n_pairs) - Linear interpolation
    function, explained later.
-   if(a,b,c) - Conditional function. If a != 0 the function returns b,
    otherwise the function returns c.
-   number_density(a) - Returns the number density for species a.
-   temp_{x,y,z}(a) - Returns temperature in the x, y or z direction
    for species a.
-   temp(a) - Returns the isotropic temperature for species a.
-   e{x,y,z}(x,y,z) - Returns the x, y or z component of the electric
    field at the specified location.
-   b{x,y,z}(x,y,z) - Returns the x, y or z component of the magnetic
    field at the specified location.
-   critical($\omega$) - Returns the critical density for the given
    frequency $\omega$. ie.
    $n_{crit}(\omega) = \omega^2 m_0 \epsilon_0 / e^2$

It is also possible for an end user to specify custom functions within
the code. An example of using a function would be:
**length_x = exp(pi)**
The use of most of these functions is fairly simple, but "interpolate"
requires some additional explanation. This function allows a user to
specify a set of position,value pairs and have the code linearly
interpolate the values between these control points. This function is
mainly intended for ease of converting initial conditions from other
existing PIC codes, and the same effect can usually be obtained more
elegantly using the "if" command. The structure of the "interpolate"
command is as follows: The first parameter is the variable which is to
be used as the axis over which to interpolate the values. This can in
general be any valid expression, but will normally just be a coordinate
axis. The next 2n entries are the position,value pairs and the final
parameter is the number of position,value pairs. The slightly clunky
syntax of this command is unfortunately necessary to allow it to work
with some fairly fundamental features of the maths parser used in EPOCH.

### Operators

The maths parser in EPOCH allows the following operators

-   a + b - Addition operator.
-   a - b - Subtraction operator or unary negation operator
    (auto-detected).
-   a \* b - Multiplication operator.
-   a / b - Division operator.
-   a \^ b - Power raise operator.
-   a e b - Power of ten operator (1.0e3 = 1000).
-   a lt b - Less than operator. Returns 1 if a $<$ b, otherwise
    returns 0. Intended for use with if.
-   a gt b - Greater than operator. Returns 1 if a $>$ b, otherwise
    returns 0.
-   a eq b - Equality operator. Returns 1 if a == b, otherwise
    returns 0.
-   a and b - Logical and operator. Returns 1 if a != 0 and b != 0,
    otherwise returns 0.
-   a or b - Logical or operator. Returns 1 if a != 0 or b != 0,
    otherwise returns 0.

These follow the usual rules for operator precedence, although it is
best to surround more complex expressions in parenthesis if the
precedence is important. It is not possible at this time to specify
custom operators without major changes to the code. An example of using
an operator would be:

```perl
 length_x = 10.0 + 12.0 
```


<!-- ########################  Cross references  ######################## -->


[Acknowledging_EPOCH]: /tutorial/acknowledging_epoch
[Basic_examples]: /tutorial/basic_examples
[Basic_examples__focussing_a_gaussian_beam]: /tutorial/basic_examples/#focussing_a_gaussian_beam
[Binary_files]: /tutorial/binary_files
[Calculable_particle_properties]: /tutorial/calculable_particle_properties
[Compiler_Flags]: /tutorial/compiler_flags
[Compiling]: /tutorial/compiling
[FAQ]: /tutorial/faq
[FAQ__how_do_i_obtain_the_code]: /tutorial/faq/#how_do_i_obtain_the_code
[Input_deck]: /tutorial/input_deck
[Input_deck_adf]: /tutorial/input_deck_adf
[Input_deck_boundaries]: /tutorial/input_deck_boundaries
[Input_deck_boundaries__cpml_boundary_conditions]: /tutorial/input_deck_boundaries/#cpml_boundary_conditions
[Input_deck_boundaries__thermal_boundary_conditions]: /tutorial/input_deck_boundaries/#thermal_boundary_conditions
[Input_deck_collisions]: /tutorial/input_deck_collisions
[Input_deck_constant]: /tutorial/input_deck_constant
[Input_deck_control]: /tutorial/input_deck_control
[Input_deck_control__basics]: /tutorial/input_deck_control/#basics
[Input_deck_control__maxwell_solvers]: /tutorial/input_deck_control/#maxwell_solvers
[Input_deck_control__requesting_output_dumps_at_run_time]: /tutorial/input_deck_control/#requesting_output_dumps_at_run_time
[Input_deck_control__stencil_block]: /tutorial/input_deck_control/#stencil_block
[Input_deck_control__strided_current_filtering]: /tutorial/input_deck_control/#strided_current_filtering
[Input_deck_dist_fn]: /tutorial/input_deck_dist_fn
[Input_deck_fields]: /tutorial/input_deck_fields
[Input_deck_injector]: /tutorial/input_deck_injector
[Input_deck_injector__keys]: /tutorial/input_deck_injector/#keys
[Input_deck_laser]: /tutorial/input_deck_laser
[Input_deck_operator]: /tutorial/input_deck_operator
[Input_deck_output__directives]: /tutorial/input_deck_output/#directives
[Input_deck_output_block]: /tutorial/input_deck_output_block
[Input_deck_output_block__derived_variables]: /tutorial/input_deck_output_block/#derived_variables
[Input_deck_output_block__directives]: /tutorial/input_deck_output_block/#directives
[Input_deck_output_block__dumpmask]: /tutorial/input_deck_output_block/#dumpmask
[Input_deck_output_block__multiple_output_blocks]: /tutorial/input_deck_output_block/#multiple_output_blocks
[Input_deck_output_block__particle_variables]: /tutorial/input_deck_output_block/#particle_variables
[Input_deck_output_block__single-precision_output]: /tutorial/input_deck_output_block/#single-precision_output
[Input_deck_output_global]: /tutorial/input_deck_output_global
[Input_deck_particle_file]: /tutorial/input_deck_particle_file
[Input_deck_probe]: /tutorial/input_deck_probe
[Input_deck_qed]: /tutorial/input_deck_qed
[Input_deck_species]: /tutorial/input_deck_species
[Input_deck_species__arbitrary_distribution_functions]: /tutorial/input_deck_species/#arbitrary_distribution_functions
[Input_deck_species__ionisation]: /tutorial/input_deck_species/#ionisation
[Input_deck_species__maxwell_juttner_distributions]: /tutorial/input_deck_species/#maxwell_juttner_distributions
[Input_deck_species__particle_migration_between_species]: /tutorial/input_deck_species/#particle_migration_between_species
[Input_deck_species__species_boundary_conditions]: /tutorial/input_deck_species/#species_boundary_conditions
[Input_deck_subset]: /tutorial/input_deck_subset
[Input_deck_window]: /tutorial/input_deck_window
[Landing]: /tutorial/landing
[Landing_Page]: /tutorial/landing_page
[Libraries]: /tutorial/libraries
[Links]: /tutorial/links
[Maths_parser__functions]: /tutorial/maths_parser/#functions
[Non-thermal_initial_conditions]: /tutorial/non-thermal_initial_conditions
[Previous_versions]: /tutorial/previous_versions
[Python]: /tutorial/python
[Running]: /tutorial/running
[SDF_Landing_Page]: /tutorial/sdf_landing_page
[Structure]: /tutorial/structure
[Using_EPOCH_in_practice]: /tutorial/using_epoch_in_practice
[Using_EPOCH_in_practice__manually_overriding_particle_parameters_set_by_the_autoloader]: /tutorial/using_epoch_in_practice/#manually_overriding_particle_parameters_set_by_the_autoloader
[Using_EPOCH_in_practice__parameterising_input_decks]: /tutorial/using_epoch_in_practice/#parameterising_input_decks
[Using_delta_f]: /tutorial/using_delta_f
[Visualising_SDF_files_with_IDL_or_GDL]: /tutorial/visualising_sdf_files_with_idl_or_gdl
[Visualising_SDF_files_with_LLNL_VisIt]: /tutorial/visualising_sdf_files_with_llnl_visit
[Workshop_examples]: /tutorial/workshop_examples
[Workshop_examples__a_2d_laser]: /tutorial/workshop_examples/#a_2d_laser
[Workshop_examples__a_basic_em-field_simulation]: /tutorial/workshop_examples/#a_basic_em-field_simulation
[Workshop_examples__getting_the_example_decks_for_this_workshop]: /tutorial/workshop_examples/#getting_the_example_decks_for_this_workshop
[Workshop_examples__specifying_particle_species]: /tutorial/workshop_examples/#specifying_particle_species
[Workshop_examples_continued]: /tutorial/workshop_examples_continued
