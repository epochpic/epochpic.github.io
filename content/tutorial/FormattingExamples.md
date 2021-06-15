+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "FormattingExamples"
[menu.tutorial]
  parent = "Content"
  weight = 200
+++

Set the page title

    {{DISPLAYTITLE: EPOCH Page Formatting}}

Note: Convert from .tex using pandoc -t mediawiki

# Versioning restrictions {#versioning_restrictions}

There are a few versioning templates to easily flag things when they
appear or disappear

    {{VersionBox| 0.0}}

A box to head up a page etc

    {{VersionedBullet|4.11| Bullet text}} - Rest of text here

\- Rest of text here

Include an '=' sign inside a template: explicitly number the template
arguments

    {{VersionedBullet|4.11| 2= Part of text = other text}} - Rest of text here

\- Rest of text here

    {{DeprecatedBox|4.12|
    * Bullet text - Some long description of the bullet goes here and can run to multiple lines as always, and can continue for as long as I want to type eh}}

    {{VersionFillBox|4.12|
    * Bullet text - Some long description of the bullet goes here and can run to multiple lines as always, and can continue for as long as I want to type eh}}

### Subsection here {#subsection_here}

    {{VersionSectionBox|4.12}}

# Syntax highlighted code {#syntax_highlighted_code}

Deck blocks can be coloured

    <syntaxhighlight lang="deck">
    begin:control
       # Global number of gridpoints
       nx = 512 # in x
       ny = 512 # in y
       # Global number of particles
       npart = 10 * nx * ny
    end:control
    </syntaxhighlight>

```perl
begin:control
   # Global number of gridpoints
   nx = 512 # in x
   ny = 512 # in y
   # Global number of particles
   npart = 10 * nx * ny
end:control
```

So can Fortran Code snippets although fancier stuff doesn't quite work

    <syntaxhighlight lang="fortran">
      TYPE particle
        REAL(num), DIMENSION(3) :: part_p
        REAL(num) :: part_pos
    #ifdef PER_PARTICLE_CHARGE_MASS
        REAL(num) :: charge
        REAL(num) :: mass
    #endif
        TYPE(particle), POINTER :: next, prev
    #ifdef PARTICLE_DEBUG
        INTEGER :: processor
        INTEGER :: processor_at_t0
    #endif
      END TYPE particle
    </syntaxhighlight>

```fortran
  TYPE particle
    REAL(num), DIMENSION(3) :: part_p
    REAL(num) :: part_pos
#ifdef PER_PARTICLE_CHARGE_MASS
    REAL(num) :: charge
    REAL(num) :: mass
#endif
    TYPE(particle), POINTER :: next, prev
#ifdef PARTICLE_DEBUG
    INTEGER :: processor
    INTEGER :: processor_at_t0
#endif
  END TYPE particle
```

# References

Inline references are created as

     <ref name="Bloggs"> J. Bloggs, "The Chicken Paper", The Journal of Chickens, page 10, 2017[http://link]</ref>

[^1]

and get dumped at the bottom of the page. It's better to include them
explicitly using

    ===References===
    <references />

### References {#references_1}

<references />

# Figures

Figures are best included as thumbnails such as

    [[File:Two Stream Late.png|right|200px|thumb|Caption]]

![Caption](/img/Two_Stream_Late.png)

[^1]: J. Bloggs, "The Chicken Paper", The Journal of Chickens, page
    10, 2017[1](http://link)
