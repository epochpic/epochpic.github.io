+++
title = ""
draft = false  # Is this a draft? true/false
toc = true  # Show table of contents? true/false
type = "docs"  # Do not modify.

# Add menu entry to sidebar.
linktitle = "Calculable particle properties"
[menu.tutorial]
  parent = "Content"
  weight = 130
+++

As of version 5, EPOCH introduces the concept of calculable particle
properties. These are generically implemented particle properties that
can be calculated for all particles in EPOCH and are used by name in
both particle output and specifying advanced distribution functions. The
currently available names are

-   dir_x - X position of particle
-   dir_y - Y position of particle (EPOCH2D and EPOCH3D)
-   dir_z - Z position of particle (EPOCH3D)
-   dir_px - X momentum of particle
-   dir_py - Y momentum of particle
-   dir_pz - Z momentum of particle
-   dir_energy - Kinetic energy of particle (total energy for photons)
-   dir_gamma_m_1 - Gamma - 1 for particle. Calculate as 1 for
    photons
-   dir_xy_angle - Angle of the particle's momentum in the X/Y plane
-   dir_yz_angle - Angle of the particle's momentum in the Y/Z plane
-   dir_zx_angle - Angle of the particle's momentum in the Z/X plane
-   dir_modp - Particle's total momentum
-   dir_mass - Particle's mass
-   dir_ekflux_xp - Particle's kinetic energy flux in the positive x
    direction
-   dir_ekflux_xm - Particle's kinetic energy flux in the negative x
    direction
-   dir_ekflux_yp - Particle's kinetic energy flux in the positive y
    direction
-   dir_ekflux_ym - Particle's kinetic energy flux in the negative y
    direction
-   dir_ekflux_zp - Particle's kinetic energy flux in the positive z
    direction
-   dir_ekflux_zm - Particle's kinetic energy flux in the negative z
    direction
-   dir_q - Particle charge
-   dir_n - Particle self weight (number density)
-   dir_jx - Single particle current in x direction
-   dir_jy - Single particle current in y direction
-   dir_jz - Single particle current in z direction
-   dir_vx - Single particle velocity in x direction
-   dir_vy - Single particle velocity in y direction
-   dir_vz - Single particle velocity in z direction
-   dir_modv - Single particle modulus velocity
-   dir_id - Particle ID
-   weight - Particle weight
-   time - Particle local time
-   dir_x_resample - Particle X position uncoupled from primary grid.
    Allows you to produce resampled spatial output at the cost of
    greater memory requirements on each node
-   dir_y_resample - Particle Y position uncoupled from primary grid.
    Allows you to produce resampled spatial output at the cost of
    greater memory requirements on each node
-   dir_z_resample - Particle Z position uncoupled from primary grid.
    Allows you to produce resampled spatial output at the cost of
    greater memory requirements on each node
