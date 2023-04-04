---
title: Current solver

draft: false
toc: true
type: docs

menu:
  developer:
    parent: Core Structure
    weight: 80
---

EPOCH uses the [Villasenor and Buneman](https://www.sciencedirect.com/science/article/abs/pii/001046559290169Y)  current calculating scheme which
solves the additional equation
${\partial \rho}/{\partial t} = \nabla\cdot\vec{J}$ to
calculate the current at each timestep. The main advantage of this scheme is
that it conserves charge _on the grid_ rather than just globally conserving
charge on the particles. This means that the error in the solution to Gauss's
law is conserved, so if Gauss's law is satisfied for $t = 0$ it
remains satisfied for all time. 

The Villasenor and Buneman scheme works because exactly the same charge added
to one cell is subtracted from another cell, which in turn means that exactly
the same current added to one cell is subtracted from another cell. This is
intuitively correct since a point particle crossing a cell boundary would
represent the loss of that particle's contribution to the current from the
source cell and the gain of that particle's contribution to the current by the
destination cell. In fact this simple type of cell boundary crossing
current calculation was used in classical Buneman type PIC codes. 

The scheme is messy, in practise, but simple. After the main particle push, the
particle is advanced a further half timestep into the future to first order
using the velocities calculated at the end of the particle push. The particle
position at $t + dt/2$ were stored earlier, and combined with the newly
calculated particle position at $t + {3dt}/{2}$ this allows a time centred
evaluation of ${\partial \rho}/{\partial t}$ meaning that the current
update is second order accurate in time. The spatial order of the scheme
matches the spatial order of the particle weight function. 

The weight functions for transferring particle properties onto the grid at the
two timesteps are calculated including a shift when necessary to allow for the
particle having crossed a cell boundary. Since the charge associated with the
particle is spatially distributed using the weight function, all that is
necessary to calculate ${\partial \rho}/{\partial t}$ is to subtract the
two functions, multiply by the charge on the pseudoparticle and the
pseudoparticle weight and finally divide by $dt$. The spatial derivative of
$\vec{J}$ is then converted to a one sided finite difference form and solved
directly. In multiple dimensions this is slightly complicated by the effects of
offsets in directions other than the direction that a given current component
is pointing in, with this adding additional weight factors based on the overlap
of the shape functions in other directions. This is explained in full in the
Villasenor and Buneman paper already quoted. 

Currents in ignorable directions are simply calculated using $J = n\rho\vec{v}$
with the correct shape functions to ensure that the current is placed in the
correct places.

## Example

In EPOCH2D V4.19.2, the source-code for the current update looks like this:

```
  jyh = 0.0_num
    DO iy = ymin, ymax
      cy = cell_y1 + iy
      yfac1 =         gy(iy) + 0.5_num * hy(iy)
      yfac2 = third * hy(iy) + 0.5_num * gy(iy)

      hy_iy = hy(iy)

      jxh = 0.0_num
      DO ix = xmin, xmax
        cx = cell_x1 + ix
        xfac1 = gx(ix) + 0.5_num * hx(ix)

        wx = hx(ix) * yfac1
        wy = hy_iy  * xfac1
        wz = gx(ix) * yfac1 + hx(ix) * yfac2

        ! This is the bit that actually solves d(rho)/dt = -div(J)
        jxh = jxh - fjx * wx
        jyh(ix) = jyh(ix) - fjy * wy
        jzh = fjz * wz

        jx(cx, cy) = jx(cx, cy) + jxh
        jy(cx, cy) = jy(cx, cy) + jyh(ix)
        jz(cx, cy) = jz(cx, cy) + jzh
      END DO
    END DO
```

At this point in the code, `gx` and `gy` contain the weight distribution of 
the macro-particle at time $t+dt/2$, where the 0 index in these arrays
correspond to the cell containing the macro-particle centre at this time (or 
the low-x, low-y corner for TOPHAT shapes). The `hx` and `hy` parameters contain
the difference of weights in each cell between $t+3dt/2$ and $t+dt/2$. The loop 
occurs from `gx` index `xmin` to `xmax`, and `gy` index `ymin` to `ymax`. These 
min/max indices will describe an array which has the same size as the number of 
cells which the macro-particle shape has touched over the time-step.

As an example, consider the $j_x$ update. For cell index `ix=xmin`, we first
calculate the average y-weight for each `iy` using the line:

```
      yfac1 =         gy(iy) + 0.5_num * hy(iy)
```

as `gy(iy)` is the initial y-weight, and `gy(iy) + hy(iy)` is the final
y-weight. We assume the macro-particle moves at a constant speed when taking the
average. Hence, the change in macro-particle weight due to motion in the $x$ 
direction from the `ix=xmin` cell is `hx(xmin) * (gy(iy) + 0.5*hy(iy))`. In the 
code, this is `wx`. We can multiply this by the macro-particle charge 
(charge * weight) to get the charge change due to $x$ motion, divide by `dt` to 
get a current, and divide by `dy` to get the current per unit area 
(as `dz` = 1m in EPOCH2D). These particle variables and simulation constants are 
contained in the `fjx` variable.

Finally, we must remember that this refers to the total current density change 
in the cell - we do not know the boundary this current flows through. In the 
`xmin` cell, we know no macro-particle weight enters `xmin-1` by definition of 
`xmin`, so all the current density flows into the cell with `ix=xmin+1`. Hence,
the current change is unambiguous here. If there is no current change in 
`xmin+1`, then an equal current must flow in and out. We have just calculated 
the `xmin` to `xmin+1` current, so we can subtract this from the new cell to 
determine the current on the next boundary. Because we need to remember the 
current from the previous calculation, we must subtract `jxh` from the 
previously calculated `jxh` in:

```
jxh = jxh - fjx * wx
```

This calculation may then iterate through the particle shape, until the $j_x$ 
contribution from this macro-particle is recorded in all cells it influences. 
The remaining current density components can be calculated using similar logic.
