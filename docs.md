# Flowskell command reference

## Primitives

`draw-cube`

Draws the unit cube with the current color.

`draw-sphere`

Draws a sphere.

`draw-torus`

Draws a torus (donut).

`draw-plane`

Draws a square in the xy plane.

`draw-teapot`

Draws the beloved OpenGL teapot.

`draw-grid`

Draws a grid through the xy plane and the three coordinate axis in a distinct color (Red=x axis, Green=y axis, Blue=z axis). This is helpful for debugging as it can easily show the current transformation.

## Transformations

`translate v`

Translate current coordinate system by vector `v`.

`scale f`

`scale fx fy fz`

Scales current coordinate system.

`rotate deg v`

Rotates current coordinate system by `deg` degrees around the axis described by vector `v`.

## Math

`vmul f v`

Multiply every element in vector `v` by `f`.

`vadd f v`
`vadd v1 v2`

Add `f` to every element in vector `v`, or add two vectors `v1` and `v2`.

## Random

`rnd n`

Random number in the range 0 to `n`.

`rndf`

Random number in the range 0 to 1.

`crnf`

Random number in the range -1 to 1.

`vrnd`

Return a random, three-dimensional vector of float numbers in the range -1 to 1.
