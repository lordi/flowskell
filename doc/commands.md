# Flowskell command reference

## Primitives

`draw-cube`

Draw the unit cube with the current color.

`draw-sphere`

Draw a sphere.

`draw-torus`

Draw a torus (donut).

`draw-plane`

Draw a square in the xy plane.

`draw-teapot`

Draw the beloved OpenGL teapot.

`draw-grid`

Draw a grid through the xy plane and the three coordinate axis in a distinct color (Red=x axis, Green=y axis, Blue=z axis). This is helpful for debugging as it can easily show the current transformation.

## Transformations

`translate v`

Translate current coordinate system by vector `v`.

`scale f`

`scale fx fy fz`

Scale current coordinate system.

`rotate deg v`

Rotate current coordinate system by `deg` degrees around the axis described by vector `v`.

## Math

`vmul f v`

Multiply every element in vector `v` by `f`.

`vadd f v`
`vadd v1 v2`

Add `f` to every element in vector `v`, or add two vectors `v1` and `v2`.

`vnorm v`

Return the normalized version of the vector `v`.

`vsum v`

Return the sum of all elements in the vector `v`.

`vlen v`

Return the spatial length (euclidian norm) of the vector `v`.

## Random

`rnd n`

Random number in the range 0 to `n`.

`rndf`

Random number in the range 0 to 1.

`crnf`

Random number in the range -1 to 1.

`vrnd`

Return a random, three-dimensional vector of float numbers in the range -1 to 1.

`vrndn`

Return a random, three-dimensional vector with the length of 1.

