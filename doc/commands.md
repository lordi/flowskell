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

`draw-line vlist`

Draws lines through the points given in the list `vlist`.


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

## Textures

In order to use the following commands, Flowskell needs to be configured with `-f Textures` (it is set by default).

`load-texture file-name`

Load a texture from the given `file-name`. Currently, the only supported format is PNG (RGB, RGBA). `load-texture` will return an identifier which can be used as the parameter `texture` command.

`texture texture-id`

Sets the active texture. `texture-id` can either be a returned value from a `load-texture` call, or `0` if you want to set no texture. Currently, you can also use `1` to set the rendered image of the last frame as the active texture. Hi fractals!

## Filters / Shaders

In order to use the following commands, Flowskell needs to be configured with `-f Shaders` (it is set by default).

`blur f`

Set the blur factor to `f`, a float in the range 0 to 1.

