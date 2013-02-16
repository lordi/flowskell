GL:

 * more GL object primitives: ~~line~~, ~~sphere~~, pyramid
 * Collect ideas/implement Flowskell.Lib.Glitch (distortion effects, etc)
 * ~~Textures~~
 * ~~Shaders! (local and global (glow, blur)) http://wiki.delphigl.com/index.php/Shadersammlung~~
 * ~~Read http://www.arcadianvisions.com/blog/?p=224~~
 * ~~make-grid command or so to show the axis (RGB=XYZ) (can be called at top level or after a specific transformation)~~

Scheme:

 * midi input
 * ~~unify (vector 0 0 0) stuff, then implement HSV coloring~~
 * ~~implement vector functions like vmul, vadd, ...~~
 * ~~fix float error~~
 * scheme compilation to AST instead of evaluation
 * memoize stuff like secs, msecs so that 1) they stay the same for each frame, 2) evaluation is faster due to lazyness

Viewer:

 * Be able to rotate top level with mouse and cursor keys
 * optional code, fps display
 * live code reloading

Audio input:
 * ~~implement Jack capture~~
 * fourier analysis/harmonic analysis

Other:
 * Write tests that verify that no example contains errors
