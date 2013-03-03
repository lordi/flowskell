GL:

 * more GL object primitives: ~~line~~, ~~sphere~~, pyramid
 * Collect ideas/implement Flowskell.Lib.Glitch (distortion effects, etc)
 * ~~Textures~~
 * ~~Shaders! (local and global (glow, blur)) http://wiki.delphigl.com/index.php/Shadersammlung~~
 * ~~Read http://www.arcadianvisions.com/blog/?p=224~~
 * ~~make-grid command or so to show the axis (RGB=XYZ) (can be called at top level or after a specific transformation)~~
 * Fix performance issues (allocating texture, depth buffer, render-to-texture)
 * ~~Fonts? using simple glut fonts for now~~

Scheme:

 * midi input
 * ~~unify (vector 0 0 0) stuff, then implement HSV coloring~~
 * ~~implement vector functions like vmul, vadd, ...~~
 * ~~fix float error~~
 * scheme compilation to AST instead of evaluation
 * memoize stuff like secs, msecs so that 1) they stay the same for each frame, 2) evaluation is faster due to lazyness

Viewer:

 * ~~be able to rotate top level with mouse~~ and cursor keys
 * optional code, ~~fps display~~
 * ~~live code reloading, and fix F5 code reloading first~~
 * Bugfix: Code reloading: do not crash when source does not exist anymore
 * Bugfix: Do not write to REPL when it is hidden
 * Keys
     * ~~F1: show help~~
     * ~~F2: toggle REPL~~
     * ~~F3: toggle fps~~
     * F4: toggle source
     * ~~F5: source reload~~
     * ~~F6: reset view matrix~~
     * ~~F7: screenshot~~
     * F11: fullscreen

Audio input:
 * ~~implement Jack capture~~
 * make Jack audio input stable
 * fourier analysis/harmonic analysis

Testing:
 * Write tests for Scheme functions
 * Write functionality that verifies that no example contains errors

