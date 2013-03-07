GL:

 * more GL object primitives: ~~line~~, ~~sphere~~, pyramid
 * Collect ideas/implement Flowskell.Lib.Glitch (distortion effects, etc)
 * ~~Textures~~
 * ~~Shaders! (local and global (glow, blur)) http://wiki.delphigl.com/index.php/Shadersammlung~~
 * ~~Read http://www.arcadianvisions.com/blog/?p=224~~
 * ~~make-grid command or so to show the axis (RGB=XYZ) (can be called at top level or after a specific transformation)~~
 * Look into performance issues (allocating texture, depth buffer, render-to-texture)
 * ~~Fonts? using simple glut fonts for now~~
 * Option to enforce viewport width/height
 * Ability to change post shader (currently blur) or use multiple post shaders (yay!)

Scheme:

 * ~~unify (vector 0 0 0) stuff, then implement HSV coloring~~
 * ~~implement vector functions like vmul, vadd, ...~~
 * ~~fix float error~~
 * scheme compilation to Haskell AST instead of evaluation
 * use fluxus-like (every-frame ...) instead of current (define (every-frame) ...)
 * memoize stuff like secs, msecs so that 1) they stay the same for each frame, 2) evaluation is faster due to lazyness

Viewer:

 * ~~be able to rotate top level with mouse~~ and cursor keys
 * optional code, ~~fps display~~
 * REPL
     * Bugfix: Do not write to REPL when it is hidden
     * Bugfix: Fix encoding in REPL
     * Funky cursor in REPL instead of pipe
 * Code reloading
     * ~~live code reloading, and fix F5 code reloading first~~
     * ~~Bugfix: Code reloading: do not crash when source does not exist anymore~~
     * Remember path to all shader sources, and also do a reload of these
     * ~~+ more robust error handling (do not exit on Scheme/GLSL error, or better yet: rewind to old environment)~~
 * Keys
     * ~~F1: show help~~
     * ~~F2: toggle REPL~~
     * ~~F3: toggle fps~~
     * F4: toggle source
     * ~~F5: source reload~~
     * ~~F6: reset view matrix~~
     * ~~F7: screenshot~~
     * F11: fullscreen

Input:
 * Audio
     * ~~implement Jack capture~~
     * make Jack audio input stable
     * fourier analysis/harmonic analysis
 * MIDI
 * Keyboard

Testing:
 * Write tests for Scheme functions
 * Write functionality that verifies that no example contains errors

