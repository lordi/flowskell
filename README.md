Flowskell - Fluxus clone in Haskell
===================================

Flowskell allows you to write visual animations using the Scheme language. It is inspired by the [fluxus project](http://www.pawfal.org/fluxus/) and driven by OpenGL and the [husk-scheme](https://github.com/justinethier/husk-scheme) interpreter.

Build Flowskell
---------------

    cabal configure
    cabal build
    cabal install

Run Flowskell
-------------

    export PATH=$PATH:~/.cabal/bin
    flowskell examples/donuts.scm

Interaction
-----------

While Flowskell is running, you have the following shortcuts available:

 * F1: Toggle help
 * F2: Toggle Scheme REPL
 * F3: Toggle FPS display
 * F4: Toggle source display (TBD)
 * F5: Reload/restart current source
 * F6: Reset view (TBD)
 * F7: Save a screenshot


Documentation
-------------

A rudimentary [command reference](https://github.com/lordi/flowskell/blob/master/doc/commands.md) exists, as well as some [examples](https://github.com/lordi/flowskell/tree/master/examples).
