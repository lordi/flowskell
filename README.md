Flowskell - Fluxus clone in Haskell
===================================

Flowskell allows you to write visual animations using the Scheme language. It is inspired by the [fluxus project](http://www.pawfal.org/fluxus/) and driven by OpenGL and the [husk-scheme](https://github.com/justinethier/husk-scheme) interpreter.

Run Flowskell
-------------

    cabal configure
    cabal build
    cabal install
    export PATH=$PATH:~/.cabal/bin
    flowskell examples/donuts.scm

Documentation
-------------

A rudimentary [command reference](https://github.com/lordi/flowskell/blob/master/doc/commands.md) exists, as well as some [examples](https://github.com/lordi/flowskell/tree/master/examples).
