# purescript-finally-tagless-ex

Direct port of examples the finally tagless, partially evaluated pattern to Purescript.

There are two examples here:

* barebones example (SimpleEx directory) comes from Oleg Kiselyov's course notes example, [here](http://okmij.org/ftp/tagless-final/)
* side-effecting example (MonadicEx directory) comes from the example in Phil Freeman's talk, [here](https://www.youtube.com/watch?v=8DdyWgRYEeI)

Each example is a standalone Pulp project, if you do a `bower init; pulp build` in each directory you should be able to verify that the code compiles (you can do a `pulp run` too, if you like but the examples don't actually _do_ anything)

Additional information:

* Phil Freeman's talk [slides](https://github.com/paf31/haskell-slides/tree/master/hoas)
* Oleg Kiselyov's [page](http://okmij.org/ftp/tagless-final/) on Typed Final (Finally Tagless) Style
