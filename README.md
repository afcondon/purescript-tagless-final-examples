# Purescript Tagless Final Examples

Direct port to Purescript (from Haskell) of two examples of the finally tagless, partially evaluated pattern.

There are two examples here:

* barebones example (SimpleEx directory) comes from Oleg Kiselyov's course notes example, [here](http://okmij.org/ftp/tagless-final/)
* side-effecting example (MonadicEx directory) comes from the example in Phil Freeman's talk, [here](https://www.youtube.com/watch?v=8DdyWgRYEeI)

Each example is a standalone Pulp project, if you do a `bower init; pulp build` in each directory you should be able to verify that the code compiles (you can do a `pulp run` too, if you like but the examples don't actually _do_ very much. The Monadic example also really only proves the concept generally but not usefully, showing String implementation and a partial Node.FS implementation which only implements `ls`)

Additional information:

* Phil Freeman's talk [slides](https://github.com/paf31/haskell-slides/tree/master/hoas)
* Oleg Kiselyov's [page](http://okmij.org/ftp/tagless-final/) on Typed Final (Finally Tagless) Style
* [StackOverflow question]((http://stackoverflow.com/questions/42166789/how-to-implement-monadic-instance-of-a-finally-tagless-typeclass-in-purescript/42168190#42168190) about having an effectful (Eff) instance for Node.FS alongside the non-Eff version, with informative answer by garyb:
