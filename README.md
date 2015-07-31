Chess in Elm
============

This is an experiment of porting [Chess in
Haskell)](https://github.com/mtak/chess-hs). Elm and Haskell share a lot of
syntax and language features so this was fairly straight forward. The Haskell
code makes use of certain features not available in Elm, particularly
monads and list comprehensions.

You can try it out at (http://mtak.github.io/chess-elm/). You can build the
code by running `elm-make Main.elm --output index.html` and finding the
resulting file in your browser, or you can try it out using code you can either
run it out by launching `elm-reactor` and navigating to
http://localhost:8000/Main.elm.
