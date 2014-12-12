Othello
=======

Threepenny-gui example of Othello
-------------------------------------

To do
-----

- [X] Threepenny-gui / FRP gui is working
- [X] Two player mode
- [X] Suggest move button
- [X] Heuristic
- [ ] Comment and document the source code
- [ ] Show number of squares each player has
- [X] Add module to test the AI
- [ ] Move syle info to static/css instead of inline
- [X] Add minimax or negamax search
- [X] Add alph-beta pruning
- [X] Order legal moves for better alpha-beta pruning
- [ ] Parallelize computations (maybe Repa?).

To run
------

  - $ cabal build
  - $ ./dist/build/othello/othello
  - point browser at localhost:8023

To test
------

  - $ cabal install --enable-tests
  - $ cabal repl tests
  - *Main> main

