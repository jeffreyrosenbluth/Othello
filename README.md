Othello
=======

Threepenny-gui example of Othello
---------------------------------

Based on the original code of [Reed Rosenbluth](https://github.com/ReedR95/othello) (c) 2014.

To do
-----

- [X] Threepenny-gui / FRP gui is working
- [X] Compture moves on it's own
- [ ] Suggest move button
- [X] Heuristic
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

