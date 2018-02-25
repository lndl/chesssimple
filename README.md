# Chesssimple

Chesssimple is a very simple Chess game that implements almost all the chess rules and includes a very simple AI. The purpose of this project is to learn Haskell and functional programming concepts; in fact, this was my project of a semestral functional programming course at UNLP.

### Instalation and execution:

```bash
./chesssimple> cabal update
./chesssimple> cabal install
./chesssimple> ./dist/build/chesssimple/chesssimple
```

### Executable options and help

There are 3 game modes with their own options:

- hvc: Human vs Computer (default)
- hvh: Human vs Human
- cvc: Computer vs Computer

```
The Chesssimple program

Chesssimple [COMMAND] ... [OPTIONS]
  Chess game & engine

Common flags:
  -? --help            Display help message
  -V --version         Print version information

Chesssimple hvh [OPTIONS]

     --p1name=ITEM     The player 1's name
     --p1color=ITEM    The player 1's color
     --p2name=ITEM     The player 2's name

Chesssimple [hvc] [OPTIONS]

     --hname=ITEM      The human player's name
     --hcolor=ITEM     The human player's color
  -c --cstrength=INT   The computer player's strength

Chesssimple cvc [OPTIONS]

     --p1strength=INT  The player 1's strength
     --p2strength=INT  The player 2's strength
```

For instance:
```bash
./dist/build/chesssimple/chesssimple hvc --cstrength=2 --hcolor=black
```
Will start a game where human player will be the black team and computer game will have a strength of 2 (very easy)

### Execution with threads:

Add these parameters to the chesssimple command: +RTS -N*t*, where *t* is the number of threads. For example:
```bash
./chesssimple> ./dist/build/chesssimple/chesssimple hvc --cstrength=2 --hcolor=black +RTS -N4
```
Will start a game with 4 threads.

### Tests

Run it with:
```bash
./chesssimple> ghci < runtests
```

### More docs:

There is a .tex file which contains technical information of Chesssimple **(only in Spanish)**

### TODOs:

- Implement castling
- Implement *in passant* rule
- Implement a better piece promotions
- Add and improve all tests suit.
