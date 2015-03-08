Formula-2-bdd - FLP 2015 Haskell projekt
========================================
author: Petr Šebek xsebek02

About
-----
Objective of this school project is to transform disjunctive normal form (DNF)
to binary decision diagram (BDD, more specifically ordered - OBDD) and then reduced
to reduced ordered binary decision diagrams (ROBDD). It is written in Haskell as
an introduction to this language.

Run
---
Source code is located only to file formula-2-bdd.hs. For compilation
run `make`. This command will create executable file `formula-2-bdd`.

Usage
-------
```
./formula-2-bdd [-i | -v | -b | -t | -r | -c] DNF_FILE

-i - print DNF representation
-t - print truth table
-b - print BDD
-r - print ROBDD

extra:
-v - print variables used in formula
-c - check whether generated BDD and ROBDD correspond to truth table
```


Print BDD/ROBDD
---------------
BDD is printed according to format from specification thus:
```
a->0
a=>b->0
a=>b=>1
```
For ROBDD where `a` leads via `low` to 0, via `high` to `b` and `b` lead via `low` to
0 and via `high` to 1. If is whole BDD reduced to 0 or 1 is printed out only this value.

Tests
-----
In directory `tests/` is located few DNF examples (suffix `.dnf`) and corresponding
truth tables (`.tt`) and ROBDD (`.rbdd`). You can test that BDD and ROBDD agree
with truth table with option `-c`.
