# Bull-s-eye

This project involves implementing, in OCaml, a program that computes all possible ways to checkout in a game of darts (501/301). A checkout consists of reducing a given score to exactly zero using at most three darts, with the constraint that the final dart must be a double or the inner bull (50). Not all scores can be checked out (for example, scores above 170).

Dart throws are represented using an algebraic data type (S, D, T), and a checkout is a list of up to three throws.

The work is divided into two tasks:

Task 1: compute all valid checkout sequences, considering different orders as distinct.

Task 2: remove equivalent sequences that differ only by the order of throws, while still respecting the constraint on the final dart.

The solution must follow an inductive programming style and comply with the provided specifications.

The project uses dune for building, running, and testing:

Run a task:
dune exec bullseye <task1|task2> <score>

Run tests:
dune runtest or task-specific test commands
