# Advent of Code

My solutions to advent of code. So far contains:

 * Complete 2020 in Python (done in 2020 advent season)
 * Partial 2019 in Python (started in 2020 advent season)
 * Partial 2018 in Haskell (started in 2018 advent season)

 In addition the `others` folder contains a few other attempts
 * 2017 in R
 * 2018 in racket

Assumes `../aoc-data` (which is a private repo) holds personalized input data and solutions.


## Comments

* 2020 my first time completing. No major problems.

    + Done without hints except looking up the Chinese Remainder Theorem (Day 13),
    + Day 20 was the most work - lot of fiddly proessing to fit the images together, but the sea monster is a nice pay off
    + Briefly stuck finding a fast method for day 23 (crab cups), saw on reddit the hint to use another data structure
    + Lots of Life

* 2019

    + Stuck on day 12 (fast method for simulating four body problem)
    + Day 13 (IntCode pong game) was fun

* 2018

    + Going back to these in Haskell in 2020/21 after doing 2019/20 in Python - clear that Python is a better fit for smaller programs were iterative improvement works, Haskell takes more effort to do easy things
    + Day 17 (water pouring) not finished in 2018, solved in 2020 after briefly looking at reddit discussion and seeing I had the wrong definition of the extent of the map for couting the final score.

## TODO

- Haskell 2018 - get doctest to work
- Haskell 2018 - fill in missing
- Haskell 2018 - linting and warnings
- Python 2020 - Speed up slow solutions
- Python 2020 - Avoid duplication in Day 17
- R etc (add from aoc folder)

## Haskell setup

Uses stack

Built with the "Haskell" VS Code extenstion that uses the Haskell Language Server

stack build aoc-code:aoc_2018_12
stack exec aoc_2018_17 <../aoc-data/input/2018_17.txt
stack ghci aoc-code:aoc_2018_17
stack runhaskell haskell/aoc_2018_17.hs <../aoc-data/input/2018_17.txt

## Python setup

Assumes modern python (tested with 3.9) installed as `python3` and `pip3`.

Best run in a venv using the following (with this directory as the current directory)

To set up the local environment (using `python3` to get the newest system installed version
of python)

```{sh}
python3 -m venv .venv
```

To use the environnment for the current shell session. This adpots the version of python used
when setting up the environment (and aliases `python` and `pip` to that version) and redirects
them to use `.venv/lib` for python libraries

```{sh}
source .venv/bin/activate
```

To install development packages into the local environment

```{sh}
pip install -r requirements.txt
```

Editting done with VS Code and its built-in extension


