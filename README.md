# Advent of Code

My solutions to advent of code.  So far contains:

 * Ongoing 2021 in Python and Haskell (done in 2021 advent season)
 * Complete 2020 in Python (done in 2020 advent season)
 * Partial 2019 in Python (started in 2020 advent season)
 * Partial 2018 in Haskell (started in 2018 advent season)

 In addition the `others` folder contains a few other attempts
 * 2017 in R
 * 2018 in racket

Assumes `../aoc-data` (which is a private repo) holds personalized input data and solutions.

## Haskell code

Single executable with one module per day that provides a solver solver function. Uses tasty for unit tests
which includes Golden file testing that creates and checks ../aoc-data/good files

 implicit-hie used for hie.

```{haskell}
stack run 2015 1
stack test --test-arguments=--pattern=2018
stack test --test-arguments=--pattern=fast
```

## Python code

Assumes modern python (tested with 3.9) installed as `python3` and `pip3`.

```
python python/aoc_2021_05.py <../aoc-data/input/2021_05.txt
python python/golden_test.py --fast
python python/golden_test.py
```

TODO - change golden_test to look at which files exist

Doctest for tests

Best run in a venv using the following (with this directory as the current directory). To set up the local environment (using `python3` to get the newest system installed version of python)

```{sh}
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Comments

TODO

- Haskell 2018 - fill in missing
- Haskell 2018 - linting and warnings
- Python 2020 - Speed up slow solutions
- Python 2020 - Avoid duplication in Day 17
- R etc (add from aoc folder)

To improve
* 2015-04 (17s) - MD5s - find a non-naive solution that is faster
* 2015-06 (18s) - Array updates - faster

Try 2021-03 in numpy

Can we make solvers have type: (Show a, Show b) => Text -> (a, b) without having string output printed within quotes

Hints used - 2015/9 - ran web code to spot my error (missing a city)
    + 2018/19 googled a hint before disassembling

* 2021

* 2020 my first time completing. No major problems.

    + Done without hints except looking up the Chinese Remainder Theorem (Day 13),
    + Day 20 was the most work - lot of fiddly processing to fit the images together, but the sea monster is a nice pay off
    + Briefly stuck finding a fast method for day 23 (crab cups), saw on reddit the hint to use another data structure
    + Lots of Life

* 2019

    + Stuck on day 12 (fast method for simulating four body problem)
    + Day 13 (IntCode pong game) was fun

* 2018

    + Going back to these in Haskell in 2020/21 after doing 2019/20 in Python - clear that Python is a better fit for smaller programs were iterative improvement works, Haskell takes more effort to do easy things
    + Day 17 (water pouring) not finished in 2018, solved in 2020 after briefly looking at reddit discussion and seeing I had the wrong definition of the extent of the map for counting the final score.















