# Advent of Code

TODO:
* Finish 2021
* Repair aoc_2021_22.py
* Use file input for all python
* Lint all python

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

Assumes Python 3.10 available. Uses poetry dependency manager (to align with repl.it)
```
pipx install poetry
```

Use poetry's virtual environment and install packages
```
poetry shell
poetry install
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









# Problem descriptions

TODO - Check haskell files which are not included in stack test --test-arguments=--pattern=fast

Time = Python execution time in
Lines = number of python lines in solution (using pygount)
Trick: 0 = just implement the instructions, 1 = needs some thought, 2 = depends on insight
Done: Languages implements (p=python, j=julia, h=haskell)

            Area            Description                             Data structures                     Time    Lines   Trick   Done

2021-01     Sonar           Sequence differences and averaging      List                                60      11      0       hjp
2021-02     Submarine       Command parsing                         List                                70      32      0       hjp
2021-03     Binary          Most common bit n in a set of numbers   List, Binary strings                80      50      1       hjp
2021-04     Bingo           First and last bingo card to win        Array (5x5)                         140     64      1       hjp
2021-05     Vents           Coordinates covered by multiple lines   Dict (counts of coordinates)        230     55      1       hjp
2021-06     Lanternfish     Number of fish with iteration rules     List                                60      17      2       hjp
2021-07     Crabs           Minimize cost to align crabs            Binary search                       70      31      1       hjp
2021-08     7 segment       Logical matching                        Dict, Set                           68      69      1       p
2021-09     Smoke           Minima and basins                       Array (2d)                          87      69      1       p
2021-10     Syntax          Bracket matching                        Stack                               61      50      1       p
2021-11     Octopus flash   Life-like Grid with overloads           Array (10x10)                       304     59      1       p
2021-12     Caves           Path counting                           Dict (links between rooms)          994     87      1       p
2021-13     Origami         Grid folding                            List, set                           69      66      1       p
2021-14     Polymerization  Apply reaction rules to chain           Dict (counts)                       64      60      2       p

2022-01     Calories        Add groups of numbers                   List
2022-02     RPS             Rock, paper, scissors
2022-03
2022-15     Beacons         TODO B
2022-16     Valves          Graph walk - TODO B: double walker
2022-17     Pyroclastic     Tetris - TODO B: very long time
2022-18     Boiling         3-d flood fill                          Set
2022-19
2022-20     Groves          Circular list mixing                    List
2022-21     Math            Tree of math operations
2022-22     Map             Movement on cube surface
2022-23     Diffusion       Life-like                               Set




59	Python	python	python/aoc_2021_11.py
87	Python	python	python/aoc_2021_12.py
66	Python	python	python/aoc_2021_13.py
60	Python	python	python/aoc_2021_14.py
71	Python	python	python/aoc_2021_15.py
64	Python	python	python/aoc_2021_17.py
337	Python	python	python/aoc_2021_19.py
97	Python	python	python/aoc_2021_20.py
69	Python	python	python/aoc_2021_21.py
182	Python	python	python/aoc_2021_22.py
283	Python	python	python/aoc_2021_23.py
118	Python	python	python/aoc_2021_24.py
100	Python	python	python/aoc_2021_25.py

2021 11: ok (304 ms)
2021 12: ok (994 ms)
2021 13: ok (69 ms)
2021 14: ok (64 ms)
2021 21: ok (424 ms)

