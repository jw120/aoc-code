# Advent of Code

My solutions to advent of code. So far contains:

 * Complete 2020 in Python (done in 2020 advent season)
 * Partial 2019 in Python (started in 2020 advent season)
 * Partial 2018 in Haskell (started in 2018 advent season)

Assumes `../aoc-data` (which is a private repo) holds personalized input data and solutions.


## TODO

- Haskell 2018 - get doctest to work
- Haskell 2018 - get all existing code working
- Haskell 2018 - fill in missing
- Haskell 2018 - linting and warnings
- Python 2020 - Speed up slow solutions
- Python 2020 - Avoid duplication in Day 17
- R etc (add from aoc folder)

## Haskell setup

Uses stack

Built with the "Haskell" VS Code extenstion that uses the Haskell Language Server


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


