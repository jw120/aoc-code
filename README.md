# Advent of Code

My solutions to advent of code. So far contains:

 * Complete 2020 in Python
 * Partial 2019 in Python

Assumes `../aoc-data` (which is a private repo) holds personalized input data and solutions.


## TODO - Python 2020 improvements

- Speed up slow solutions
- Avoid duplication in Day 17


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


