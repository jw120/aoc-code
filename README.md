# Advent of Code

My solutions to advent of code.

Assumes `../aoc-data` (which is a private repo) holds personalized input data and solutions.


## TODO
- When mypy supports 3.9, use list[] instead of List as a type

- Add timing
- Speed up 2020/11
- Avoid duplication (and speeed up 2020/17): Grid[Coord3]?
- Review use of if key in (and try .setdefault)
- Try using Counter


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


