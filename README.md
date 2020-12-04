Advent of Code

# Python setup

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

Editting done with VS Code and pylance


