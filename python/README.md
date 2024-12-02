# Python Advent of Code

## TODO

* Check all run

  * 2019 08 (broken in golden)
  * 2020 14
  * 2020 19
  * 2022 24 (very slow)
  * 2023-04 and beyond not tested

* Check 2022-24... working
* Check 2021 working
* Refactor golden_test

## Installation

* Uses `devbox` to provide python and pip (specific in `devbox.json`)
* Uses `direnv` to trigger devbox on changing to this directory (in `.envrc`)
* Uses `pip` to install python packages (requires `pip install -r requirements.txt`)
* For VS Code - best to open from within the directory. Can use

    /Applications/Visual\ Studio\ Code.app/Contents/Resources/app/bin/code .

## Tools note

* We rely on pyright's exhaustiveness checker for enums, so we don't use assert_never
