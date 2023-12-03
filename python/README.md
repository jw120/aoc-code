

# TODO

* Check 2022-11 and 2022-16... working
* Check 2021 working
* Ruff clean 2019-20
* pyright clean 2019-20
* Check running 2019-20
* Refactor golden_test


# Installation

* Uses `devbox` to provide python and pip (specific in `devbox.json`)
* Uses `direnv`` to trigger devbox on changing to this directory (in `.envrc`)
* Uses `pip` to install python packages (requires `pip install -r requirements.txt `)
* For VS Code - best to open from within the directory. Can use 

    /Applications/Visual\ Studio\ Code.app/Contents/Resources/app/bin/code .


# Tools note

* We rely on pyright's exhaustiveness checker for enums, so we don't use assert_never