
.PHONY: all check

PYTHON_SRC := src/aoc_2023_*.py

all: check run_2023


run_2023: \
	python_2023_01 python_2023_02

check:
	ruff format $(PYTHON_SRC)
	ruff check $(PYTHON_SRC)
	pyright $(PYTHON_SRC)

python-test:
	python python/golden_test.py

python-test-fast:
	python python/golden_test.py --fast


OUT_DIR = out

# Python: run and show the runtime in seconds
python_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@/usr/bin/time -p python src/aoc_$*.py < ../../aoc-data/input/$*.txt > $(OUT_DIR)/python_$*.txt 2> $(OUT_DIR)/python_time_$*.txt
	@head -1 $(OUT_DIR)/python_time_$*.txt | cut -c 14-
	@diff $(OUT_DIR)/python_$*.txt ../../aoc-data/good/$*.txt


