
.PHONY: all check run run_2022 run_2023

PYTHON_SRC := src/aoc_2021_*.py src/aoc_2022_*.py src/aoc_2023_*.py

all: check run

run: run_2022 run_2023

run_2022: \
	python_2022_01 python_2022_02 python_2022_03 python_2022_04 python_2022_05 \
	python_2022_06 python_2022_07 python_2022_08 python_2022_09 python_2022_10 \
	python_2022_11 python_2022_12 python_2022_13 python_2022_14 python_2022_15 \
	python_2022_16 python_2022_17 python_2022_18 python_2022_19 python_2022_20 \
	python_2022_21 python_2022_22 python_2022_23 python_2022_24 python_2022_25

run_2023: \
	python_2023_01 python_2023_02 python_2023_03

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


