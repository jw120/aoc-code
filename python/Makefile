
.PHONY: all check run run_2022 run_2023

PYTHON_SRC := src/

all: check run

run: run_2020 run_2021 run_2022 run_2023

run_2019: \
	python_2019_01 python_2019_02 python_2019_03 python_2019_04 python_2019_05 \
	python_2019_06 python_2019_07 python_2019_08 python_2019_09 python_2019_10 \
	python_2019_11 python_2019_12 python_2019_13 python_2019_14 python_2019_15 \
	python_2019_16 python_2019_17 python_2019_18 python_2019_19 python_2019_20 \
	python_2019_21 python_2019_22 python_2019_23 python_2019_24 python_2019_25

run_2020: \
	python_2020_01 python_2020_02 python_2020_03 python_2020_04 python_2020_05 \
	python_2020_06 python_2020_07 python_2020_08 python_2020_09 python_2020_10 \
	python_2020_11 python_2020_12 python_2020_13 python_2020_14 python_2020_15 \
	python_2020_16 python_2020_17 python_2020_18 python_2020_19 python_2020_20 \
	python_2020_21 python_2020_22 python_2020_23 python_2020_24 python_2020_25

run_2021: \
	python_2021_01 python_2021_02 python_2021_03 python_2021_04 python_2021_05 \
	python_2021_06 python_2021_07 python_2021_08 python_2021_09 python_2021_10 \
	python_2021_11 python_2021_12 python_2021_13 python_2021_14 python_2021_15 \
	python_2021_16 python_2021_17 python_2021_18 python_2021_19 python_2021_20 \
	python_2021_21 python_2021_22 python_2021_23 python_2021_24 python_2021_25

run_2022: \
	python_2022_01 python_2022_02 python_2022_03 python_2022_04 python_2022_05 \
	python_2022_06 python_2022_07 python_2022_08 python_2022_09 python_2022_10 \
	python_2022_11 python_2022_12 python_2022_13 python_2022_14 python_2022_15 \
	python_2022_16 python_2022_17 python_2022_18 python_2022_19 python_2022_20 \
	python_2022_21 python_2022_22 python_2022_23 python_2022_24 python_2022_25

run_2023: \
	python_2023_01 python_2023_02 python_2023_03

run_2024: \
	python_2024_01 python_2024_02 python_2024_03 python_2024_04 python_2024_05 \
	python_2024_06 python_2024_07 python_2024_08

check:
	ruff format $(PYTHON_SRC)
	ruff check $(PYTHON_SRC)
	pyright $(PYTHON_SRC)

python-test:
	python src/golden_test.py

python-test-fast:
	python src/golden_test.py --fast

OUT_DIR = out

python_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@python src/aoc_$*.py < ../../aoc-data/input/$*.txt > $(OUT_DIR)/python_$*.txt
	@diff $(OUT_DIR)/python_$*.txt ../../aoc-data/good/$*.txt && /bin/echo "OK"


