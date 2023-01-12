
all: aoc_2018 aoc_2019 aoc_2020

aoc_2018: \
	haskell_2018_01 haskell_2018_02 haskell_2018_03 haskell_2018_04 haskell_2018_05 \
	haskell_2018_06 haskell_2018_07 haskell_2018_08 haskell_2018_09 haskell_2018_10 \
	haskell_2018_11                 haskell_2018_13 haskell_2018_14 haskell_2018_15 \
	haskell_2018_16 haskell_2018_17 haskell_2018_18                 haskell_2018_20 \
	                                                haskell_2018_24

aoc_2019: \
	python_2019_01 python_2019_02 python_2019_03 python_2019_04 python_2019_05 \
	python_2019_06 python_2019_07 python_2019_08 python_2019_09 python_2019_10 \
	python_2019_11                python_2019_13 python_2019_14 python_2019_15

aoc_2020: \
	python_2020_01 python_2020_02 python_2020_03 python_2020_04 python_2020_05 \
	python_2020_06 python_2020_07 python_2020_08 python_2020_09 python_2020_10 \
	python_2020_11 python_2020_12 python_2020_13 python_2020_14 python_2020_15 \
	python_2020_16 python_2020_17 python_2020_18 python_2020_19 python_2020_20 \
	python_2020_21 python_2020_22 python_2020_23 python_2020_24 python_2020_25

aoc_2022: \
	python_2022_01 python_2022_02 python_2022_03

python-test:
	python python/golden_test.py

python-test-fast:
	python python/golden_test.py --fast


OUT_DIR = out
		
# Python: run and show the runtime in seconds
python_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@/usr/bin/time -p python python/aoc_$*.py < ../aoc-data/input/$*.txt > $(OUT_DIR)/python_$*.txt 2> $(OUT_DIR)/python_time_$*.txt
	@head -1 $(OUT_DIR)/python_time_$*.txt | cut -c 14-
	@diff $(OUT_DIR)/python_$*.txt ../aoc-data/good/$*.txt

# Haskell: run and show the runtime in seconds (assumes stack build has already been run)
haskell_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@/usr/bin/time -p stack run aoc_$* < ../aoc-data/input/$*.txt > $(OUT_DIR)/haskell_$*.txt 2> $(OUT_DIR)/haskell_time_$*.txt
	@grep real $(OUT_DIR)/haskell_time_$*.txt | cut -c 14-
	@diff $(OUT_DIR)/haskell_$*.txt ../aoc-data/good/$*.txt

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

check: python-check haskell-check

python-check:
	isort python/
	black --quiet python/
	pyright python/
	pylint python/aoc_2022_*.py python/golden_test.py python/coord.py

haskell-check:
	hlint haskell/

clean:
	-rm -f out/*
