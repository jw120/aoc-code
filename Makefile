all: aoc_2018 aoc_2019 aoc_2020

aoc_2018: \
	haskell_2018_01 haskell_2018_02 haskell_2018_03 haskell_2018_04 haskell_2018_05 \
	haskell_2018_06 haskell_2018_07 haskell_2018_08 haskell_2018_09 haskell_2018_10 \
	haskell_2018_11                 haskell_2018_13 haskell_2018_14 haskell_2018_15 \
	haskell_2018_16                 haskell_2018_18                 haskell_2018_20 \
	                                                haskell_2018_24

aoc_2019: \
	python_2019_01 python_2019_02 python_2019_03 python_2019_04 python_2019_05 \
	python_2019_06 python_2019_07 python_2019_08 python_2019_09 python_2019_10

aoc_2020: \
	python_2020_01 python_2020_02 python_2020_03 python_2020_04 python_2020_05 \
	python_2020_06 python_2020_07 python_2020_08 python_2020_09 python_2020_10 \
	python_2020_11 python_2020_12 python_2020_13 python_2020_14 python_2020_15 \
	python_2020_16 python_2020_17 python_2020_18 python_2020_19 python_2020_20 \
	python_2020_21 python_2020_22 python_2020_23 python_2020_24 python_2020_25

# Python: run and show the runtime in seconds
python_%:
	@/bin/echo -n "$@: "
	@/usr/bin/time -p python python/aoc_$*.py < ../aoc-data/input/$*.txt > out/python_$*.txt 2> out/python_time_$*.txt
	@head -1 out/python_time_$*.txt | cut -c 14-
	@diff out/python_$*.txt ../aoc-data/good/$*.txt

# Haskell: run and show the runtime in seconds (assumes stack build has already been run)
haskell_%:
	@/bin/echo -n "$@: "
	@/usr/bin/time -p stack run aoc_$* < ../aoc-data/input/$*.txt > out/haskell_$*.txt 2> out/haskell_time_$*.txt
	@head -1 out/haskell_time_$*.txt | cut -c 14-
	@diff out/haskell_$*.txt ../aoc-data/good/$*.txt

check:
	black --quiet python/
	flake8 python/
	mypy python/
	hlint haskell/

clean:
	-rm -f out/*