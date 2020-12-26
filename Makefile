all: aoc2019 aoc2020

aoc2019: \
	python_2019_01 python_2019_02 python_2019_03 python_2019_04 python_2019_05 \
	python_2019_06 python_2019_07 python_2019_08 python_2019_09 python_2019_10

aoc2020: \
	python_2020_01 python_2020_02 python_2020_03 python_2020_04 python_2020_05 \
	python_2020_06 python_2020_07 python_2020_08 python_2020_09 python_2020_10 \
	python_2020_11 python_2020_12 python_2020_13 python_2020_14 python_2020_15 \
	python_2020_16 python_2020_17 python_2020_18 python_2020_19 python_2020_20 \
	python_2020_21 python_2020_22 python_2020_23 python_2020_24 python_2020_25

# Run and show the runtime in seconds
python_%:
	@/bin/echo -n "$@: "
	@/usr/bin/time -p python python/aoc_$*.py < ../aoc-data/input/$*.txt > out/python_$*.txt 2> out/python_time_$*.txt
	@head -1 out/python_time_$*.txt | cut -c 14-
	@diff out/python_$*.txt ../aoc-data/good/$*.txt

check:
	black --quiet python/
	flake8 python/
	mypy python/

clean:
	-rm -f out/*