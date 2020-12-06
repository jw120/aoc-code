all: aoc2019 aoc2020

aoc2019: \
	python_2019_01	python_2019_02 python_2019_03 python_2019_04 python_2019_05

aoc2020: \
	python_2020_01 python_2020_02 python_2020_03 python_2020_04 python_2020_05 \
	python_2020_06

python_%:
	@/bin/echo -n "$@: "
	@python python/aoc_$*.py < input/$*.txt | diff - good/$*.txt && echo OK


check:
	black --quiet python/
	flake8 python/
	mypy python/
