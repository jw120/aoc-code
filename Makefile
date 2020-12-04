run: \
	python_2019_01 \
	python_2020_01 \
	python_2020_02 \
	python_2020_03 \
	python_2020_04

python_%:
	@/bin/echo -n "$@: "
	@python python/aoc_$*.py < input/$*.txt | diff - good/$*.txt && echo OK


check:
	black python/
	flake8 python/
	mypy python/
