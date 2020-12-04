run: \
	python-2020-01 \
	python-2020-02 \
	python-2020-03

python-%:
	@/bin/echo -n "$@: "
	@python python/$*.py < input/$*.txt | diff - good/$*.txt && echo OK


check:
	black python/
	flake8 python/
	mypy python/
