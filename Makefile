run: \
	python-2020-01 \
	python-2020-02 \
	python-2020-03

python-%:
	@/bin/echo -n "$@: "
	@poetry run python python/$*.py < input/$*.txt | diff - good/$*.txt && echo OK


check:
	poetry run black python/
	poetry run flake8 python
	poetry run mypy python/
