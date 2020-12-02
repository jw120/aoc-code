run: day01

day%:
	@/bin/echo -n "$@: "
	@poetry run python src/$@.py < inputs/$@.txt | diff - good/$@.txt && echo OK


check:
	poetry run black src/
	poetry run flake8 src/
	poetry run mypy src/
