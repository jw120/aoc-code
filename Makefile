check: day01

day%:
	@/bin/echo -n "$@: "
	@poetry run python src/$@.py | diff - good/$@.txt && echo OK
