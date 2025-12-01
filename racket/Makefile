.PHONY: all run test format

all: format test run

format:
	raco fmt -i src/*.rkt

test:
	raco test src/*.rkt

run: run_2018 run_2024 run_2025

run_2018: aoc_2018_01 aoc_2018_02 aoc_2018_03 aoc_2018_04 aoc_2018_05

run_2024: aoc_2024_01

run_2025: aoc_2025_01

OUT_DIR = out

aoc_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@racket src/aoc_$*.rkt <../../aoc-data/input/$*.txt >$(OUT_DIR)/aoc_$*.txt
	@diff $(OUT_DIR)/aoc_$*.txt ../../aoc-data/good/$*.txt && /bin/echo "OK"

init:
	mkdir out
	raco pkg install fmt