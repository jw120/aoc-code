.PHONY: all run test format

all: format test run

format:
	raco fmt -i src/*.rkt

test:
	raco test src/*.rkt

run: run_2018 run_2024

run_2018: rkt_2018_01 rkt_2018_02 rkt_2018_03 rkt_2018_04 rkt_2018_05

run_2024: rkt_2024_01

OUT_DIR = out

rkt_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@racket src/rkt_$*.rkt <../../aoc-data/input/$*.txt >$(OUT_DIR)/rkt_$*.txt
	@diff $(OUT_DIR)/rkt_$*.txt ../../aoc-data/good/$*.txt && /bin/echo "OK"

init:
	mkdir out
	raco pkg install fmt