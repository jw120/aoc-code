.PHONY: all run test

all: test run

test:
	raco test day01.rkt

run: rkt_2024_01

OUT_DIR = out

rkt_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@racket rkt_$*.rkt <../../../../aoc-data/input/$*.txt >$(OUT_DIR)/rkt_$*.txt
	@diff $(OUT_DIR)/rkt_$*.txt ../../../../aoc-data/good/$*.txt && /bin/echo "OK"
