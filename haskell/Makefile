
all: aoc_2018 aoc_2019 aoc_2020

run_2018: \
	haskell_2018_01 haskell_2018_02 haskell_2018_03 haskell_2018_04 haskell_2018_05 \
	haskell_2018_06 haskell_2018_07 haskell_2018_08 haskell_2018_09 haskell_2018_10 \
	haskell_2018_11                 haskell_2018_13 haskell_2018_14 haskell_2018_15 \
	haskell_2018_16 haskell_2018_17 haskell_2018_18                 haskell_2018_20 \
	                                                haskell_2018_24

OUT_DIR = out

haskell_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@/usr/bin/time -p stack run aoc_$* < ../aoc-data/input/$*.txt > $(OUT_DIR)/haskell_$*.txt 2> $(OUT_DIR)/haskell_time_$*.txt
	@grep real $(OUT_DIR)/haskell_time_$*.txt | cut -c 14-
	@diff $(OUT_DIR)/haskell_$*.txt ../aoc-data/good/$*.txt

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

check:
	hlint haskell/

