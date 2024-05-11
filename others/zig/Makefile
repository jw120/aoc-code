
all: build zig

build:
	zig build

zig: \
	zig_2022_01 zig_2022_02 zig_2022_03 zig_2022_04 zig_2022_05 \
	zig_2022_06 zig_2022_07 zig_2022_08 zig_2022_09 zig_2022_10 \
	zig_2022_11 zig_2022_12 zig_2022_13 zig_2022_14 zig_2022_15

OUT_DIR = out

# Python: run and show the runtime in seconds
zig_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@/usr/bin/time -p ./zig-out/bin/aoc_$* < ../aoc-data/input/$*.txt > $(OUT_DIR)/zig_$*.txt 2> $(OUT_DIR)/zig_time_$*.txt
	@head -1 $(OUT_DIR)/zig_time_$*.txt | cut -c 6-
	@diff $(OUT_DIR)/zig_$*.txt ../aoc-data/good/$*.txt && /bin/echo "OK"

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

clean:
	-rm -fr out/* zig-cache/* zig-out/*
