.PHONY: all check run run_2023

all: check run

run: run_2023

run_2023: \
	rust_2023_01 rust_2023_02 rust_2023_03 rust_2023_04 rust_2023_05 \
	rust_2023_06

check:
	cargo fmt --quiet
	cargo clippy --quiet
	cargo check --quiet

OUT_DIR = out

rust_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@cargo run --quiet --bin=aoc_$* < ../../aoc-data/input/$*.txt > $(OUT_DIR)/rust_$*.txt
	@diff $(OUT_DIR)/rust_$*.txt ../../aoc-data/good/$*.txt && /bin/echo "OK"


