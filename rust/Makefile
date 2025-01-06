.PHONY: all check run run_2022 run_2023 run_2024

all: check run

run: run_2022 run_2023 run_2024

run_2022: \
	rust_2022_01 rust_2022_02 rust_2022_03 rust_2022_04 rust_2022_05 \
	rust_2022_06 rust_2022_07 rust_2022_08 rust_2022_09 rust_2022_10 \
	rust_2022_11 rust_2022_12 rust_2022_13 rust_2022_14 rust_2022_15

run_2023: \
	rust_2023_01 rust_2023_02 rust_2023_03 rust_2023_04 rust_2023_05 \
	rust_2023_06 rust_2023_07 rust_2023_08 rust_2023_09              \
	rust_2023_11              rust_2023_13 rust_2023_14 rust_2023_15 \
	rust_2023_16 

run_2024: \
	rust_2024_01 rust_2024_02 rust_2024_03 rust_2024_04

check:
	cargo fmt --quiet
	cargo clippy --quiet -- -W clippy::pedantic
	cargo check --quiet

OUT_DIR = out

rust_%: | $(OUT_DIR)
	@/bin/echo -n "$@: "
	@cargo build --quiet --bin=aoc_$*
	@time -o $(OUT_DIR)/time_$*.txt -p cargo run --quiet --bin=aoc_$* < ../../aoc-data/input/$*.txt > $(OUT_DIR)/rust_$*.txt
	@diff $(OUT_DIR)/rust_$*.txt ../../aoc-data/good/$*.txt && /bin/echo -n "OK" && (grep real $(OUT_DIR)/time_$*.txt | sed s/real//)

