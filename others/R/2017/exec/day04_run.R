#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_lines <- readLines("inputs/day04.txt")
output_str <- day04(input_lines)
cat(output_str, file = "outputs/04.txt")
