#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_lines <- readLines("inputs/day05.txt")
output_str <- day05(input_lines)
cat(output_str, file = "outputs/05.txt")
