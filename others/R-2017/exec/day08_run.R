#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_lines <- readLines("inputs/day08.txt")
output_str <- day08(input_lines)
cat(output_str, file = "outputs/08.txt")
