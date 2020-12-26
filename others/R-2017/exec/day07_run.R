#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_lines <- readLines("inputs/day07.txt")
output_str <- day07(input_lines)
cat(output_str, file = "outputs/07.txt")
