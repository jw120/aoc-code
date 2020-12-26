#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_lines <- readLines("inputs/day02.txt")
output_str <- day02(input_lines)
cat(output_str, file = "outputs/02.txt")
