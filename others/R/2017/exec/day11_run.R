#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_str <- head(readLines("inputs/day11.txt", n = 1, ok = FALSE))
output_str <- day11(input_str)
cat(output_str, file = "outputs/11.txt")
