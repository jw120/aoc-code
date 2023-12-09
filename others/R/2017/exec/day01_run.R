#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_str <- head(readLines("inputs/day01.txt", n = 1, ok = FALSE))
output_str <- day01(input_str)
cat(output_str, file = "outputs/01.txt")
