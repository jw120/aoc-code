#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_str <- head(readLines("inputs/day06.txt", n = 1, ok = FALSE))
output_str <- day06(input_str)
cat(output_str, file = "outputs/06.txt")
