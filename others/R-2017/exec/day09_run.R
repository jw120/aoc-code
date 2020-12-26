#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_str <- head(readLines("inputs/day09.txt", n = 1, ok = FALSE))
output_str <- day09(input_str)
cat(output_str, file = "outputs/09.txt")
