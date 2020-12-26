#!/usr/bin/env Rscript

devtools::load_all(quiet = TRUE)

input_str <- head(readLines("inputs/day03.txt", n = 1, ok = FALSE))
output_str <- day03(as.numeric(input_str))
cat(output_str, file = "outputs/03.txt")
