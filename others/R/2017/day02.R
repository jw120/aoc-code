
day02 <- function(lines) {
  day02_checksum(day02_parse(lines))
}

# Takes a character vector whose elements are white-space separated ints
# returns a list of numeric vectors
# strips any blank lines
day02_parse <- function(lines) {

  # argument lines is a character vector
  stopifnot(is.character(lines))

  lines %>%

    # remove leading whitespace
    sub("^\\s+", "", .) %>%

    # remove trailing whitespace
    sub("\\s+$", "", .) %>%

    # keep only non-empty lines
    grep(".", ., value = TRUE) %>%

    # split lines on whitespace
    strsplit("\\s+") %>%

    # convert character vectors to numberic vectors
    purrr::map(as.numeric)

}

# Given a list of numeric vectors, return the sum of each line's
# difference between smallest and largest elements
day02_checksum <- function(xs) {

  sum(purrr::map_dbl(xs, function(v) max(v) - min(v)))

}

