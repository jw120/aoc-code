# Given a character vector of lines, return the number which have no duplicate words
#' @importFrom magrittr %>%
day04 <- function(lines) {

  # argument lines is a character vector
  stopifnot(is.character(lines))

  lines %>%

    # remove leading whitespace
    sub("^\\s+", "", .) %>%

    # remove trailing whitespace
    sub("\\s+$", "", .) %>%

    # keep only non-empty lines
    grep(".", ., value = TRUE) %>%

    # split lines on whitespace (gives a list of character vectors)
    strsplit("\\s+") %>%

    # Check which lines are duplicate-free
    purrr::map_lgl(day04_dup_free) %>%

    # And return the number of TRUEs
    sum
}

# Given a vector of characters, return TRUE if none are duplicates
day04_dup_free <- function(ws) {
  !anyDuplicated(ws)
}