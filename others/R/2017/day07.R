# Given a char vector of the input lines, return the name of the bottom program
day07 <- function(lines) {

  # Parse the input lines
  parsed_table <- day07_parse(lines)

  # All the program names
  prog_names <- parsed_table$prog

  # All the programs which are above another program
  above_names <- unlist(parsed_table$above)

  # Bottom-most program is the one not above any other
  Filter(function(s) !(s %in% above_names), prog_names)

}

# Parse vector of input lines, returning a tibble with
# column prog - vector of program names
# column above - list of column vectors
day07_parse <- function(lines) {

  # Regex to match program name at beginning of string
  front_pattern <- "^([[:alpha:]]+).*"

  # Regex to match everything before the list of above programs at the end of the line
  strip_pattern <- "^[[:alpha:]]+[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+->[[:space:]]+"

  prog = sub(front_pattern, "\\1", lines)

  have_above <- grepl(strip_pattern, lines)
  above_strings <- sub(strip_pattern, "", lines)
  above <- purrr::map2(have_above, above_strings, function(has_above, above_string) {
    if (has_above) {
      strsplit(above_string, ",[[:space:]]+")[[1]]
    } else {
      character(0)
    }
  })

  tibble::tibble(prog = prog, above = above)
}