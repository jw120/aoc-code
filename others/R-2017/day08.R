day08 <- function(lines) {

  # Parse all the input lines into a table
  parsed <- day08_parse(lines)

  # Create an environment to hold all the registers referred to
  register_names <- unique(c(parsed$target, parsed$cond_reg))
  registers <- new.env(parent = emptyenv())
  for (r in register_names) {
    registers[[r]] <- 0
  }

  # Run the program by iterating over the rows of the parsed data
  purrr::pwalk(parsed, function(target, delta, cond_reg, cond_type, cond_val) {
    if (day08_cond(registers[[cond_reg]], cond_type, cond_val)) {
      registers[[target]] <- registers[[target]] + delta
    }
  })

  # Return the highest value
  max(purrr::map_dbl(register_names, ~ registers[[.]]))

}

# Test to see if the given condition is true
day08_cond <- function(x, cond, y) {
  switch(cond,
    "<" = x < y,
    ">" = x > y,
    "<=" = x <= y,
    ">=" = x >= y,
    "==" = x == y,
    "!=" = x != y,
    stop("Unknown condition: ", cond)
  )
}

# Parsed the input lines and returns a table with one row per instruction
# target - register to be changed
# delta - amount to increment the register
# cond_reg - register to test the condition on
# cond_type - "<", ">", "<=", ">=", "==", "!="
# cond_val - integer value for the comparison
day08_parse <- function(lines) {

  # Regex pattern
  pattern <- "^([[:alpha:]]+) (inc|dec) (-?[[:digit:]]+) if ([[:alpha:]]+) (<|>|<=|>=|==|!=) (-?[[:digit:]]+)$"

  # Check all lines match the pattern
  if (!all(grepl(pattern, lines))) {
    cat(grep(pattern, lines, value = TRUE, invert = TRUE), sep = "\n")
    stop("Matching failed")
  }

  # Extract the values from the pattern matches
  target <- sub(pattern, "\\1", lines)
  inc_dec <- sub(pattern, "\\2", lines)
  delta_size <- as.numeric(sub(pattern, "\\3", lines))
  cond_reg <- sub(pattern, "\\4", lines)
  cond_type <- sub(pattern, "\\5", lines)
  cond_val <- as.numeric(sub(pattern, "\\6", lines))

  tibble::tibble(
    target = target,
    delta = ifelse(inc_dec == "inc", delta_size, -delta_size),
    cond_reg = cond_reg,
    cond_type = cond_type,
    cond_val = cond_val
  )

}