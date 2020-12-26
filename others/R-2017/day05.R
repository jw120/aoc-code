# Perform our iteration
#
# state of system represented by a list of two elements
#   a numeric index (the program counter, 1-indexed)
#   a numeric vector (the program)
# or NULL if the program has finished
#
day05_iterate <- function(state) {

  # Extract components of state
  counter <- state[[1]]
  program <- state[[2]]

  # Update to new state
  new_counter <- counter + program[[counter]]
  program[[counter]] <- program[[counter]] + 1
  if (new_counter < 1 || new_counter > length(program)) {
    return(NULL)
  }
  return(list(new_counter, program))
}

# Return the number of iterations required to reach NULL for given program
day05_count <- function(program) {
  count <- 0
  state <- list(1, program)
  while (!is.null(state)) {
    count <- count + 1
    state <- day05_iterate(state)
  }
  count
}

# Takes a program as a character vector and returns number of iterations needed
day05 <- function(lines) {
  day05_count(as.numeric(lines))
}

