# Top-level function working on a string
day06 <- function(s) {
  day06_cycle(as.numeric(strsplit(s, "\\s+")[[1]]))
}

# Repeat iteration until a state is repeated, return number of iterations needed
# This is a very poor algorithm (linear search of history) and poor R (adding to vector in while loop)
day06_cycle <- function(state) {

  history <- character(0) # We hold history in a char vector
  state_str <- paste(state, collapse = ",")
  count <- 0

  while (!(state_str %in% history)) {
    history <- append(history, state_str)
    state <- day06_iterate(state)
    count <- count + 1
    state_str <- paste(state, collapse = ",")
  }

  count
}

# Run one iteration of redistribution
day06_iterate <- function(state) {

  n <- length(state)

  # We redistribute from the (first) maximum
  redist_index <- which.max(state)
  redist_amount <- state[[redist_index]]

  redist_all <- redist_amount %/% n # All states get this amount
  redist_rest <- redist_amount - redist_all * n # states 1..redist_rest to the right get one more

  state[[redist_index]] <- 0
  for (i in 1:n) {
    index <- ((redist_index + i - 1) %% n) + 1
    state[[index]] <-
      state[[index]] +
      redist_all +
      ifelse(i <= redist_rest, 1, 0)
  }

  state

}