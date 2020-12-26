# Returns the score of a group (a non-vector)
#
#   s is the whole string
#   i is the index to the next character
#   score is the score to date
#   groups is the number of groups open (i.e., number of }s expected)
#   state is one of "Normal", "Garbage", "Pling"
#
day09 <- function(s) {

    i <- 1
    score <- 0
    groups <- 0
    state <- "Normal"

    while (i <= nchar(s)) {

      next_char <- substr(s, i, i)

#      cat(next_char, "i = ", i, "score = ", score, "groups = ", groups, state, "\n")

      if (state == "Normal") {

        if (next_char == "{") {
          groups <- groups + 1
        } else if (next_char == "}") {
          score <- score + groups
          groups <- groups - 1
        } else if (next_char == "<") {
          state <- "Garbage"
        }

      } else if (state == "Garbage") {

        if (next_char == ">") {
          state <- "Normal"
        } else if (next_char == "!") {
          state <- "Pling"
        }

      } else if (state == "Pling") {
        state <- "Garbage"
      }

      i <- i + 1

    }

#    cat("  i = ", i, "score = ", score, "groups = ", groups, state, "\n")

    return(score)

}
