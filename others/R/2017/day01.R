day01 <- function(s) {

  cs <- strsplit(s, "")[[1]] # input as a vector of strings
  xs <- purrr::map_int(cs, strtoi) # as a vector of ints
  ys <- dplyr::lead(xs, default = xs[[1]]) # shifted vector of ints

  purrr::reduce2(
    xs,
    ys,
    function(acc, x, y) ifelse(x == y, acc + x, acc),
    .init = 0
  )

}
