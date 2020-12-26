day10 <- function(s) {
  lens <- as.numeric(strsplit(s, ",")[[1]])
  result <- day10_apply(0:255, lens)
  result[[1]] * result[[2]]
}

# Apply the lengths in lens to the list xs
day10_apply <- function(xs, lens) {

  current_position <- 1
  skip_size <- 0

  for (len in lens) {
    xs <- day10_reverse(xs, current_position, len)
    current_position <- current_position + len + skip_size
    current_position <- ((current_position - 1) %% length(xs)) + 1
    skip_size <- skip_size + 1
  }
  xs
}

# Reverse the order of the len elements of the vector xs starting at position start
# (wrapping around as needed)
day10_reverse <- function(xs, start, len) {

  stopifnot(len <= length(xs))

  # Handle zero len case
  if (len == 0) {
    return(xs)
  }

  # Handle case where target does not wrap around
  end <- start + len - 1 # index of last element to be reversed
  if (end <= length(xs)) {
    before <- if (start > 1) xs[1:(start - 1)] else NULL
    target <- xs[start:end]
    after <- if (end < length(xs)) xs[(end + 1):length(xs)] else NULL
    return(c(before, rev(target), after))
  }

  # Wrapping case
  front_len <- end - length(xs)
  front <- xs[1:front_len]
  mid_len <- length(xs) - len
  mid <- if (mid_len > 0) xs[(front_len + 1):(start - 1)] else NULL
  back_len <- len - front_len
  back <- xs[start:length(xs)]
  new_combined <- rev(c(back, front))
  new_back <- new_combined[1:back_len]
  new_front <- new_combined[(back_len + 1):length(new_combined)]
  c(new_front, mid, new_back)

}
