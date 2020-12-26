# #
#
# -3  -2  -1   0   1   2   3
#
# 37  36  35  34  33  32  31    3
# 38  17  16  15  14  13  30    2
# 39  18   5   4   3  12  29    1
# 40  19   6   1   2  11  28    0
# 41  20   7   8   9  10  27   -1
# 42  21  22  23  24  25  26   -2
# 43  44  45  46  47  48  49   -3
#
# 1-9 are irregular
# 10-25 is a sequence of 16 = 4 * 4
# 26-49 is a sequence of 24 = 4 * 6
# 50-81 is a sequence of 32 = 4 * 8
#
# Ring (r)  Numbers   Size          Bottom right corner (x,y)
# 0         1..1      1             (0, 0)
# 1         2..9      8  = 4 * 2    (1, -1)
# 2         10..25    16 = 4 * 4    (2, -2)
# 3         26..49    24 = 4 * 6
# ...
# r                   8 * r         (r, -r)


# return the x, y coordinates of number x
day03_coord <- function(x) {

  # Check that x is a positive non-vector integer
  stopifnot(is.numeric(x) && length(x) == 1 && x == round(x))
  stopifnot(x > 0)

  # Handle special case of 1 separately
  if (x == 1) {
    return(c(0, 0))
  }

  ring <- 1
  ring_min <- 2
  ring_max <- ring_min + 8 * ring - 1
  while (x > ring_max) {
    ring <- ring + 1
    ring_min <- ring_max + 1
    ring_max <- ring_min + 8 * ring - 1
  }
#  cat("ring", ring, "-", ring_min, "...", ring_max, "\n")

  # Handle first upward section of ring
  up <- x - ring_min # In range 0... 2 * ring - 1
  if (up <= 2 * ring - 1) {
    return(c(ring, -ring + 1 + up))
  }
  # leftward section
  left <- x - (ring_min + 2 * ring - 1) # In range 1..2 * ring
  if (left <= 2 * ring) {
    return(c(ring - left, ring))
  }
  # downward section
  down <- x - (ring_min + 4 * ring - 1) # In range 1..2 * ring
  if (down <= 2 * ring) {
    return(c(-ring, ring - down))
  }
  # right section
  right <- x - (ring_min + 6 * ring - 1) # In range 1.. 2 * ring
  return(c(-ring + right, -ring))

}

# Distance to origin is just the sum of the absolute values of the coordinates
day03 <- function(x) {
  sum(abs(day03_coord(x)))
}
