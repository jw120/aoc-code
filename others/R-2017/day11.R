# Day 11
#
# We use the cube coorindate system from https://www.redblobgames.com/grids/hexagons/
#
# Each hex has 3 coordinates x, y, z where x + y + z = 0
#
# Moving one step changes two coordinates
# n  ( 0,  +1, -1)
# ne (+1,  0, -1)
# se (+1, -1,  0)
# s  ( 0,  -1, +1)
# sw (-1,  0, +1)
# nw (-1, +1,  0)
#
# Manhattan distance on the hex matrix between a and b is given by
# (abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)) / 2

day11 <- function(s) {
  moves <- strsplit(s, ",")[[1]]
  final <- Reduce(day11_move, moves, c(0, 0, 0))
  sum(abs(final)) / 2
}

# Move position (as a 3-vector) in given direction
day11_move <- function(pos, move) {
  switch(move,
    n  = pos + c( 0, +1, -1),
    ne = pos + c(+1,  0, -1),
    se = pos + c(+1, -1,  0),
    s  = pos + c( 0,  -1, +1),
    sw = pos + c(-1,  0, +1),
    nw = pos + c(-1, +1,  0),
    stop("Unknown move ", move)
  )
}