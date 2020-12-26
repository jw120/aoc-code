context("Day 2")

in1 <- c(
  "1  2    3 4",
  " 3 4    5   ",
  ""
)
ex1 <- list(
  c(1, 2, 3, 4),
  c(3, 4, 5)
)

test_that("Parsing", {
  expect_equal(day02_parse(in1), ex1)
})

in2 <- list(
  c(5, 1, 9, 5),
  c(7, 5, 3),
  c(2, 4, 6, 8)
)
ex2 <- 18

test_that("Example numbers", {
  expect_equal(day02_checksum(in2), ex2)
})

in3 <- c(
  "5 1 9 5",
  "7 5 3",
  "2 4 6 8",
  ""
)
ex3 <- 18

test_that("Example lines", {
  expect_equal(day02(in3), ex3)
})

