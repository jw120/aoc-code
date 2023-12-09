context("Day 10")

test_that("Given reverse examples", {
  expect_equal(day10_reverse(c(0, 1, 2, 3, 4), 1, 3), c(2, 1, 0, 3, 4))
  expect_equal(day10_reverse(c(2, 1, 0, 3, 4), 4, 4), c(4, 3, 0, 1, 2))
  expect_equal(day10_reverse(c(4, 3, 0, 1, 2), 4, 1), c(4, 3, 0, 1, 2))
  expect_equal(day10_reverse(c(4, 3, 0, 1, 2), 2, 5), c(3, 4, 2, 1, 0))
})

test_that("More reverse testing", {
  # Single length (do nothing)
  expect_equal(day10_reverse(c(2, 4, 6, 8), 1, 1), c(2, 4, 6, 8))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 2, 1), c(2, 4, 6, 8))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 3, 1), c(2, 4, 6, 8))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 4, 1), c(2, 4, 6, 8))
  # Length 2, non-wrapping
  expect_equal(day10_reverse(c(2, 4, 6, 8), 2, 2), c(2, 6, 4, 8))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 1, 2), c(4, 2, 6, 8))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 3, 2), c(2, 4, 8, 6))
  # Length 2, wrapping
  expect_equal(day10_reverse(c(2, 4, 6, 8), 4, 2), c(8, 4, 6, 2))
  # Length 3, non-wrapping
  expect_equal(day10_reverse(c(2, 4, 6, 8), 2, 3), c(2, 8, 6, 4))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 1, 3), c(6, 4, 2, 8))
  # Length 3, wrapping
  expect_equal(day10_reverse(c(2, 4, 6, 8), 3, 3), c(6, 4, 2, 8))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 4, 3), c(2, 8, 6, 4))
  # Length 4, non-wrapping
  expect_equal(day10_reverse(c(2, 4, 6, 8), 1, 4), c(8, 6, 4, 2))
  # Length 4, wrapping
  expect_equal(day10_reverse(c(2, 4, 6, 8), 2, 4), c(4, 2, 8, 6))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 3, 4), c(8, 6, 4, 2))
  expect_equal(day10_reverse(c(2, 4, 6, 8), 4, 4), c(4, 2, 8, 6))
})

test_that("Full examples", {
  expect_equal(day10_apply(0:4, c(3, 4, 1, 5)), c(3, 4, 2, 1, 0))
})
