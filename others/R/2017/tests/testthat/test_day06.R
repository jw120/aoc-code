context("Day 6")

test_that("Iterate examples", {
  expect_equal(day06_iterate(c(0, 2, 7, 0)), c(2, 4, 1, 2))
  expect_equal(day06_iterate(c(2, 4, 1, 2)), c(3, 1, 2, 3))
  expect_equal(day06_iterate(c(3, 1, 2, 3)), c(0, 2, 3, 4))
  expect_equal(day06_iterate(c(0, 2, 3, 4)), c(1, 3, 4, 1))
  expect_equal(day06_iterate(c(1, 3, 4, 1)), c(2, 4, 1, 2))
})

test_that("Cycle example", {
  expect_equal(day06_cycle(c(0, 2, 7, 0)), 5)
})

test_that("Harness example", {
  expect_equal(day06("0 2 7 0"), 5)
})
