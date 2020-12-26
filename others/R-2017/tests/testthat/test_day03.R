context("Day 3")

test_that("Examples", {
  expect_equal(day03(1), 0)
  expect_equal(day03(12), 3)
  expect_equal(day03(23), 2)
  expect_equal(day03(1024), 31)
})
