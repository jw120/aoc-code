context("Day 1")

test_that("Given examples", {
  expect_equal(day01("1122"), 3)
  expect_equal(day01("1111"), 4)
  expect_equal(day01("1234"), 0)
  expect_equal(day01("91212129"), 9)
})
