context("Day 4")

test_that("Single examples", {
  expect_true(day04_dup_free(c("aa", "bb", "cc", "dd", "ee")))
  expect_false(day04_dup_free(c("aa", "bb", "cc", "dd", "aa")))
  expect_true(day04_dup_free(c("aa", "bb", "cc", "dd", "aaa")))
})

test_that("Lines example", {
  expect_equal(day04(c(
    "aa bb cc dd ee",
    "aa bb cc dd aa",
    "aa bb cc dd aaa"
  )), 2)
})
