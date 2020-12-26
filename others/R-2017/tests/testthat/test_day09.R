context("Day 9")

test_that("Given examples", {
  expect_identical(day09("{}"), 1)
  expect_identical(day09("{{{}}}"), 6)
  expect_identical(day09("{{},{}}"), 5)
  expect_identical(day09("{{{},{},{{}}}}"), 16)
  expect_identical(day09("{<a>,<a>,<a>,<a>}"), 1)
  expect_identical(day09("{{<ab>},{<ab>},{<ab>},{<ab>}}"), 9)
  expect_identical(day09("{{<!!>},{<!!>},{<!!>},{<!!>}}"), 9)
  expect_identical(day09("{{<a!>},{<a!>},{<a!>},{<ab>}}"), 3)
})
