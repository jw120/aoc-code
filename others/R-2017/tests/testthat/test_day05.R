context("Day 5")

example_states <- list(
  list(1, c(0, 3, 0, 1, -3)),
  list(1, c(1, 3, 0, 1, -3)),
  list(2, c(2, 3, 0, 1, -3)),
  list(5, c(2, 4, 0, 1, -3)),
  list(2, c(2, 4, 0, 1, -2)),
  NULL
)

test_that("Iterate examples", {
  expect_equal(day05_iterate(example_states[[1]]), example_states[[2]])
  expect_equal(day05_iterate(example_states[[2]]), example_states[[3]])
  expect_equal(day05_iterate(example_states[[3]]), example_states[[4]])
  expect_equal(day05_iterate(example_states[[4]]), example_states[[5]])
  expect_equal(day05_iterate(example_states[[5]]), example_states[[6]])
})

test_that("Count example", {
  expect_equal(day05_count(c(0, 3, 0, 1, -3)), 5)
})
