context("Day 8")

in1 <- c(
  "b inc 5 if a > 1",
  "a inc 1 if b < 5",
  "c dec -10 if a >= 1",
  "c inc -20 if c == 10"
)
out1 <- tibble::tibble(
  target = c("b", "a", "c", "c"),
  delta = c(5, 1, 10, -20),
  cond_reg = c("a", "b", "a", "c"),
  cond_type = c(">", "<", ">=", "=="),
  cond_val = c(1, 5, 1, 10)
)

test_that("Parse example", {
  expect_identical(day08_parse(in1), out1)
})

test_that("Full example", {
  expect_equal(day08(in1), 1)
})

test_that("Cond examples", {
  expect_true(day08_cond(1, "<", 2))
  expect_false(day08_cond(1, "<", 1))
  expect_false(day08_cond(1, "<", 0))
  expect_false(day08_cond(1, ">", 2))
  expect_false(day08_cond(1, ">", 1))
  expect_true(day08_cond(1, ">", 0))
  expect_true(day08_cond(1, "<=", 2))
  expect_true(day08_cond(1, "<=", 1))
  expect_false(day08_cond(1, "<=", 0))
  expect_false(day08_cond(1, ">=", 2))
  expect_true(day08_cond(1, ">=", 1))
  expect_true(day08_cond(1, ">=", 0))
  expect_false(day08_cond(1, "==", 2))
  expect_true(day08_cond(1, "==", 1))
  expect_false(day08_cond(1, "==", 0))
  expect_true(day08_cond(1, "!=", 2))
  expect_false(day08_cond(1, "!=", 1))
  expect_true(day08_cond(1, "!=", 0))
})
