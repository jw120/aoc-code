context("Day 11")

test_that("Given  examples", {
  expect_equal(day11("ne,ne,ne"), 3)
  expect_equal(day11("ne,ne,sw,sw"), 0)
  expect_equal(day11("ne,ne,s,s"), 2)
  expect_equal(day11("se,sw,se,sw,sw"), 3)
})

test_that("Reversibility examples", {
  expect_equal(day11("ne,sw"), 0)
  expect_equal(day11("nw,se"), 0)
  expect_equal(day11("n,s"), 0)
})
