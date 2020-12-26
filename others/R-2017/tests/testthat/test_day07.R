context("Day 7")

test_that("Parse examples", {
  expect_identical(
    day07_parse(c("jikpfbk (53)", "usztpox (160) -> goyzt, eagggd")),
    tibble::tibble(
      prog = c("jikpfbk", "usztpox"),
      above = list(character(0), c("goyzt", "eagggd"))
    )
  )
})

ex <- c(
  "pbga (66)",
  "xhth (57)",
  "ebii (61)",
  "havc (66)",
  "ktlj (57)",
  "fwft (72) -> ktlj, cntj, xhth",
  "qoyq (66)",
  "padx (45) -> pbga, havc, qoyq",
  "tknk (41) -> ugml, padx, fwft",
  "jptl (61)",
  "ugml (68) -> gyxo, ebii, jptl",
  "gyxo (61)",
  "cntj (57)"
)

test_that("Full example", {
  expect_equal(day07(ex), "tknk")
})
