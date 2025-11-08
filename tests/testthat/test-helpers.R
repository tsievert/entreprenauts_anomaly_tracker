# -------- tests/testthat/test-helpers.R --------
library(testthat)

test_that("edge_adjacent detects edge-touch only", {
  # sharing an edge vertically
  a_minR <- 1L
  a_maxR <- 2L
  a_minC <- 1L
  a_maxC <- 2L
  b_minR <- 3L
  b_maxR <- 4L
  b_minC <- 1L
  b_maxC <- 2L
  expect_true(edge_adjacent(
    a_minR, a_maxR, a_minC, a_maxC,
    b_minR, b_maxR, b_minC, b_maxC
  ))

  # sharing an edge horizontally
  a_minR <- 1L
  a_maxR <- 2L
  a_minC <- 1L
  a_maxC <- 2L
  b_minR <- 1L
  b_maxR <- 2L
  b_minC <- 3L
  b_maxC <- 4L
  expect_true(edge_adjacent(
    a_minR, a_maxR, a_minC, a_maxC,
    b_minR, b_maxR, b_minC, b_maxC
  ))

  # touching only at a corner -> not adjacent
  a_minR <- 1L
  a_maxR <- 2L
  a_minC <- 1L
  a_maxC <- 2L
  b_minR <- 3L
  b_maxR <- 4L
  b_minC <- 3L
  b_maxC <- 4L
  expect_false(edge_adjacent(
    a_minR, a_maxR, a_minC, a_maxC,
    b_minR, b_maxR, b_minC, b_maxC
  ))
})
