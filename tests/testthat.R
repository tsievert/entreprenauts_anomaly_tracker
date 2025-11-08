# --------tests/testthat.R--------
# Runner for tests in a non-package Shiny app.
# Run from project root:  testthat::test_dir("tests/testthat", reporter = "summary")

library(testthat)
test_dir("tests/testthat", reporter = "summary")
