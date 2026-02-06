## ---------- tests/testthat/test-colors.R ----------
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(app_root, "global.R"), local = TRUE)

.hex_regex <- "^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$"

test_that("get_viridis_colors returns correct length and valid hex codes", {
  pal <- get_viridis_colors("magma", 5L)
  expect_equal(length(pal), 5L)
  expect_true(all(grepl(.hex_regex, pal)))
})

test_that("default_color_state uses a known palette and valid hex colors", {
  st <- default_color_state()
  expect_true(st$palette %in% c("magma", "inferno", "plasma", "viridis", "cividis"))

  cols <- c(
    st$possible,
    st$tested,
    st$hit,
    st$miss,
    st$albs,
    st$impossible,
    st$gridLines
  )
  expect_true(all(grepl(.hex_regex, cols)))
})
