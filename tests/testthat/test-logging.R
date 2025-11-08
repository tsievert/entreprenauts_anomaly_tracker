# -------- tests/testthat/test-logging.R --------
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(app_root, "global.R"), local = TRUE)

test_that(".empty_log_df has expected schema", {
  df <- .empty_log_df()
  expect_s3_class(df, "tbl_df")
  expect_identical(
    names(df),
    c(
      "time", "grid", "action", "long", "lat",
      "radiusName", "radiusVal", "outcome", "direction",
      "stage", "ratio", "remaining", "cellsChecked",
      "distanceFromLast"
    )
  )
  expect_type(df$time, "character")
  expect_type(df$grid, "character")
  expect_type(df$action, "character")
  expect_type(df$long, "double")
  expect_type(df$lat, "double")
})

test_that(".make_log_row creates a single well-typed row", {
  row <- .make_log_row(
    grid = "gridA",
    action = "Drop",
    long = 10,
    lat = 5,
    radiusName = "TestProbe",
    radiusVal = 2,
    outcome = "Miss",
    direction = "",
    stage = "FULL",
    ratio = 1,
    remaining = 42,
    cellsChecked = 9,
    distanceFromLast = 7
  )
  expect_s3_class(row, "tbl_df")
  expect_equal(nrow(row), 1L)
  expect_identical(row$grid[[1]], "gridA")
  expect_identical(row$action[[1]], "Drop")
  expect_equal(row$long[[1]], 10)
  expect_equal(row$lat[[1]], 5)
  expect_identical(row$radiusName[[1]], "TestProbe")
  expect_equal(row$radiusVal[[1]], 2)
  expect_identical(row$outcome[[1]], "Miss")
  expect_identical(row$stage[[1]], "FULL")
})

test_that(".append_log_row appends rows and preserves schema", {
  lg0 <- .empty_log_df()
  row1 <- .make_log_row(
    grid       = "gridA",
    action     = "Drop",
    long       = 10,
    lat        = 5,
    radiusName = "TestProbe",
    radiusVal  = 2,
    outcome    = "Miss",
    stage      = "FULL"
  )
  lg1 <- .append_log_row(lg0, row1)
  expect_equal(nrow(lg1), 1L)
  expect_identical(lg1$grid[[1]], "gridA")
  expect_identical(lg1$action[[1]], "Drop")

  row2 <- .make_log_row(
    grid       = "gridA",
    action     = "ALBS",
    long       = 20,
    lat        = 10,
    radiusName = "ALBS",
    radiusVal  = 5,
    outcome    = "Mask applied",
    stage      = "ALBS"
  )
  lg2 <- .append_log_row(lg1, row2)
  expect_equal(nrow(lg2), 2L)
  expect_identical(lg2$action[[2]], "ALBS")

  # upgrading a partially wrong log should still work
  bad <- as.data.frame(lg2)
  bad$extra <- 1:2
  upgraded <- upgrade_log(bad)
  expect_false("extra" %in% names(upgraded))
  expect_equal(nrow(upgraded), 2L)
})
