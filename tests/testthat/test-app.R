# -------- tests/testthat/test-app.R --------
library(testthat)

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

test_that("app.R constructs a shiny app object", {
  app_path <- file.path(app_root, "app.R")
  app <- shiny::shinyAppFile(app_path)
  expect_s3_class(app, "shiny.appobj")
})
