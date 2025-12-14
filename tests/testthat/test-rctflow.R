library(testthat)
test_that("rctflow returns a DiagrammeR widget", {
  df <- data.frame(
    Subject_ID = sprintf("P%03d", 1:6),
    Group = c("tDCS", "tDCS", "tDCS", "Sham", "Sham", "Sham"),
    stringsAsFactors = FALSE
  )

  g <- rctflow(df, group_col = "Group")

  expect_s3_class(g, "htmlwidget")
  expect_true(inherits(g, "grViz"))
})
