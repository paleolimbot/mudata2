
context("utility functions")

test_that("numeric variables are correctly identified", {
  df <- data.frame(a=factor("a factor"), b="not a factor", c=4,
                   d=4.5, e=Sys.Date(), f=Sys.time())
  expect_that(names(df)[sapply(df, is.numericish)], equals(c("c", "d", "e", "f")))
  expect_that(!sapply(df, is.numericish), equals(sapply(df, ggplot2:::is.discrete)))
})
