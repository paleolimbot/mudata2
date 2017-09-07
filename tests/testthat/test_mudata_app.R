
context("mudata shiny app")

test_that("mudata_app returns a shiny app", {
  expect_is(mudata_app(), "shiny.appobj")
})

test_that("mudata_app_options is cleared by mudata_app_clear_cache()", {
  mudata_app_options[["hashcode"]] <- "values"
  expect_identical(names(mudata_app_options), "hashcode")
  mudata_app_clear_cache()
  expect_identical(names(mudata_app_options), character(0))
})
