
context("mudata helper functions")

test_that("distinct_* functions return the correct values", {
  expect_equal(distinct_params(kentvillegreenwood),
               unique(kentvillegreenwood$data$param))
  expect_equal(distinct_locations(kentvillegreenwood),
               unique(kentvillegreenwood$data$location))
  expect_equal(distinct_datasets(kentvillegreenwood), "ecclimate")
  expect_equal(distinct_columns(kentvillegreenwood, "data"),
               c("dataset", "location", "param", "date", "value", "flags"))
})

test_that("accessors return the correct values", {
  expect_identical(data_tbl(kentvillegreenwood), kentvillegreenwood$data)
  expect_identical(params_tbl(kentvillegreenwood), kentvillegreenwood$params)
  expect_identical(locations_tbl(kentvillegreenwood), kentvillegreenwood$locations)
  expect_identical(datasets_tbl(kentvillegreenwood), kentvillegreenwood$datasets)
  expect_identical(columns_tbl(kentvillegreenwood), kentvillegreenwood$columns)
  expect_identical(x_columns(kentvillegreenwood), attr(kentvillegreenwood, "x_columns"))
})


