
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
  expect_identical(tbl_data(kentvillegreenwood), kentvillegreenwood$data)
  expect_identical(tbl_params(kentvillegreenwood), kentvillegreenwood$params)
  expect_identical(tbl_locations(kentvillegreenwood), kentvillegreenwood$locations)
  expect_identical(tbl_datasets(kentvillegreenwood), kentvillegreenwood$datasets)
  expect_identical(tbl_columns(kentvillegreenwood), kentvillegreenwood$columns)
  expect_identical(x_columns(kentvillegreenwood), attr(kentvillegreenwood, "x_columns"))
})

test_that("unique_* functions return the correct values", {
  expect_equal(unique_params(kentvillegreenwood),
               unique(kentvillegreenwood$data$param))
  expect_equal(unique_locations(kentvillegreenwood),
               unique(kentvillegreenwood$data$location))
  expect_equal(unique_datasets(kentvillegreenwood), "ecclimate")
  expect_equal(unique_columns(kentvillegreenwood, "data"),
               c("dataset", "location", "param", "date", "value", "flags"))
})
