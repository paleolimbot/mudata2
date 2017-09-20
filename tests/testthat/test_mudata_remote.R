
context("mudata objects with sqlite")

# create sqlite database with kentville greenwood dataset
sql_file <- tempfile()[1]
kg_sql <- dplyr::src_sqlite(sql_file, create = TRUE)
sources <- sapply(c("data", "locations", "params", "datasets", "columns"),
                  function(table) {
                    dplyr::copy_to(kg_sql, kentvillegreenwood[[table]], table)
                  }, simplify = FALSE)

test_that("mudata constructor works with local data frames", {
  # try copy of kg data
  kg2 <- mudata(data = kentvillegreenwood$data, locations = kentvillegreenwood$locations,
                       params = kentvillegreenwood$params, datasets = kentvillegreenwood$datasets,
                       columns = kentvillegreenwood$columns)
  expect_is(kg2, "mudata")
  expect_is(validate_mudata(kg2), "mudata")
  
  # try with automatic metadata fixing
  kg2 <- mudata(data = kentvillegreenwood$data)
  expect_is(kg2, "mudata")
  expect_is(validate_mudata(kg2), "mudata")
  
  # try with dataset_id filled in
  kg2 <- mudata(data = dplyr::select(kentvillegreenwood$data, -dataset),
                       dataset_id = "the_default")
  expect_is(kg2, "mudata")
  expect_is(validate_mudata(kg2), "mudata")
  
  expect_equal(unique(kg2$data$dataset), "the_default")
  expect_equal(unique(kg2$locations$dataset), "the_default")
  expect_equal(unique(kg2$params$dataset), "the_default")
  expect_equal(unique(kg2$datasets$dataset), "the_default")
  expect_equal(unique(kg2$columns$dataset), "the_default")
})

test_that("mudata constructor works with sqlite data frames", {

  # create remote dataset
  kg2 <- mudata(data = sources$data, locations = sources$locations,
                       params = sources$params, datasets = sources$datasets,
                       columns = sources$columns)
  
  expect_is(kg2, "mudata")
  expect_is(validate_mudata(kg2), "mudata")
  expect_output(print(kg2))
  
  # try with automatic metadata fixing
  kg2 <- mudata(data = sources$data)
  expect_is(kg2, "mudata")
  expect_is(validate_mudata(kg2), "mudata")
  expect_output(print(kg2))
  
  # try with dataset_id filled in
  kg2 <- mudata(data = dplyr::select(sources$data, -dataset),
                       dataset_id = "the_default")
  expect_is(kg2, "mudata")
  expect_is(validate_mudata(kg2), "mudata")
  expect_output(print(kg2))
  
  # collect before analyzing datasets
  kg2 <- dplyr::collect(kg2)
  expect_is(kg2, "mudata")
  expect_output(print(kg2))
  
  expect_equal(unique(kg2$data$dataset), "the_default")
  expect_equal(unique(kg2$locations$dataset), "the_default")
  expect_equal(unique(kg2$params$dataset), "the_default")
  expect_equal(unique(kg2$datasets$dataset), "the_default")
  expect_equal(unique(kg2$columns$dataset), "the_default")
})

test_that("mudata_sql works as expected", {
  # using default arguments
  mdb <- mudata_sql(kg_sql)
  kg2 <- mudata(data = sources$data, locations = sources$locations,
                params = sources$params, datasets = sources$datasets,
                columns = sources$columns)
  expect_identical(dplyr::collect(mdb), dplyr::collect(kg2))
  
  # using only data table
  kdb_def <- mudata_sql(kg_sql, locations = NULL, params = NULL, datasets = NULL,
                       columns = NULL)
  kg2_def <- mudata(data = sources$data)
  expect_identical(dplyr::collect(kdb_def), dplyr::collect(kg2_def))
})

test_that("distinct_* functions return the correct values", {
  kg2 <- mudata(data = sources$data, locations = sources$locations,
                params = sources$params, datasets = sources$datasets,
                columns = sources$columns)
  
  expect_equal(distinct_params(kg2),
               unique(kentvillegreenwood$data$param))
  expect_equal(distinct_locations(kg2),
               unique(kentvillegreenwood$data$location))
  expect_equal(distinct_datasets(kg2), "ecclimate")
  expect_equal(distinct_columns(kg2, "data"),
               c("dataset", "location", "param", "date", "value", "flags"))
})

# clean temporary database
unlink(sql_file)
rm(kg_sql); gc() # disconnect sqlite database
