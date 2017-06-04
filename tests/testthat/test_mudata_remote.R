
context("mudata_remote objects")

# create sqlite database with kentville greenwood dataset
sql_file <- tempfile()[1]
kg_sql <- dplyr::src_sqlite(sql_file, create = TRUE)
sources <- sapply(c("data", "locations", "params", "datasets", "columns"),
                  function(table) {
                    dplyr::copy_to(kg_sql, kentvillegreenwood[[table]], table)
                  }, simplify = FALSE)

test_that("mudata_remote constructor works with local data frames", {
  # try copy of kg data
  kg2 <- mudata_remote(data = kentvillegreenwood$data, locations = kentvillegreenwood$locations,
                       params = kentvillegreenwood$params, datasets = kentvillegreenwood$datasets,
                       columns = kentvillegreenwood$columns)
  expect_is(kg2, "mudata")
  expect_is(kg2, "mudata_remote")
  expect_true(validate.mudata(kg2))
  
  # try with automatic metadata fixing
  kg2 <- mudata_remote(data = kentvillegreenwood$data)
  expect_is(kg2, "mudata")
  expect_is(kg2, "mudata_remote")
  expect_true(validate.mudata(kg2))
  
  # try with dataset.id filled in
  kg2 <- mudata_remote(data = dplyr::select(kentvillegreenwood$data, -dataset),
                       dataset.id = "the_default")
  expect_is(kg2, "mudata")
  expect_is(kg2, "mudata_remote")
  expect_true(validate.mudata(kg2))
  
  expect_equal(unique(kg2$data$dataset), "the_default")
  expect_equal(unique(kg2$locations$dataset), "the_default")
  expect_equal(unique(kg2$params$dataset), "the_default")
  expect_equal(unique(kg2$datasets$dataset), "the_default")
  expect_equal(unique(kg2$columns$dataset), "the_default")
})

test_that("mudata_remote constructor works with sqlite data frames", {

  # create remote dataset
  kg2 <- mudata_remote(data = sources$data, locations = sources$locations,
                       params = sources$params, datasets = sources$datasets,
                       columns = sources$columns)
  
  expect_is(kg2, "mudata")
  expect_is(kg2, "mudata_remote")
  expect_true(validate.mudata(kg2))
  expect_is(summary(kg2), "tbl")
  expect_output(print(kg2), "A mudata_remote object*")
  
  # try with automatic metadata fixing
  kg2 <- mudata_remote(data = sources$data)
  expect_is(kg2, "mudata")
  expect_is(kg2, "mudata_remote")
  expect_true(validate.mudata(kg2))
  expect_is(summary(kg2), "tbl")
  expect_output(print(kg2), "A mudata_remote object*")
  
  # try with dataset.id filled in
  kg2 <- mudata_remote(data = dplyr::select(sources$data, -dataset),
                       dataset.id = "the_default")
  expect_is(kg2, "mudata")
  expect_is(kg2, "mudata_remote")
  expect_true(validate.mudata(kg2))
  expect_is(summary(kg2), "tbl")
  expect_output(print(kg2), "A mudata_remote object*")
  
  # collect before analyzing datasets
  kg2 <- dplyr::collect(kg2)
  expect_is(kg2, "mudata")
  expect_false(inherits(kg2, "mudata_remote"))
  expect_is(summary(kg2), "data.frame")
  expect_output(print(kg2), "A mudata object*")
  
  expect_equal(unique(kg2$data$dataset), "the_default")
  expect_equal(unique(kg2$locations$dataset), "the_default")
  expect_equal(unique(kg2$params$dataset), "the_default")
  expect_equal(unique(kg2$datasets$dataset), "the_default")
  expect_equal(unique(kg2$columns$dataset), "the_default")
})

# clean temporary database
unlink(sql_file)
