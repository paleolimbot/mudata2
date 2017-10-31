
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
  expect_false(inherits(kg2, "mudata_sql"))
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
  expect_is(kg2, "mudata_sql")
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

test_that("summary and print methods are sql type safe", {
  # create remote dataset
  kg2 <- mudata(data = sources$data, locations = sources$locations,
                params = sources$params, datasets = sources$datasets,
                columns = sources$columns)
  
  # print method
  expect_identical(print(kg2), kg2)
  expect_output(print(kg2))
  
  # summary method
  expect_is(summary(kg2), "tbl_df")
  expect_equal(summary(kg2) %>% colnames(), 
               c("param", "location", "dataset", "mean_value", "sd_value", "n", "n_NA"))
  
})

test_that("distinct_* functions return the correct values", {
  kg2 <- mudata(data = sources$data, locations = sources$locations,
                params = sources$params, datasets = sources$datasets,
                columns = sources$columns)
  
  expect_equal(distinct_params(kg2),
               distinct_params(kentvillegreenwood))
  expect_equal(distinct_locations(kg2),
               distinct_locations(kentvillegreenwood))
  expect_equal(distinct_datasets(kg2), "ecclimate")
  expect_equal(distinct_columns(kg2, "data"),
               distinct_columns(kentvillegreenwood, "data"))
})

test_that("autoplot/plot works on sqlite sources", {
  kg2 <- mudata(data = sources$data, locations = sources$locations,
                params = sources$params, datasets = sources$datasets,
                columns = sources$columns)
  expect_is(ggplot2::autoplot(kg2), "ggplot")
  plot(kg2)
})

test_that("long_pairs works with sqlite sources", {
  kg2 <- mudata(data = sources$data, locations = sources$locations,
                params = sources$params, datasets = sources$datasets,
                columns = sources$columns)
  df_local <- kg2$data %>% dplyr::collect()
  pairs_sqlite <- long_pairs(kg2$data, id_vars = c("location", "date"), 
                             names_x = c("meantemp", "maxtemp"), names_y = c("mintemp", "meantemp"),
                             name_var = "param") %>%
    dplyr::mutate(date = as.numeric(date))
  pairs_local <- long_pairs(df_local, id_vars = c("location", "date"), 
                            names_x = c("meantemp", "maxtemp"), names_y = c("mintemp", "meantemp"),
                            name_var = "param") %>%
    dplyr::mutate(date = as.numeric(date))
  
  expect_identical(pairs_sqlite, pairs_local)
})

# clean temporary database
unlink(sql_file)
rm(kg_sql); gc() # disconnect sqlite database
