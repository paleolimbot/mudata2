
context("mudata helper functions")

test_that("distinct_* functions return the correct values", {
  expect_equal(distinct_params(kentvillegreenwood),
               sort(unique(kentvillegreenwood$data$param)))
  expect_equal(distinct_locations(kentvillegreenwood),
               sort(unique(kentvillegreenwood$data$location)))
  expect_equal(distinct_datasets(kentvillegreenwood), "ecclimate")
  expect_equal(distinct_columns(kentvillegreenwood, "data"),
               sort(c("dataset", "location", "param", "date", "value", "flags")))
})

test_that("dplyr tbl interface works with mudata objects", {
  expect_equal(src_tbls(kentvillegreenwood), names(kentvillegreenwood))
  expect_identical(dplyr::tbl(kentvillegreenwood, "datasets"), 
                   kentvillegreenwood %>% tbl_datasets())
})

test_that("distinct_* functions always return character vectors", {
  kg2 <- kentvillegreenwood
  kg2$data$param <- factor(kg2$data$param)
  kg2$params$param <- factor(kg2$params$param)
  kg2$data$location <- factor(kg2$data$location)
  kg2$locations$location <- factor(kg2$locations$location)
  kg2$data$dataset <- factor(kg2$data$dataset)
  kg2$locations$dataset <- factor(kg2$locations$dataset)
  kg2$params$dataset <- factor(kg2$params$dataset)
  kg2$columns$dataset <- factor(kg2$columns$dataset)
  
  expect_is(kg2 %>% distinct_params(), "character")
  expect_is(kg2 %>% distinct_datasets(), "character")
  expect_is(kg2 %>% distinct_locations(), "character")
})

test_that("accessors return the correct values", {
  expect_identical(tbl_data(kentvillegreenwood), kentvillegreenwood$data)
  expect_identical(tbl_params(kentvillegreenwood), kentvillegreenwood$params)
  expect_identical(tbl_locations(kentvillegreenwood), kentvillegreenwood$locations)
  expect_identical(tbl_datasets(kentvillegreenwood), kentvillegreenwood$datasets)
  expect_identical(tbl_columns(kentvillegreenwood), kentvillegreenwood$columns)
  expect_identical(x_columns(kentvillegreenwood), attr(kentvillegreenwood, "x_columns"))
})

test_that("tbl_data_wide works as expected", {
  expect_identical(
    tbl_data_wide(kentvillegreenwood),
    kentvillegreenwood %>%
      tbl_data() %>%
      dplyr::select(-flags) %>%
      tidyr::spread(key = param, value = value)
  )
})

test_that("update_datasets() function works as expected", {
  # updating a field
  updated_url <- kentvillegreenwood %>%
    update_datasets(url = "new_url") %>%
    tbl_datasets()
  expect_identical(colnames(updated_url), c("dataset", "url"))
  expect_identical(updated_url$url, "new_url")
  expect_equal(nrow(updated_url), 1)
  
  # adding a field
  updated_newkey <- kentvillegreenwood %>%
    update_datasets(newkey = "newval") %>%
    tbl_datasets()
  expect_identical(colnames(updated_newkey), c("dataset", "url", "newkey"))
  expect_identical(updated_newkey$newkey, "newval")
  expect_equal(nrow(updated_newkey), 1)
  
  # check that missing dataset is filled in
  expect_identical(
    update_datasets(kentvillegreenwood, "ecclimate", newkey = "newval"),
    update_datasets(kentvillegreenwood, newkey = "newval")
  )
  
  # check nonexistent dataset
  expect_error(
    update_datasets(kentvillegreenwood, "not_a_dataset", newkey = "newval"),
    "Zero rows were found for dataset"
  )
  
  # check bad values
  expect_error(
    update_datasets(kentvillegreenwood, newkey = c("newval1", "newval2")),
    "values to update must all be of length 1"
  )
})

test_that("update_locations() function works as expected", {
  # check column update
  updated_prov <- kentvillegreenwood %>%
    update_locations("GREENWOOD A", province = "Nova Scotia") %>%
    tbl_locations()
  expect_identical(colnames(kentvillegreenwood$locations), colnames(updated_prov))
  expect_identical(nrow(kentvillegreenwood$locations), nrow(updated_prov))
  expect_identical(
    updated_prov %>% dplyr::filter(location == "GREENWOOD A") %>% dplyr::pull(province),
    "Nova Scotia"
  )
  
  # check column create
  new_key <- kentvillegreenwood %>%
    update_locations("GREENWOOD A", newkey = "newval") %>%
    tbl_locations()
  expect_identical(kentvillegreenwood$locations, new_key %>% dplyr::select(-newkey))
  expect_identical(
    new_key %>% dplyr::filter(location == "GREENWOOD A") %>% dplyr::pull(newkey),
    "newval"
  )
  
  # check that missing location is filled in
  expect_identical(
    update_locations(kentvillegreenwood, newkey = "newval"),
    update_locations(kentvillegreenwood, locations = distinct_locations(kentvillegreenwood),
                    newkey = "newval")
  )
  
  # check that bad datasets, locations throw an error
  expect_error(
    update_locations(kentvillegreenwood, "not_a_location", newkey = "newval"),
    "Zero rows were found for locations"
  )
  expect_error(
    update_locations(kentvillegreenwood, datasets = "not_a_dataset", newkey = "newval"),
    "Zero rows were found for locations"
  )
  
})

test_that("update_params() function works as expected", {
  # check column update
  updated_label <- kentvillegreenwood %>%
    update_params("maxtemp", label = "newlabel") %>%
    tbl_params()
  expect_identical(kentvillegreenwood$params[-3], updated_label[-3])
  expect_identical(
    updated_label %>% dplyr::filter(param == "maxtemp") %>% dplyr::pull(label),
    "newlabel"
  )
  
  # check column create
  new_key <- kentvillegreenwood %>%
    update_params("maxtemp", newkey = "newval") %>%
    tbl_params()
  expect_identical(kentvillegreenwood$params, new_key %>% dplyr::select(-newkey))
  expect_identical(
    new_key %>% dplyr::filter(param == "maxtemp") %>% dplyr::pull(newkey),
    "newval"
  )
  
  # check that missing param is filled in
  expect_identical(
    update_params(kentvillegreenwood, newkey = "newval"),
    update_params(kentvillegreenwood, params = distinct_params(kentvillegreenwood),
                 newkey = "newval")
  )
  
  # check that bad datasets, locations throw an error
  expect_error(
    update_params(kentvillegreenwood, "not_a_param", newkey = "newval"),
    "Zero rows were found for params"
  )
  expect_error(
    update_params(kentvillegreenwood, datasets = "not_a_dataset", newkey = "newval"),
    "Zero rows were found for params"
  )
})

test_that("update_columns() works as expected", {
  # check updated value
  updated_type <- kentvillegreenwood %>%
    update_columns("flags", tables = "data", type = "newtype") %>%
    tbl_columns()
  expect_identical(kentvillegreenwood$columns[-4], updated_type[-4])
  expect_identical(
    updated_type %>% dplyr::filter(table == "data", column == "flags") %>% dplyr::pull(type),
    "newtype"
  )
  
  # check new value
  new_key <- kentvillegreenwood %>%
    update_columns("flags", tables = "data", unit = "newunit") %>%
    tbl_columns()
  expect_identical(kentvillegreenwood$columns, new_key[-5])
  expect_identical(
    new_key %>% dplyr::filter(table == "data", column == "flags") %>% dplyr::pull(unit),
    "newunit"
  )
  
  # check default filled in table, columns
  expect_identical(
    update_columns(kentvillegreenwood, newkey = "newval"),
    update_columns(kentvillegreenwood,
                   tables = names(kentvillegreenwood),
                   columns = distinct_columns(kentvillegreenwood),
                   newkey = "newval")
  )
  
  # check that bad values throw an error
  expect_error(
    update_columns(kentvillegreenwood, tables = "not_a_table", newkey = "newval"),
    "Zero rows were found for columns"
  )
  expect_error(
    update_columns(kentvillegreenwood, columns = "not_a_column", newkey = "newval"),
    "Zero rows were found for columns"
  )
  expect_error(
    update_columns(kentvillegreenwood, datasets = "not_a_dataset", newkey = "newval"),
    "Zero rows were found for columns"
  )
})

test_that("mutate shortcuts work", {
  expect_identical(
    second_lake_temp %>% mutate_data(datetime = lubridate::with_tz(datetime, "America/Halifax")),
    {
      sl2 <- second_lake_temp
      sl2$data <- sl2$data %>% dplyr::mutate(datetime = lubridate::with_tz(datetime, "America/Halifax"))
      sl2
    }
  )
  
  expect_identical(
    second_lake_temp %>% mutate_params(param_upper = toupper(param)),
    {
      sl2 <- second_lake_temp
      sl2$params <- sl2$params %>% dplyr::mutate(param_upper = toupper(param))
      sl2
    }
  )
  
  expect_identical(
    second_lake_temp %>% mutate_locations(loc_upper = toupper(location)),
    {
      sl2 <- second_lake_temp
      sl2$locations <- sl2$locations %>% dplyr::mutate(loc_upper = toupper(location))
      sl2
    }
  )
  
  expect_identical(
    second_lake_temp %>% mutate_datasets(ds_upper = toupper(dataset)),
    {
      sl2 <- second_lake_temp
      sl2$datasets <- sl2$datasets %>% dplyr::mutate(ds_upper = toupper(dataset))
      sl2
    }
  )
  
  expect_identical(
    second_lake_temp %>% mutate_columns(ds_upper = toupper(dataset)),
    {
      sl2 <- second_lake_temp
      sl2$columns <- sl2$columns %>% dplyr::mutate(ds_upper = toupper(dataset))
      sl2
    }
  )
})
