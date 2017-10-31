
context("mudata constructor")

pocmaj_data <- pocmajsum %>%
  dplyr::select(core, depth, Ca, Ti, V) %>%
  tidyr::gather(Ca, Ti, V, key = "param", value = "value") %>%
  dplyr::select(location = core, param, depth, value)

test_that("mudata constructor creates a mudata object", {
  md <- mudata(pocmaj_data)
  expect_that(md, is_a("mudata"))
})

test_that("default dataset/location actually changes the default dataset/location name", {
  md <- mudata(pocmaj_data, dataset_id = "otherdataset")
  expect_that(md$datasets$dataset, equals("otherdataset"))
  expect_that(length(unique(md$data$dataset)), equals(1))
  expect_that(unique(md$data$dataset), equals("otherdataset"))
  
  pocmaj_data <- pocmaj_data[pocmaj_data$location == "POC-2",]
  pocmaj_data$location <- NULL
  md <- mudata(pocmaj_data, location_id = "otherlocation")
  expect_that(md$locations$location, equals("otherlocation"))
  expect_that(length(unique(md$data$location)), equals(1))
  expect_that(unique(md$data$location), equals("otherlocation"))
})

test_that("x_columns are correctly assigned/identified", {
  # default guessing should throw a message
  expect_message(mudata(pocmaj_data), "Guessing x columns: depth")
  # x_columns is stored in a mudata attribute
  expect_equal(attr(mudata(pocmaj_data), "x_columns"), "depth")
  # when explicitly assiged, it should be silent
  expect_silent(mudata(pocmaj_data, x_columns = "depth"))
  # ...but should still assign the x_columns attribute
  expect_equal(attr(mudata(pocmaj_data, x_columns = "depth"), "x_columns"), "depth")
  
  # x_columns should be able to be character(0), for the case where there is no axis other than
  # dataset, location, and param
  pocmaj_nodepth <- pocmaj_data %>% 
    dplyr::filter(depth == 0) %>%
    dplyr::select(-depth)
  expect_identical(x_columns(mudata(pocmaj_nodepth)), character(0))
  
  # when zero x_columns are passed (or guessed), previouslly there should have been an error
  # pocmajinv <- pocmaj_data %>% dplyr::select(-depth)
  # expect_error(mudata(pocmajinv), "Could not guess x columns from names: location, param, value")
  # expect_error(mudata(pocmaj_data, x_columns = character(0)),
  #              "x_columns must be a character vector of length > 0")
  # expect_error(mudata(pocmaj_data, x_columns = "not_in_pocmaj_data"),
  #              "Table 'data' is missing columns 'not_in_pocmaj_data'")
})

test_that("passing invalid inputs throws an error", {
  # invalid types
  expect_that(mudata(data=NULL), throws_error("Table 'data' is not a data\\.frame"))
  expect_that(mudata(pocmaj_data, locations=list()), 
              throws_error("Table 'locations' is not a data\\.frame"))
  expect_that(mudata(pocmaj_data, params=list()), 
              throws_error("Table 'params' is not a data\\.frame"))
  expect_that(mudata(pocmaj_data, columns=list()), 
              throws_error("Table 'columns' is not a data\\.frame"))
  expect_that(mudata(pocmaj_data, datasets=list()), 
              throws_error("Table 'datasets' is not a data\\.frame"))
  
  # zero-row data objects should be ok if all columns are present
  expect_silent(mudata(data.frame(dataset = character(0), location = character(0), 
                                  param = character(0),
                                  xcol = character(0), value = character(0)),
                       x_columns = "xcol"))
  md_empty <- mudata(data.frame(dataset = character(0), location = character(0), 
                                param = character(0),
                                xcol = character(0), value = character(0)))
  expect_true(all(vapply(md_empty, nrow, integer(1)) == 0))
  
  # other empty tables get caught in validation, but not in construction
  pocmaj_complete <- pocmaj_data %>% 
    dplyr::mutate(dataset = "a_dataset", location = "a_location")
  expect_silent(
    mudata(pocmaj_complete, 
           locations = data.frame(dataset = character(0), location = character(0)),
           validate = FALSE, x_columns = "depth")
  )
  expect_silent(
    mudata(pocmaj_complete, 
           params = data.frame(dataset = character(0), param = character(0)),
           validate = FALSE, x_columns = "depth")
  )
  expect_silent(
    mudata(pocmaj_complete, 
           datasets = data.frame(dataset = character(0)),
           validate = FALSE, x_columns = "depth")
  )
  expect_silent(
    mudata(pocmaj_complete, 
           columns = data.frame(dataset = character(0), table = character(0),
                                column = character(0)),
           validate = FALSE, x_columns = "depth")
  )
  
  # other zero-row data frames shouldn't work if dataset or location must be added
  expect_error(mudata(pocmaj_data, datasets = data.frame()),
               "Can't add a dataset to a table with zero rows!")
  expect_error(mudata(pocmaj_data, locations = data.frame()),
               "Can't add a dataset to a table with zero rows!")
  expect_error(mudata(pocmaj_data, params = data.frame(param=character(0))),
               "Can't add a dataset to a table with zero rows!")
  expect_error(mudata(pocmaj_data, columns = data.frame(table = character(0), column=character(0))),
               "Can't add a dataset to a table with zero rows!")
  
  
  # invalid columns (missing location and dataset columns aren't a problem because they
  # get filled in)
  expect_error(mudata(pocmaj_data %>% dplyr::select(-param, -value)),
               "Table 'data' is missing columns 'param', 'value'")
  expect_error(mudata(pocmaj_data, params=data.frame()), 
              "Table 'params' is missing columns 'param'")
  expect_error(mudata(pocmaj_data, columns=data.frame()), 
              "Table 'columns' is missing columns 'table', 'column'")
})

test_that("duplicate data is detected", {
  pocmaj_not_summarised <- pocmaj %>%
    tidyr::gather(Ca, Ti, V, key = "param", value = "value") %>%
    dplyr::select(location = core, param, depth, value)
  
  # skip aggregation
  md <- mudata(pocmaj_not_summarised, validate = FALSE)
  expect_that(validate_mudata(md), 
              throws_error("Duplicate data in data table"))
})

test_that("duplicate location metadata are detected", {
  data("kentvillegreenwood")
  expect_is(validate_mudata(kentvillegreenwood), "mudata")
  kentvillegreenwood$locations <- rbind(kentvillegreenwood$locations, kentvillegreenwood$locations[1,])
  expect_error(validate_mudata(kentvillegreenwood), "Duplicate locations in locations table")
})

test_that("duplicate param metadata are detected", {
  data("kentvillegreenwood")
  expect_is(validate_mudata(kentvillegreenwood), "mudata")
  kentvillegreenwood$params <- rbind(kentvillegreenwood$params, kentvillegreenwood$params[1,])
  expect_error(validate_mudata(kentvillegreenwood), "Duplicate params in params table")
})

test_that("duplicate dataset metadata are detected", {
  data("kentvillegreenwood")
  expect_is(validate_mudata(kentvillegreenwood), "mudata")
  kentvillegreenwood$datasets <- rbind(kentvillegreenwood$datasets, kentvillegreenwood$datasets[1,])
  expect_error(validate_mudata(kentvillegreenwood), "Duplicate datasets in datasets table")
})


test_that("duplicate column metadata are detected", {
  data("kentvillegreenwood")
  expect_is(validate_mudata(kentvillegreenwood), "mudata")
  kentvillegreenwood$columns <- rbind(kentvillegreenwood$columns, kentvillegreenwood$columns[1,])
  expect_error(validate_mudata(kentvillegreenwood), "Duplicate columns in columns table")
})

test_that("printing of a mudata actually prints things", {
  md <- mudata(pocmaj_data)
  expect_that(print(md), is_a("mudata"))
  expect_output(print(md))
})

test_that("mudata summaries are tibbles", {
  md <- mudata(pocmaj_data)
  expect_is(summary(md), "tbl_df")
  expect_equal(summary(md) %>% colnames(), 
               c("param", "location", "dataset", "mean_value", "sd_value", "n", "n_NA"))
  
  # check with value as non-numeric
  md$data$value <- as.character(md$data$value)
  expect_is(summary(md), "tbl_df")
  expect_equal(summary(md) %>% colnames(), 
               c("param", "location", "dataset", "n"))
  
})

test_that("grouped data frames don't cause problems in the mudata constructor", {
  md <- mudata(pocmaj_data)
  expect_silent(mudata(dplyr::group_by(pocmaj_data, location, param), 
                       validate = FALSE, x_columns = "depth"))
  md2 <- mudata(dplyr::group_by(pocmaj_data, location, param), validate = FALSE)
  
  
  md2 <- new_mudata(lapply(md2, dplyr::ungroup), x_columns = attr(md2, "x_columns"))
  expect_silent(validate_mudata(md2))
  expect_is(validate_mudata(md2), "mudata")
  
  expect_true(all(mapply(function(x, y) identical(as.data.frame(x), as.data.frame(y)), md, md2)))
})

test_that("grouped data frames don't cause problems in the validate method", {
  md <- mudata(pocmaj_data)
  md$data <- dplyr::group_by(md$data, location, param)
  expect_silent(validate_mudata(md))
  expect_is(validate_mudata(md), "mudata")
})

test_that("coersion methods work as expected", {
  # mudata
  expect_identical(kentvillegreenwood, as_mudata(kentvillegreenwood))
  # tbl
  expect_identical(as_mudata(kentvillegreenwood$data),
                   mudata(kentvillegreenwood$data))
  # data.frame
  expect_identical(as_mudata(as.data.frame(kentvillegreenwood$data)),
                   mudata(kentvillegreenwood$data))
  # list
  expect_identical(as_mudata(unclass(kentvillegreenwood)),
                   kentvillegreenwood)
  # make sure x_columns are passed on if present
  expect_silent(as_mudata(unclass(kentvillegreenwood)))
  kg2 <- kentvillegreenwood
  attr(kg2, "x_columns") <- NULL
  expect_message(as_mudata(unclass(kg2)))
  
  # check as.mudata
  expect_identical(as_mudata(kentvillegreenwood),
                   as.mudata(kentvillegreenwood))
})

test_that("is_mudata works as expected", {
  expect_true(is_mudata(kentvillegreenwood))
  expect_true(is.mudata(kentvillegreenwood))
  expect_false(is_mudata(NULL))
  expect_false(is.mudata(NULL))
})

test_that("more_tbls argument works as expected in the mudata constructor", {
  # create a possible fictional table that might want to be included in a mudata object
  flags_dict <- kentvillegreenwood %>% 
    tbl_data() %>% 
    dplyr::filter(!is.na(flags)) %>% 
    dplyr::distinct(param, flags) %>%
    dplyr::mutate(data_number = 1:4, data_date = as.Date("1970-01-01") + 1:4, 
                  data_chr = c("one", "two", "three", "four"))
  
  # expect that flags_dict makes it into the mudata and columns table
  kg2 <- mudata(kentvillegreenwood$data, flags_dict = flags_dict)
  expect_true("flags_dict" %in% names(kg2))
  expect_true("flags_dict" %in% kg2$columns$table)
  expect_identical(kg2$flags_dict, flags_dict)
  
  # expect that ... and more_tbls do the same thing
  expect_identical(
    mudata(kentvillegreenwood$data, flags_dict = flags_dict),
    mudata(kentvillegreenwood$data, more_tbls = list(flags_dict = flags_dict))
  )
  
  # check that invalid values to more_tbls generate errors
  # no names
  expect_error(
    mudata(kentvillegreenwood$data, more_tbls = list(flags_dict)),
    "more_tbls must only contain named tbls"
  )
  # some names
  expect_error(
    mudata(kentvillegreenwood$data, more_tbls = list(flags_dict = flags_dict, flags_dict)),
    "more_tbls must only contain named tbls"
  )
  # not tbls
  expect_error(
    mudata(kentvillegreenwood$data, more_tbls = list(flags_dict = list('not a tibble'))),
    "more_tbls must only contain tbls"
  )
})
