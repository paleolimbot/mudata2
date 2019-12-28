
context("renaming functions")

# ---- forward-facing rename functions ----

test_that("rename functions work as expected", {
  expect_identical(rename_datasets(kentvillegreenwood), kentvillegreenwood)
  expect_identical(
    rename_datasets(kentvillegreenwood, avalley = ecclimate) %>% distinct_datasets(),
    "avalley"
  )
  
  expect_identical(rename_locations(kentvillegreenwood), kentvillegreenwood)
  expect_identical(
    rename_locations(kentvillegreenwood, Greenwood = starts_with("GREENWOOD")) %>% distinct_locations(),
    c("Greenwood", "KENTVILLE CDA CS")
  )
  
  expect_identical(rename_params(kentvillegreenwood), kentvillegreenwood)
  expect_true(
    all(c("max_temp", "min_temp") %in% 
          (rename_params(kentvillegreenwood, max_temp = maxtemp, min_temp = mintemp) %>% 
          distinct_params()))
  )
  expect_false(
    any(c("maxtemp", "mintemp") %in% 
          (rename_params(kentvillegreenwood, max_temp = maxtemp, min_temp = mintemp) %>% 
             distinct_params()))
  )
  
  expect_identical(rename_columns(kentvillegreenwood), kentvillegreenwood)
  col_rename <- rename_columns(kentvillegreenwood, lon = longitude, lat = latitude) %>% 
    tbl_locations()
  expect_true(all(c("lat", "lon") %in% colnames(col_rename)))
  expect_false(any(c("latitude", "longitude") %in% colnames(col_rename)))
  
  # make sure x_columns are renamed as well
  x_rename <- rename_columns(kentvillegreenwood, Date = date)
  expect_true("Date" %in% colnames(x_rename %>% tbl_data()))
  expect_false("date" %in% colnames(x_rename %>% tbl_data()))
  expect_identical(x_columns(x_rename), "Date")
})

test_that("rename functions throw errors", {
  
  # test renaming of required columns
  expect_error(rename_columns(kentvillegreenwood, bad = dataset), "Cannot rename required mudata columns")
  expect_error(rename_columns(kentvillegreenwood, bad = location), "Cannot rename required mudata columns")
  expect_error(rename_columns(kentvillegreenwood, bad = param), "Cannot rename required mudata columns")
  expect_error(rename_columns(kentvillegreenwood, bad = value), "Cannot rename required mudata columns")
  expect_error(rename_columns(kentvillegreenwood, bad = table), "Cannot rename required mudata columns")
  expect_error(rename_columns(kentvillegreenwood, bad = column), "Cannot rename required mudata columns")
  
  # test renaming of things that don't exist
  expect_error(rename_datasets(kentvillegreenwood, bad = not_a_dataset), "object 'not_a_dataset' not found")
  expect_error(rename_locations(kentvillegreenwood, bad = not_a_location), "object 'not_a_location' not found")
  expect_error(rename_params(kentvillegreenwood, bad = not_a_param), "object 'not_a_param' not found")
  expect_error(rename_columns(kentvillegreenwood, bad = not_a_col), "object 'not_a_col' not found")
  
  # test renaming of things to things that already exist
  kg2 <- kentvillegreenwood %>%
    rename_datasets(ec2 = ecclimate) %>%
    rbind(kentvillegreenwood)
  
  expect_message(rename_datasets(kg2, ecclimate = ec2), "Possible duplicated values in x")
  expect_message(rename_locations(kg2, `GREENWOOD A` = `KENTVILLE CDA CS`), 
                 "Possible duplicated values in x")
  expect_message(rename_params(kg2, maxtemp = mintemp), "Possible duplicated values in x")
  expect_message(rename_columns(kg2, date = flags), "Possible duplicated values in x")
})

test_that("rename works when some values are factors", {
  expect_identical(
    rename_datasets(kentvillegreenwood %>% 
                      select_datasets(everything(), .factor = TRUE), avalley = ecclimate) %>% 
      distinct_datasets(),
    "avalley"
  )
  expect_identical(
    kentvillegreenwood %>%
      select_locations(`KENTVILLE CDA CS`, `GREENWOOD A`, .factor = TRUE) %>%
      rename_locations(Greenwood = starts_with("GREENWOOD")) %>% 
      distinct_locations(),
    c("KENTVILLE CDA CS", "Greenwood")
  )
  expect_identical(
    kentvillegreenwood %>%
      select_params(mintemp, meantemp, maxtemp, .factor = TRUE) %>%
      rename_params(min_temp = mintemp) %>% 
      distinct_params(),
    c("min_temp", "meantemp", "maxtemp")
  )
})

# ---- base renaming functions ----

test_that("rename_cols_base function works", {
  df <- data.frame(a=1, b=2, c=3, d=4)
  expect_that(names(rename_cols_base(df, a="letter_a", b="letter_b")),
              equals(c("letter_a", "letter_b", "c", "d")))
})

test_that("rename_cols_base outputs a message when no names are found", {
  df <- data.frame(a=1, b=2, c=3, d=4)
  expect_message(rename_cols_base(df, notacolumn="willnotbeacolumn"), 
                 "Not all values were found: notacolumn")
})

test_that("rename_cols_base outputs a warning when duplicate names are created", {
  df <- data.frame(a=1, b=2, c=3, d=4)
  expect_message(rename_cols_base(df, b="a"), "Possible duplicated values in x: a")
})

test_that("rename_values_base function works", {
  x <- c("fish", "fish", "fish", "whistle")
  expect_that(rename_values_base(x, fish="newfish"), equals(c("newfish", "newfish", "newfish", "whistle")))
  expect_that(rename_values_base(x, whistle="newwhistle"), equals(c("fish", "fish", "fish", "newwhistle")))
  expect_that(rename_values_base(x, fish="newfish", default_value="not a fish"), 
              equals(c("newfish", "newfish", "newfish", "not a fish")))
})

test_that("rename_values_base outputs a message if values are not found", {
  x <- c("fish", "fish", "fish", "whistle")
  expect_message(rename_values_base(x, notfound="willneverbefound"), 
                 "Not all values were found: notfound")
})

test_that("rename_values_base outputs a message if values are duplicated", {
  x <- c("fish", "fish", "fish", "whistle")
  expect_message(rename_values_base(x, "whistle"="fish"), 
                 "Possible duplicated values in x: fish")
})

test_that("rename_values_base works on factors", {
  expect_identical(
    rename_values_base(factor(c("one", "two", "three", "one"), levels = c("one", "two", "three")), 
                       "one" = "number one"),
    factor(c("number one", "two", "three", "number one"), levels = c("number one", "two", "three"))
  )
})

test_that("mudata rename works", {
  data("kentvillegreenwood")
  md2 <- rename_datasets_base(kentvillegreenwood, ecclimate="avalley")
  expect_that(unique(c(md2$data$dataset, md2$locations$dataset, 
                       md2$params$dataset, md2$datasets$dataset)), equals("avalley"))
  
  md2 <- rename_locations_base(kentvillegreenwood, "GREENWOOD A"="Greenwood")
  expect_true("Greenwood" %in% unique(c(md2$data$location, md2$locations$location)))
  expect_false("GREENWOOD A" %in% unique(c(md2$data$location, md2$locations$location)))
  
  md2 <- rename_params_base(kentvillegreenwood, maxtemp="Maximum Temperature")
  expect_true("Maximum Temperature" %in% unique(c(md2$data$param, md2$params$param)))
  expect_false("maxtemp" %in% unique(c(md2$data$param, md2$params$param)))
  
  md2 <- rename_cols_base(kentvillegreenwood, latitude="lat", longitude="lon")
  expect_true("lon" %in% names(md2$locations))
  expect_false("longitude" %in% names(md2$locations))
  expect_true("lat" %in% md2$columns$column)
  expect_false("latitude" %in% md2$columns$column)
})
