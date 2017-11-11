
context("biplot functions")

# create test data
pocmaj_data <- parallel_gather(pocmajsum, key = "param", 
                               value = c(Ca, Ti, V),
                               sd = c(Ca_sd, Ti_sd, V_sd)) %>%
  dplyr::rename(location = core)
kvtemp <- subset(kentvillegreenwood, params = c("mintemp", "maxtemp", "meantemp"))

# ---- tests ----

test_that("autobiplot works on both data frames and mudata objects", {
  # these are manual tests
  expect_is(autobiplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param"),
            "ggplot")
  expect_is(autobiplot(kvtemp), "ggplot")
})

test_that("biplot works on both data frames and mudata objects", {
  biplot(kvtemp)
  long_biplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param")
  expect_true(TRUE) # these are manual tests
})

test_that("error bars show up in autoplot", {
  autobiplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param", 
             error_var = "sd")
  autobiplot(alta_lake, error_var = "stdev")
  expect_true(TRUE) # these are manual tests
})

test_that("long_pairs finds invalid inputs", {
  # vars not in pocmaj_data
  expect_error(long_pairs(pocmaj_data, id_vars = c("core", "depth"), name_var = "param"),
               "Table 'x' is missing columns 'core'")
  expect_error(long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "params"),
               "Table 'x' is missing columns 'params'")
  
  # names not in param
  expect_error(long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param", 
               names_x = "fish"), "The following names were missing from param: fish")
  expect_error(long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param", 
                          names_y = "fish"), "The following names were missing from param: fish")
  expect_error(long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param",
                          names_x = "thing", names_y = "fish"), 
               "The following names were missing from param: thing, fish")
  
  # non data frame input
  expect_error(long_pairs(NULL, id_vars = c("location", "depth"), name_var = "param"), 
               "Table 'x' is not a data.frame")
  
  # messages for when names_x and names_y are guessed
  expect_message(long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param"),
                 'Using names_x = c\\("Ti", "Ca"\\), names_y = c\\("V", "Ti"\\)')
  expect_message(long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param",
                            names_x = c("Ca", "Ti", "V")),
                 'Using names_x = c\\("Ti", "Ca"\\), names_y = c\\("V", "Ti"\\)')
})

test_that("long_biplot functions detect invalid inputs", {
  # vars not in pocmaj_data
  expect_error(long_biplot(pocmaj_data, id_vars = c("core", "depth"), name_var = "param"),
               "Table 'x' is missing columns 'core'")
  expect_error(long_biplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "params"),
               "Table 'x' is missing columns 'params'")
  
  # names not in param
  expect_error(long_biplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param", 
                          names_x = "fish"), 'Zero rows were found for names_x = c\\("fish"\\)')
  
  # non data frame input
  expect_error(long_biplot(NULL, id_vars = c("location", "depth"), name_var = "param"), 
               "Table 'x' is not a data.frame")
})

test_that("autobiplot.data.frame funcion detects invalid inputs", {
  # vars not in pocmaj_data
  expect_error(autobiplot(pocmaj_data, id_vars = c("core", "depth"), name_var = "param"),
               "Table 'x' is missing columns 'core'")
  expect_error(autobiplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "params"),
               "Table 'x' is missing columns 'params'")
  
  # names not in param
  expect_error(autobiplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param", 
                          names_x = "fish"), "The following names were missing from .name: fish")
  expect_error(autobiplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param", 
                          names_y = "fish"), "The following names were missing from .name: fish")
  expect_error(autobiplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param",
                          names_x = "thing", names_y = "fish"), 
               "The following names were missing from .name: thing, fish")
  
  # messages for when names_x and names_y are guessed
  expect_message(autobiplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param"),
                 'Using names_x = c\\("Ti", "Ca"\\), names_y = c\\("V", "Ti"\\)')
  expect_message(autobiplot(pocmaj_data, id_vars = c("location", "depth"), name_var = "param",
                            names_x = c("Ca", "Ti", "V")),
                 'Using names_x = c\\("Ti", "Ca"\\), names_y = c\\("V", "Ti"\\)')
})

test_that("long_pairs correctly assigns parameter combinations", {
  # expect identical behaviour when names_x = NULL to names_x = unique(params)
  all_params <- unique(pocmaj_data$param)
  expect_identical(long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param"),
                   long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param",
                              names_x = all_params))
  
  # function to identify a combination
  make_combo_id <- function(x, y) {
    paste(sort(c(x, y)), collapse = " ")
  }
  # create all combinations for pocmaj
  all_combinations <- expand.grid(.name_x = all_params, .name_y = all_params,
                                  stringsAsFactors = FALSE) %>%
    dplyr::filter(.name_x != .name_y) %>%
    dplyr::mutate(combo_id = mapply(make_combo_id, .name_x, .name_y)) %>%
    dplyr::distinct(combo_id) %>%
    dplyr::pull(combo_id)
  
  auto_combinations <- long_pairs(pocmaj_data, id_vars = c("location", "depth"), 
                                  name_var = "param") %>%
    dplyr::mutate(combo_id = mapply(make_combo_id, 
                                    as.character(.name_x), 
                                    as.character(.name_y))) %>%
    dplyr::distinct(combo_id) %>%
    dplyr::pull(combo_id)
    
  expect_identical(sort(all_combinations), sort(auto_combinations))
})

test_that("long_pairs name_var can be included in id_vars", {
  expect_identical(long_pairs(pocmaj_data, id_vars = c("location", "depth", "param"), 
                              name_var = "param"),
                   long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param"))
})

test_that("long_pairs handles grouped data frames", {
  pocmaj_data2 <- pocmaj_data %>% dplyr::group_by(location)
  expect_identical(long_pairs(pocmaj_data2, id_vars = c("location", "depth"), name_var = "param"),
                   long_pairs(pocmaj_data, id_vars = c("location", "depth"), name_var = "param"))
})

test_that("long_pairs handles zero-row combinations gracefully", {
  # make depth columns such that there will be zero rows
  # for at least one parameter combination
  pocmaj_data2 <- pocmaj_data %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(depth2 = ifelse(param == "Ca", 6:12, depth)) %>%
    dplyr::mutate(depth3 = ifelse(param == "V", 12:17, depth2))
  
  one_pair <- long_pairs(pocmaj_data2, id_vars = c("location", "depth2"), name_var = "param")
  expect_equal(nrow(dplyr::count(one_pair, .name_x, .name_y)), 1)
  zero_pairs <- long_pairs(pocmaj_data2, id_vars = c("location", "depth3"), name_var = "param")
  expect_equal(nrow(zero_pairs), 0)
})

test_that("max_names is respected in long_pairs, autobiplot, and long_biplot", {
  # kentvillegreenwood has enough parameters that biplotting is a little unwieldy
  kvdata <- kentvillegreenwood$data
  # define function to count the number of pairs found
  n_combinations <- function(long_pairs_result) {
    long_pairs_result %>%
      dplyr::distinct(.name_x, .name_y) %>%
      nrow()
  }
  
  expect_message(long_pairs(kvdata, id_vars = c("location", "date"), name_var = "param"),
                 "Only using first 5 names. Use max_names = FALSE to use all combinations of names.")
  expect_message(autobiplot(kvdata, id_vars = c("location", "date"), name_var = "param"),
                 "Only using first 5 names. Use max_names = FALSE to use all combinations of names.")
  expect_message(long_biplot(kvdata, id_vars = c("location", "date"), name_var = "param"),
                 "Only using first 5 names. Use max_names = FALSE to use all combinations of names.")

  constrained_result <- long_pairs(kvdata, id_vars = c("location", "date"), name_var = "param",
                                   max_names = 5)
  unconstrained_result <- long_pairs(kvdata, id_vars = c("location", "date"), name_var = "param", 
                                     max_names = FALSE)
  expect_equal(n_combinations(constrained_result), 4*5 / 2)
  expect_equal(n_combinations(unconstrained_result), 10*11 / 2)
})


