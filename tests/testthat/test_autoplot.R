
context("test automatic plotting functions")

# create test data
pocmaj_data <- pocmajsum %>%
  dplyr::select(core, depth, Ca, Ti, V) %>%
  tidyr::gather(Ca, Ti, V, key = "param", value = "value") %>%
  dplyr::select(location = core, param, depth, value)
kvtemp <- subset(kentvillegreenwood, params = c("mintemp", "maxtemp", "meantemp"))

# ---- tests ----

test_that("long_plot_base correctly guesses mappings", {
  
  expect_message(long_plot_base(pocmaj_data),
                 'Using id_vars = c\\("location", "param", "depth"\\)')
  expect_message(long_plot_base(pocmaj_data),
                 'Using x = "depth", y = "value"')
  expect_message(long_plot_base(pocmaj_data),
                 'Using facets = c\\("param"\\)')
  expect_message(long_plot_base(pocmaj_data),
                 'Using col = "location"')
  
  expect_message(long_plot_base(kentvillegreenwood$data),
                 'Using id_vars = c\\("dataset", "location", "param", "date"\\)')
  expect_message(long_plot_base(kentvillegreenwood$data),
                 'Using x = "date", y = "value"')
  expect_message(long_plot_base(kentvillegreenwood$data),
                 'Using facets = c\\("param"\\)')
  expect_message(long_plot_base(kentvillegreenwood$data),
                 'Using col = "location", pch = "dataset"')
})

test_that("long_plot_base produces expected outout", {
  pocmaj_output <- long_plot_base(pocmaj_data)
  kg_output <- long_plot_base(kentvillegreenwood$data)
  
  # data should be passed through unchanged
  expect_identical(pocmaj_output$.data, pocmaj_data)
  expect_identical(kg_output$.data, kentvillegreenwood$data)
  
  # expect base names to be data, geom, mapping
  expect_equal(names(pocmaj_output), c(".data", "geom", "mapping"))
  expect_equal(names(kg_output), c(".data", "geom", "mapping"))
  
  # expect mapping names to be x, y, facets, measure_var, more_args
  expect_equal(names(pocmaj_output$mapping),
               c("x", "y", "error_var", "facets", "measure_var", "more_args"))
  expect_equal(names(kg_output$mapping),
               c("x", "y", "error_var", "facets", "measure_var", "more_args"))
  
  # all mappings should be column names
  expect_true(all(unlist(pocmaj_output$mapping) %in% colnames(pocmaj_output$.data)))
  expect_true(all(unlist(kg_output$mapping) %in% colnames(kg_output$.data)))
  
  # geom should be a character vector
  expect_is(pocmaj_output$geom, "character")
  expect_is(kg_output$geom, "character")
})

test_that("variables not in .data throws error", {
  expect_error(long_plot_base(pocmaj_data, measure_var = "not_a_column"),
               "Table '.data' is missing columns 'not_a_column'")
  expect_error(long_plot_base(kentvillegreenwood$data, id_vars = "not_a_column"),
               "Table '.data' is missing columns 'not_a_column'")
  expect_error(long_plot_base(kentvillegreenwood$data, x = "not_a_column"),
               "Table '.data' is missing columns 'not_a_column'")
  expect_error(long_plot_base(kentvillegreenwood$data, y = "not_a_column"),
               "Table '.data' is missing columns 'not_a_column'")
  expect_error(long_plot_base(kentvillegreenwood$data, facets = "not_a_column"),
               "Table '.data' is missing columns 'not_a_column'")
  expect_error(long_plot_base(kentvillegreenwood$data, col = "not_a_column"),
               "Table '.data' is missing columns 'not_a_column'")
  expect_error(long_plot_base(kentvillegreenwood$data, error_var = "not_a_column"),
               "Table '.data' is missing columns 'not_a_column'")
})

test_that("zero-row data frames throw an error in long_plot_base", {
  expect_error(long_plot_base(data.frame(value = character(0))),
               ".data contains no data")
})

test_that("x and y vars are guessed correctly", {
  out <- long_plot_base(tibble::tibble(a = 4, b = 7, value = 9))
  expect_equal(out$mapping$x, "b")
  out <- long_plot_base(tibble::tibble(a = 4, b = "seven", value = 9))
  expect_equal(out$mapping$x, "a")
  expect_equal(out$mapping$facets, "b")
})

test_that("autoplot returns ggplot objects", {
  expect_is(ggplot2::autoplot(kentvillegreenwood), "ggplot")
  expect_is(long_ggplot(kentvillegreenwood$data), "ggplot")
})

test_that("limited default number of facets are repected", {
  expect_message(plot(kentvillegreenwood), 
                 "Using first 9 facets of 11. Use max_facets = FALSE to plot all facets") 
})

test_that("manual tests for plot()", {
  plot(kentvillegreenwood)
  plot(kentvillegreenwood, max_facets = 6)
  plot(kentvillegreenwood, max_facets = FALSE)
  plot(kentvillegreenwood, geom = "point")
  plot(kentvillegreenwood, geom = "p")
  plot(kentvillegreenwood, geom = "b")
  plot(kentvillegreenwood, geom = "path")
  
  plot(kentvillegreenwood, facets = "location")
  plot(kentvillegreenwood, y = "date")
  
  # when there are no non-position aesthetics things should also work in plot()
  long_plot(pocmajsum, c("core", "depth"), "Ca")
  
  expect_true(TRUE)
})

test_that("formulas or character vectors can be supplied to facets", {
  expect_identical(long_plot_base(kentvillegreenwood$data, 
                                  facets = c("location", "param")),
                   long_plot_base(kentvillegreenwood$data, 
                                  facets = ~location + param))
  
  expect_identical(long_plot_base(kentvillegreenwood$data, 
                                  facets = c("location", "param")),
                   long_plot_base(kentvillegreenwood$data, 
                                  facets = location ~ param))
})

test_that("multiple variables can be suppied to facets", {
  fout <- long_plot_base(kentvillegreenwood$data, facets = c("location", "param"))
  expect_equal(fout$mapping$facets, c("location", "param"))
  expect_equal(fout$mapping$more_args$col, "dataset")
  
  # manual test of multiple facets
  expect_false(identical(
    plot(kentvillegreenwood, facets = location ~ param)$facet_df,
    plot(kentvillegreenwood, facets = param ~ location)$facet_df
  ))
  
  # check that multiple identical facets are ok
  expect_identical(plot(kentvillegreenwood, facets = param ~ param)$facet_df,
                   plot(kentvillegreenwood, facets = ~param)$facet_df)
  ggplot2::autoplot(kentvillegreenwood, facets = location ~ param)
  ggplot2::autoplot(kentvillegreenwood, facets = param ~ location)
  
  # check that multiple identical facets are ok
  ggplot2::autoplot(kentvillegreenwood, facets = param ~ param)
})

test_that("manual tests for autoplot()", {
  ggplot2::autoplot(kentvillegreenwood)
  ggplot2::autoplot(kentvillegreenwood, geom = "point")
  ggplot2::autoplot(kentvillegreenwood, geom = "line")
  
  ggplot2::autoplot(kentvillegreenwood, facets = "location")
  ggplot2::autoplot(kentvillegreenwood, y = "date")
  
  # check error bars
  pocmajlong <- parallel_gather(pocmajsum, key = "param", 
                                value = c(Ca, Ti, V),
                                sd = c(Ca_sd, Ti_sd, V_sd))
  long_ggplot(pocmajlong, error_var = "sd")
  long_ggplot(pocmajlong, y = "depth", error_var = "sd")
  
  expect_true(TRUE)
})


test_that("grouped data frames don't cause problems in plot()", {
  datatable <- pocmaj %>%
    tidyr::gather(Ca, Ti, V, key = "param", value = "param_value") %>%
    dplyr::group_by(core, param, depth) %>%
    dplyr::summarise(value=mean(param_value), sd=mean(param_value)) %>%
    dplyr::rename(location = core)
  
  # check that autoplot works
  expect_is(long_ggplot(datatable), "ggplot")
  
  # check that plot works
  expect_identical(long_plot(datatable)$facet_df,
                   long_plot(dplyr::ungroup(datatable))$facet_df)
})

test_that("numeric variables are correctly identified", {
  df <- data.frame(a=factor("a factor"), b="not a factor", c=4,
                   d=4.5, e=Sys.Date(), f=Sys.time())
  expect_that(names(df)[sapply(df, is.numericish)], equals(c("c", "d", "e", "f")))
  expect_that(!sapply(df, is.numericish), equals(sapply(df, ggplot2:::is.discrete)))
})
