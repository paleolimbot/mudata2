
context("tag expand/condense functions")

test_that("expand/condense functions work properly for data frames", {
  data("pocmaj")
  condensed <- condense.tags(pocmaj, tagcolumns = c("Ca", "Ti", "V"),
                             tagcolumn = 'thetagcolumn')
  expect_that(names(condensed), equals(c("core", "depth", "thetagcolumn")))
  reexpanded <- expand.tags(condensed, tagcolumn='thetagcolumn')
  expect_true(all(sapply(data.frame(pocmaj == reexpanded), all)))
})

test_that("expand/condense tags work properly for mudata objects", {
  data("kentvillegreenwood")
  md2 <- condense.tags(kentvillegreenwood)
  expect_that(names(md2$data), equals(c("dataset", "location", "param", "x", "value", "tags")))
  expect_that(names(md2$locations), equals(c("dataset", "location", "tags")))
  expect_that(names(md2$params), equals(c("dataset", "param", "tags")))
  expect_that(names(md2$datasets), equals(c("dataset", "tags")))
  expect_that(names(md2$columns), equals(c("dataset", "table", "column", "tags")))
  
  reexpanded <- expand.tags(md2)
  # there should not be a 'data quality' column since these were all NA
  expect_true(all(is.na(kentvillegreenwood$data$dataquality)))
  kentvillegreenwood$data$dataquality <- NULL
  #
  # make sure names are the same
  expect_true(all(sapply(mapply(`==`, lapply(kentvillegreenwood, names), lapply(reexpanded, names)), all)))
  # make sure flags data are the same (with "" being the same as NA)
  expect_true(all(mapply(function(v1, v2) {
    if(!is.na(v1) && v1 == "") {
      v1 <- NA
    }
    if(!is.na(v2) && v2 == "") {
      v2 <- NA
    }
    if(is.na(v1) && is.na(v2)) {
      return(TRUE)
    } else if(!any(is.na(v1), is.na(v2)) && (v1 == v2)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, kentvillegreenwood$data$flags, reexpanded$data$flags, USE.NAMES = FALSE)))
})