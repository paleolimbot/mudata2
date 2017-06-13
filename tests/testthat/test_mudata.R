
context("mudata constructor")


pocmaj_long <- reshape2::melt(pocmajsum, id.vars = c("core", "depth"), 
                          measure.vars = c("Ti", "Ca", "V"),
                          variable.name = "param")
pocmaj_data <- rename.cols(pocmaj_long, core="location", depth="x")

test_that("mudata constructor creates a mudata object", {
  md <- mudata(pocmaj_data)
  expect_that(md, is_a("mudata"))
})

test_that("default dataset/location actually changes the default dataset/location name", {
  md <- mudata(pocmaj_data, dataset.id = "otherdataset")
  expect_that(md$datasets$dataset, equals("otherdataset"))
  expect_that(length(unique(md$data$dataset)), equals(1))
  expect_that(unique(md$data$dataset), equals("otherdataset"))
  
  pocmaj_data <- pocmaj_data[pocmaj_data$location == "POC-2",]
  pocmaj_data$location <- NULL
  md <- mudata(pocmaj_data, location.id = "otherlocation")
  expect_that(md$locations$location, equals("otherlocation"))
  expect_that(length(unique(md$data$location)), equals(1))
  expect_that(unique(md$data$location), equals("otherlocation"))
})

test_that("passing invalid inputs throws an error", {
  # invalid types
  expect_that(mudata(data=NULL), throws_error("Table 'data' is not a data\\.frame"))
  expect_that(mudata(pocmaj_data, locations=list()), throws_error("Table 'locations' is not a data\\.frame"))
  expect_that(mudata(pocmaj_data, params=list()), throws_error("Table 'params' is not a data\\.frame"))
  expect_that(mudata(pocmaj_data, columns=list()), throws_error("Table 'columns' is not a data\\.frame"))
  
  # datasets is special, can be a vector since this sometimes occurs in subset.
  expect_that(mudata(pocmaj_data, datasets=list()), 
              throws_error("Table 'datasets' is missing columns 'dataset'"))
  
  # invalid columns
  pocmajqinv <- pocmaj_data
  pocmajqinv$x <- NULL
  expect_that(mudata(pocmajqinv), throws_error("Table 'data' is missing columns 'x'"))
  expect_that(mudata(pocmaj_data, locations=data.frame()), 
              throws_error("Table 'locations' is missing columns 'dataset', 'location'"))
  expect_that(mudata(pocmaj_data, params=data.frame()), 
              throws_error("Table 'params' is missing columns 'dataset', 'param'"))
  expect_that(mudata(pocmaj_data, datasets=data.frame()), 
              throws_error("Table 'datasets' is missing columns 'dataset'"))
  expect_that(mudata(pocmaj_data, columns=data.frame()), 
              throws_error("Table 'columns' is missing columns 'dataset', 'table', 'column'"))
})

test_that("expand.tags behaviour in constructor is consistent", {
  # base case, where data is not tagged
  md <- mudata(pocmaj_data)
  expect_true(all(!mapply(`%in%`, "tags", lapply(md, names), USE.NAMES = FALSE)))
  md <- mudata(pocmaj_data, expand.tags = FALSE)
  expect_true(all(c('dataset', 'location', 'param', 'x', 
                    'value', 'tags', 'table', 'column') %in% unique(unlist(lapply(md, names)))))
  expect_that(length(unique(unlist(lapply(md, names)))), equals(8))
  
  # base case, where data is tagged
  pocmaj_data$tags <- '{"thing": "val"}'
  md <- mudata(pocmaj_data)
  expect_true(all(!mapply(`%in%`, "tags", lapply(md, names), USE.NAMES = FALSE)))
  md <- mudata(pocmaj_data, expand.tags = FALSE)
  expect_true(all(c('dataset', 'location', 'param', 'x', 
                    'value', 'tags', 'table', 'column') %in% unique(unlist(lapply(md, names)))))
  expect_that(length(unique(unlist(lapply(md, names)))), equals(8))
})

test_that("duplicate data is detected", {
  pocmaj_not_summarised <- reshape2::melt(pocmaj, id.vars = c("core", "depth"),
                                          variable.name = "param")
  pocmaj_not_summarised <- rename.cols(pocmaj_not_summarised, "depth" = "x")
  # skip aggregation
  md <- mudata(pocmaj_not_summarised, validate = FALSE)
  expect_that(validate.mudata(md), 
              throws_error("Duplicate data in data table"))
})

test_that("duplicate location metadata are detected", {
  data("kentvillegreenwood")
  expect_true(validate.mudata(kentvillegreenwood))
  kentvillegreenwood$locations <- rbind(kentvillegreenwood$locations, kentvillegreenwood$locations[1,])
  expect_error(validate.mudata(kentvillegreenwood), "Duplicate locations in locations table")
})

test_that("duplicate param metadata are detected", {
  data("kentvillegreenwood")
  expect_true(validate.mudata(kentvillegreenwood))
  kentvillegreenwood$params <- rbind(kentvillegreenwood$params, kentvillegreenwood$params[1,])
  expect_error(validate.mudata(kentvillegreenwood), "Duplicate params in params table")
})

test_that("duplicate dataset metadata are detected", {
  data("kentvillegreenwood")
  expect_true(validate.mudata(kentvillegreenwood))
  kentvillegreenwood$datasets <- rbind(kentvillegreenwood$datasets, kentvillegreenwood$datasets[1,])
  expect_error(validate.mudata(kentvillegreenwood), "Duplicate datasets in datasets table")
})


test_that("duplicate column metadata are detected", {
  data("kentvillegreenwood")
  expect_true(validate.mudata(kentvillegreenwood))
  kentvillegreenwood$columns <- rbind(kentvillegreenwood$columns, kentvillegreenwood$columns[1,])
  expect_error(validate.mudata(kentvillegreenwood), "Duplicate columns in columns table")
})

test_that("mudata objects subset properly", {
  md <- mudata(pocmaj_data)
  mdlocsub <- subset(md, locations="MAJ-1")
  expect_identical(unique(mdlocsub$data$location), "MAJ-1")
  expect_identical(unique(mdlocsub$locations$location), "MAJ-1")
  mdlocsub2 <- subset(md, locations="POC-2")
  expect_identical(unique(mdlocsub2$data$location), "POC-2")
  mdparamsub <- subset(md, params="Ca")
  expect_identical(unique(mdparamsub$data$param), "Ca")
  mdparamsub2 <- subset(md, params=c("V", "Ti"))
  expect_identical(sort(unique(mdparamsub2$data$param)), c("Ti", "V"))
})

test_that("recombined subsetted objects are the same as the original", {
  
  md <- mudata(pocmaj_data)
  mdlocsub <- subset(md, locations="MAJ-1")
  mdlocsub2 <- subset(md, locations="POC-2")
  mdparamsub <- subset(md, params="Ca")
  mdparamsub2 <- subset(md, params=c("V", "Ti"))
  
  expect_that(nrow(rbind(mdlocsub, mdlocsub2)$data), equals(nrow(md$data)))
  expect_that(nrow(rbind(mdparamsub, mdparamsub2)$data), equals(nrow(md$data)))
})

test_that("summary object is a data frame", {
  md <- mudata(pocmaj_data)
  expect_that(summary(md), is_a("data.frame"))
})

test_that("printing of a mudata actually prints things", {
  md <- mudata(pocmaj_data)
  expect_that(print(md), is_a("mudata"))
  expect_output(print(md), "A mudata object with.*")
})

test_that("grouped data frames don't cause problems in the mudata constructor", {
  expect_silent(mudata(dplyr::group_by(pocmaj_data, location, param)))
})

test_that("grouped data frames don't cause problems in summary and print methods", {
  md <- mudata(pocmaj_data)
  md$data <- dplyr::group_by(md$data, location, param)
  expect_silent(summary(md))
  expect_silent(print(md))
})
