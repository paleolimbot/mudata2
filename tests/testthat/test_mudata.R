
context("mudata constructor")

test_that("mudata constructor creates a mudata object", {
  pocmajq <- as.qtag(pocmaj)
  pocmajq <- long(pocmajq)
  pocmajq <- aggregate(pocmajq)
  md <- mudata(rename.cols(pocmajq, core="location", depth="x"))
  expect_that(md, is_a("mudata"))
})

test_that("default dataset/location actually changes the default dataset/location name", {
  pocmajq <- as.qtag(pocmaj)
  pocmajq <- long(pocmajq)
  pocmajq <- aggregate(pocmajq)
  md <- mudata(rename.cols(pocmajq, core="location", depth="x"), dataset.id = "otherdataset")
  expect_that(md$datasets$dataset, equals("otherdataset"))
  expect_that(length(unique(md$data$dataset)), equals(1))
  expect_that(unique(md$data$dataset), equals("otherdataset"))
  
  pocmajq <- pocmajq[pocmajq$core == "POC-2",]
  pocmajq$core <- NULL
  md <- mudata(rename.cols(pocmajq, depth="x"), location.id = "otherlocation")
  expect_that(md$locations$location, equals("otherlocation"))
  expect_that(length(unique(md$data$location)), equals(1))
  expect_that(unique(md$data$location), equals("otherlocation"))
})

test_that("passing invalid inputs throws an error", {
  pocmajq <- as.qtag(pocmaj)
  pocmajq <- long(pocmajq)
  pocmajq <- aggregate(pocmajq)
  pocmajq <- rename.cols(pocmajq, core="location", depth="x")
  
  # invalid types
  expect_that(mudata(data=NULL), throws_error("Table 'data' is not a data\\.frame"))
  expect_that(mudata(pocmajq, locations=list()), throws_error("Table 'locations' is not a data\\.frame"))
  expect_that(mudata(pocmajq, params=list()), throws_error("Table 'params' is not a data\\.frame"))
  expect_that(mudata(pocmajq, columns=list()), throws_error("Table 'columns' is not a data\\.frame"))
  
  # datasets is special, can be a vector since this sometimes occurs in subset.
  expect_that(mudata(pocmajq, datasets=list()), 
              throws_error("Table 'datasets' is missing columns 'dataset'"))
  
  # invalid columns
  pocmajqinv <- pocmajq
  pocmajqinv$x <- NULL
  expect_that(mudata(pocmajqinv), throws_error("Table 'data' is missing columns 'x'"))
  expect_that(mudata(pocmajq, locations=data.frame()), 
              throws_error("Table 'locations' is missing columns 'dataset', 'location'"))
  expect_that(mudata(pocmajq, params=data.frame()), 
              throws_error("Table 'params' is missing columns 'dataset', 'param'"))
  expect_that(mudata(pocmajq, datasets=data.frame()), 
              throws_error("Table 'datasets' is missing columns 'dataset'"))
  expect_that(mudata(pocmajq, columns=data.frame()), 
              throws_error("Table 'columns' is missing columns 'dataset', 'table', 'column'"))
})


