
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

test_that("expand.tags behaviour in constructor is consistent", {
  pocmajq <- as.qtag(pocmaj)
  pocmajq <- long(pocmajq)
  pocmajq <- aggregate(pocmajq)
  pocmajq <- rename.cols(pocmajq, core="location", depth="x")
  
  # base case, where data is not tagged
  md <- mudata(pocmajq)
  expect_true(all(!mapply(`%in%`, "tags", lapply(md, names), USE.NAMES = FALSE)))
  md <- mudata(pocmajq, expand.tags = FALSE)
  expect_true(all(c('dataset', 'location', 'param', 'x', 
                    'value', 'tags', 'table', 'column') %in% unique(unlist(lapply(md, names)))))
  expect_that(length(unique(unlist(lapply(md, names)))), equals(8))
  
  # base case, where data is tagged
  pocmajq$tags <- '{"thing": "val"}'
  md <- mudata(pocmajq)
  expect_true(all(!mapply(`%in%`, "tags", lapply(md, names), USE.NAMES = FALSE)))
  md <- mudata(pocmajq, expand.tags = FALSE)
  expect_true(all(c('dataset', 'location', 'param', 'x', 
                    'value', 'tags', 'table', 'column') %in% unique(unlist(lapply(md, names)))))
  expect_that(length(unique(unlist(lapply(md, names)))), equals(8))
})

test_that("duplicate data is detected", {
  pocmajq <- as.qtag(pocmaj)
  pocmajq <- long(pocmajq)
  # skip aggregation
  md <- mudata(rename.cols(pocmajq, core="location", depth="x"), validate = FALSE)
  expect_that(validate.mudata(md), 
              throws_error("dataset, location, param, and x do not identify unique rows for:.*"))
})

test_that("duplicate location metadata are detected", {
  data("kentvillegreenwood")
  expect_true(validate.mudata(kentvillegreenwood))
  kentvillegreenwood$locations <- rbind(kentvillegreenwood$locations, kentvillegreenwood$locations[1,])
  expect_error(validate.mudata(kentvillegreenwood), "Duplicate location in locations table:.*?")
})

test_that("duplicate param metadata are detected", {
  data("kentvillegreenwood")
  expect_true(validate.mudata(kentvillegreenwood))
  kentvillegreenwood$params <- rbind(kentvillegreenwood$params, kentvillegreenwood$params[1,])
  expect_error(validate.mudata(kentvillegreenwood), "Duplicate parameter in parameters table:.*?")
})

test_that("duplicate dataset metadata are detected", {
  data("kentvillegreenwood")
  expect_true(validate.mudata(kentvillegreenwood))
  kentvillegreenwood$datasets <- rbind(kentvillegreenwood$datasets, kentvillegreenwood$datasets[1,])
  expect_error(validate.mudata(kentvillegreenwood), "Duplicate dataset in datasets table")
})


test_that("duplicate column metadata are detected", {
  data("kentvillegreenwood")
  expect_true(validate.mudata(kentvillegreenwood))
  kentvillegreenwood$columns <- rbind(kentvillegreenwood$columns, kentvillegreenwood$columns[1,])
  expect_error(validate.mudata(kentvillegreenwood), "Duplicate column in columns table:.*?")
})

test_that("recombined subsetted objects are the same as the original", {
  pocmajq <- as.qtag(pocmaj)
  pocmajq <- long(pocmajq)
  pocmajq <- aggregate(pocmajq)
  pocmajq <- rename.cols(pocmajq, core="location", depth="x")
  
  md <- mudata(pocmajq)
  mdlocsub <- subset(md, locations="MAJ-1")
  mdlocsub2 <- subset(md, locations="POC-2")
  mdparamsub <- subset(md, params="Ca")
  mdparamsub2 <- subset(md, params=c("V", "Ti"))
  
  expect_that(nrow(rbind(mdlocsub, mdlocsub2)$data), equals(nrow(md$data)))
  expect_that(nrow(rbind(mdparamsub, mdparamsub2)$data), equals(nrow(md$data)))
})

test_that("summary object is a data frame", {
  pocmajq <- as.qtag(pocmaj)
  pocmajq <- long(pocmajq)
  pocmajq <- aggregate(pocmajq)
  pocmajq <- rename.cols(pocmajq, core="location", depth="x")
  
  md <- mudata(pocmajq)
  expect_that(summary(md), is_a("data.frame"))
})

test_that("printing of a mudata actually prints things", {
  pocmajq <- as.qtag(pocmaj)
  pocmajq <- long(pocmajq)
  pocmajq <- aggregate(pocmajq)
  pocmajq <- rename.cols(pocmajq, core="location", depth="x")
  
  md <- mudata(pocmajq)
  expect_that(print(md), is_a("mudata"))
  expect_output(print(md), "A mudata object with.*")
})


