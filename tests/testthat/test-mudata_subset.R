
pocmaj_data <- pocmajsum %>%
  dplyr::select(core, depth, Ca, Ti, V) %>%
  tidyr::gather(Ca, Ti, V, key = "param", value = "value") %>%
  dplyr::select(location = core, param, depth, value)

test_that("mudata objects subset properly", {
  # location subsets
  md <- mudata(pocmaj_data)
  mdlocsub <- subset(md, locations = "MAJ-1")
  expect_identical(unique(mdlocsub$data$location), "MAJ-1")
  expect_identical(unique(mdlocsub$locations$location), "MAJ-1")
  mdlocsub2 <- subset(md, locations = "POC-2")
  expect_identical(unique(mdlocsub2$data$location), "POC-2")

  # param subsets
  mdparamsub <- subset(md, params = "Ca")
  expect_identical(unique(mdparamsub$data$param), "Ca")
  mdparamsub2 <- subset(md, params = c("V", "Ti"))
  expect_identical(sort(unique(mdparamsub2$data$param)), c("Ti", "V"))
  expect_identical(sort(unique(mdparamsub2$params$param)), c("Ti", "V"))

  # both subsets
  mdbothsub <- subset(md, locations = "MAJ-1", params = c("Ti", "V"))
  expect_identical(unique(mdbothsub$data$location), "MAJ-1")
  expect_identical(unique(mdbothsub$locations$location), "MAJ-1")
  expect_identical(sort(unique(mdbothsub$data$param)), c("Ti", "V"))
  expect_identical(sort(unique(mdbothsub$params$param)), c("Ti", "V"))

  # dataset subsets
  md2 <- md %>% rename_datasets(ds2 = "default")
  mddatasetsub <- subset(rbind(md, md2), datasets = "ds2")
  expect_identical(distinct_datasets(mddatasetsub), "ds2")
})

test_that("select* methods work", {
  expect_identical(
    kentvillegreenwood %>% select_datasets(ecclimate) %>% distinct_datasets(),
    "ecclimate"
  )
  expect_identical(
    kentvillegreenwood %>% select_datasets(newds = ecclimate) %>% distinct_datasets(),
    "newds"
  )
  expect_identical(
    kentvillegreenwood %>% select_locations("GREENWOOD A") %>% distinct_locations(),
    "GREENWOOD A"
  )
  expect_identical(
    kentvillegreenwood %>% select_locations(Greenwood = "GREENWOOD A") %>% distinct_locations(),
    "Greenwood"
  )
  expect_true(setequal(
    kentvillegreenwood %>% select_locations(Greenwood = "GREENWOOD A", "KENTVILLE CDA CS") %>%
      distinct_locations(),
    c("KENTVILLE CDA CS", "Greenwood")
  ))
  expect_true(setequal(
    kentvillegreenwood %>% select_params(ends_with("temp")) %>% distinct_params(),
    c("mintemp", "maxtemp", "meantemp")
  ))
  expect_true(setequal(
    kentvillegreenwood %>% select_params(MaxTemp = maxtemp, mintemp) %>% distinct_params(),
    c("MaxTemp", "mintemp")
  ))
})

test_that("recombined subsetted objects are the same as the original", {
  md <- mudata(pocmaj_data)
  mdlocsub <- subset(md, locations = "MAJ-1")
  mdlocsub2 <- subset(md, locations = "POC-2")
  mdparamsub <- subset(md, params = "Ca")
  mdparamsub2 <- subset(md, params = c("V", "Ti"))

  expect_equal(nrow(rbind(mdlocsub, mdlocsub2)$data), nrow(md$data))
  expect_equal(nrow(rbind(mdparamsub, mdparamsub2)$data), nrow(md$data))
})

test_that("filter_* functions work as expected", {
  # filter datasets
  expect_identical(
    filter_datasets(kentvillegreenwood, dataset == "ecclimate"),
    kentvillegreenwood
  )

  expect_identical(
    filter_datasets(kentvillegreenwood, url == "not a url") %>%
      lapply(nrow) %>%
      unlist() %>%
      unique() %>%
      unname(),
    0L
  )

  # filter data
  expect_identical(
    filter_data(kentvillegreenwood, param %in% c("mintemp", "maxtemp", "meantemp")),
    select_params(kentvillegreenwood, ends_with("temp"))
  )

  # filter locations
  expect_identical(
    filter_locations(kentvillegreenwood, grepl("CS$", location)),
    select_locations(kentvillegreenwood, ends_with("CS"))
  )

  # filter params
  expect_identical(
    filter_params(kentvillegreenwood, grepl("Temp", label)),
    select_params(kentvillegreenwood, ends_with("temp"))
  )
})

test_that("identity subsets work", {
  expect_identical(kentvillegreenwood, filter_data(kentvillegreenwood))
  expect_identical(kentvillegreenwood, filter_datasets(kentvillegreenwood))
  expect_identical(kentvillegreenwood, filter_locations(kentvillegreenwood))
  expect_identical(kentvillegreenwood, filter_params(kentvillegreenwood))
})

test_that("subsets with factorized columns work", {
  kg2 <- kentvillegreenwood
  kg2$data$param <- factor(kg2$data$param)
  kg2$params$param <- factor(kg2$params$param)
  kg2$data$location <- factor(kg2$data$location)
  kg2$locations$location <- factor(kg2$locations$location)
  kg2$data$dataset <- factor(kg2$data$dataset)
  kg2$locations$dataset <- factor(kg2$locations$dataset)
  kg2$params$dataset <- factor(kg2$params$dataset)
  kg2$columns$dataset <- factor(kg2$columns$dataset)
  kg2$datasets$dataset <- factor(kg2$datasets$dataset)

  expect_identical(
    kg2 %>% select_params(ends_with("temp")) %>% distinct_params(),
    kentvillegreenwood %>% select_params(ends_with("temp")) %>% distinct_params()
  )
})

test_that("maintaining order using .factor = TRUE works", {
  kgtemp <- kentvillegreenwood %>% select_params(mt = mintemp, meantemp, maxtemp, .factor = TRUE)
  expect_equal(distinct_params(kgtemp), c("mt", "meantemp", "maxtemp"))
  expect_s3_class(kgtemp$data$param, "factor")
  expect_s3_class(kgtemp$params$param, "factor")

  kgloc <- kentvillegreenwood %>% select_locations(
    Kentville = `KENTVILLE CDA CS`, Greenwood = `GREENWOOD A`,
    .factor = TRUE
  )
  expect_equal(distinct_locations(kgloc), c("Kentville", "Greenwood"))
  expect_s3_class(kgloc$data$location, "factor")
  expect_s3_class(kgloc$locations$location, "factor")

  kgds <- kentvillegreenwood %>% select_datasets(everything(), .factor = TRUE)
  expect_s3_class(kgds$data$dataset, "factor")
  expect_s3_class(kgds$params$dataset, "factor")
  expect_s3_class(kgds$locations$dataset, "factor")
  expect_s3_class(kgds$datasets$dataset, "factor")
  expect_s3_class(kgds$columns$dataset, "factor")
})

test_that("param table gets filtered properly when there are multiple datasets", {
  ll2 <- long_lake %>%
    tbl_data() %>%
    dplyr::mutate(dataset = location) %>%
    mudata(x_columns = "depth")
  expect_equal(
    ll2 %>% select_datasets("LL GC10") %>% tbl_params() %>% dplyr::distinct(dataset) %>% dplyr::pull(),
    "LL GC10"
  )
})
