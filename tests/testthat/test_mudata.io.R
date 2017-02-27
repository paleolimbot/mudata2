

context("mudata read/write")

test_that("read/write zip functions work", {
  data("kentvillegreenwood")
  outfile <- tempfile(fileext = ".zip")
  write.mudata.zip(kentvillegreenwood, outfile)
  md2 <- read.mudata.zip(outfile)
  expect_that(md2, is_a('mudata'))
  expect_true(all(sapply(kentvillegreenwood, nrow) == sapply(md2, nrow)))
  unlink(outfile)
})

test_that("read/write JSON functions work", {
  data("kentvillegreenwood")
  outfile <- tempfile(fileext = ".json")
  write.mudata.json(kentvillegreenwood, outfile)
  md2 <- read.mudata.json(outfile)
  expect_that(md2, is_a('mudata'))
  expect_true(all(sapply(kentvillegreenwood, nrow) == sapply(md2, nrow)))
  unlink(outfile)
})

test_that("autodetection of read function filename extension works", {
  data("kentvillegreenwood")
  outfile_json <- tempfile(fileext = ".json")
  write.mudata(kentvillegreenwood, outfile_json)
  expect_true(file.exists(outfile_json))
  expect_that(read.mudata.json(outfile_json), is_a('mudata'))
  unlink(outfile_json)
  
  outfile_zip <- tempfile(fileext = ".zip")
  write.mudata(kentvillegreenwood, outfile_zip)
  expect_true(file.exists(outfile_zip))
  expect_that(read.mudata.zip(outfile_zip), is_a('mudata'))
  unlink(outfile_zip)
})

test_that("invalid objects are not written", {
  data("kentvillegreenwood")
  kentvillegreenwood$data <- rbind(kentvillegreenwood$data, kentvillegreenwood$data)
  expect_error(validate.mudata(kentvillegreenwood))
  expect_error(write.mudata.json(kentvillegreenwood, tempfile()))
  expect_error(write.mudata.zip(kentvillegreenwood, tempfile()))
})

test_that("invalid objects are not read", {
  data("kentvillegreenwood")
  kentvillegreenwood$data <- rbind(kentvillegreenwood$data, kentvillegreenwood$data)
  expect_error(validate.mudata(kentvillegreenwood))
  outfile_json <- tempfile(fileext = ".json")
  outfile_zip <- tempfile(fileext = ".zip")
  
  write.mudata(kentvillegreenwood, outfile_json, validate=FALSE)
  write.mudata(kentvillegreenwood, outfile_zip, validate=FALSE)
  
  expect_error(read.mudata(outfile_json))
  expect_error(read.mudata(outfile_zip))
  
  unlink(outfile_zip)
  unlink(outfile_json)
})

test_that("retyping on read/write works", {
  data("kentvillegreenwood")
  outfile <- tempfile(fileext = ".zip")
  write.mudata.zip(kentvillegreenwood, outfile)
  expect_message(read.mudata.zip(outfile, retype=TRUE))
  
  md2 <- read.mudata.zip(outfile, retype=TRUE)
  expect_that(md2, is_a('mudata'))
  expect_that(md2$data$x, is_a(md2$columns$type[md2$columns$column == "x"]))
  unlink(outfile)
})


