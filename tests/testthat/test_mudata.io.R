
context("mudata read/write")

# test_that("read/write zip functions work", {
#   data("kentvillegreenwood")
#   outfile <- tempfile(fileext = ".zip")
#   write.mudata.zip(kentvillegreenwood, outfile)
#   md2 <- read.mudata.zip(outfile)
#   expect_that(md2, is_a('mudata'))
#   expect_true(all(sapply(kentvillegreenwood, nrow) == sapply(md2, nrow)))
#   unlink(outfile)
# })
# 
# test_that("read/write JSON functions work", {
#   data("kentvillegreenwood")
#   outfile <- tempfile(fileext = ".json")
#   write.mudata.json(kentvillegreenwood, outfile)
#   md2 <- read.mudata.json(outfile)
#   expect_that(md2, is_a('mudata'))
#   expect_true(all(sapply(kentvillegreenwood, nrow) == sapply(md2, nrow)))
#   unlink(outfile)
# })
# 
# test_that("autodetection of read function filename extension works", {
#   data("kentvillegreenwood")
#   outfile_json <- tempfile(fileext = ".json")
#   write.mudata(kentvillegreenwood, outfile_json)
#   expect_true(file.exists(outfile_json))
#   expect_that(read.mudata.json(outfile_json), is_a('mudata'))
#   unlink(outfile_json)
#   
#   outfile_zip <- tempfile(fileext = ".zip")
#   write.mudata(kentvillegreenwood, outfile_zip)
#   expect_true(file.exists(outfile_zip))
#   expect_that(read.mudata.zip(outfile_zip), is_a('mudata'))
#   unlink(outfile_zip)
# })
# 
# test_that("invalid objects are not written", {
#   data("kentvillegreenwood")
#   kentvillegreenwood$data <- rbind(kentvillegreenwood$data, kentvillegreenwood$data)
#   expect_error(validate_mudata(kentvillegreenwood))
#   expect_error(write.mudata.json(kentvillegreenwood, tempfile()))
#   expect_error(write.mudata.zip(kentvillegreenwood, tempfile()))
# })
# 
# test_that("invalid objects are not read", {
#   data("kentvillegreenwood")
#   kentvillegreenwood$data <- rbind(kentvillegreenwood$data, kentvillegreenwood$data)
#   expect_error(validate_mudata(kentvillegreenwood))
#   outfile_json <- tempfile(fileext = ".json")
#   outfile_zip <- tempfile(fileext = ".zip")
#   
#   write.mudata(kentvillegreenwood, outfile_json, validate=FALSE)
#   write.mudata(kentvillegreenwood, outfile_zip, validate=FALSE)
#   
#   expect_error(read.mudata(outfile_json))
#   expect_error(read.mudata(outfile_zip))
#   
#   unlink(outfile_zip)
#   unlink(outfile_json)
# })
# 
# test_that("retyping on read/write works", {
#   data("kentvillegreenwood")
#   outfile <- tempfile(fileext = ".zip")
#   write.mudata.zip(kentvillegreenwood, outfile)
#   expect_message(read.mudata.zip(outfile, retype=TRUE))
#   
#   md2 <- read.mudata.zip(outfile, retype=TRUE)
#   expect_that(md2, is_a('mudata'))
#   expect_that(md2$data$x, is_a(md2$columns$type[md2$columns$column == "x"]))
#   unlink(outfile)
# })


test_that("mudata_prepare_column_write and mudata_parse_column are opposites", {
  # create test df with all supported types
  test_df <- tibble::tibble(
    c1 = c(1, 2, 3),
    c1a = c(1L, 2L, 3L),
    c2 = c("one", "two", "three"),
    c3 = factor(c2, levels = c2),
    c4 = factor(c3, ordered = TRUE),
    c5 = as.Date(c(1, 2, 3), origin = Sys.Date()),
    c6 = as.POSIXct(c5),
    c7 = parse_json(c("{}", "{}", "[]")),
    c8 = sf::st_as_sfc(c("POINT(0 0)", "POINT(1 1)", "POINT(2 2)")),
    c9 = hms::as.hms(1:3)
  )
  
  # get type strings, expected output classes
  type_strs <- generate_type_tbl(test_df) %>% tibble::deframe()
  output_clases <- vapply(type_strs, parse_output_class, character(1))
  
  # get prepared columns
  prepared <- lapply(test_df, mudata_prepare_column_write, format = NA)
  prepared_json <- lapply(test_df, mudata_prepare_column_write, format = "json")
  prepared_csv <- lapply(test_df, mudata_prepare_column_write, format = "csv")
  
  # parse prepared columns using mudata_parse_column
  parsed <- mapply(mudata_parse_column, prepared, type_strs, SIMPLIFY = FALSE)
  parsed_json <- mapply(mudata_parse_column, prepared_json, type_strs, SIMPLIFY = FALSE)
  parsed_csv <- mapply(mudata_parse_column, prepared_csv, type_strs, SIMPLIFY = FALSE)
  
  # test that results inherit from output_classes
  classes <- lapply(parsed, class)
  classes_json <- lapply(parsed_json, class)
  classes_csv <- lapply(parsed_csv, class)
  
  # classes should be all the same
  expect_identical(classes, classes_json)
  expect_identical(classes, classes_csv)
  
  # and inherit from output_classes
  expect_true(all(mapply(inherits, parsed, output_clases)))
  expect_true(all(mapply(inherits, parsed_json, output_clases)))
  expect_true(all(mapply(inherits, parsed_csv, output_clases)))
})

