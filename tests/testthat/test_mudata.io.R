
context("mudata read/write")

# create test data
pocmaj_data <- pocmajsum %>%
  dplyr::select(core, depth, Ca, Ti, V) %>%
  tidyr::gather(Ca, Ti, V, key = "param", value = "value") %>%
  dplyr::select(location = core, param, depth, value)
pocmaj_md <- mudata(pocmaj_data)

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

test_that("columns table is updated properly", {
  kg2 <- kentvillegreenwood
  kg2$columns[['type']][1] <- "a_new_type"
  
  # check that message is produced, and not when quiet = TRUE
  expect_message(update_columns_table(kg2), 
                 paste0("Replacing types ecclimate/data/dataset/character ",
                        "with ecclimate/data/dataset/a_new_type"))
  expect_silent(update_columns_table(kg2, quiet = TRUE))
  
  # check that column was updated
  updated <- update_columns_table(kg2)
  expect_identical(kg2$columns$type, updated$columns$type)
  
  # check that quiet = TRUE doesn't affect updating
  expect_identical(update_columns_table(kg2), 
                   update_columns_table(kg2, quiet = TRUE))
  
  # check that updating a columns table that doesn't need upating is quiet
  expect_identical(update_columns_table(kentvillegreenwood, quiet = FALSE),
                   kentvillegreenwood)
  
  # check that updating a columns table without a type column works
  kg2$columns$type <- NULL
  expect_false("type" %in% colnames(kg2$columns))
  expect_true("type" %in% colnames(update_columns_table(kg2)$columns))
  expect_identical(update_columns_table(kg2)$columns %>% dplyr::select(-type),
                   kg2$columns)
  # should be silent, since no information is replaced
  expect_silent(update_columns_table(kg2, quiet = FALSE))
})

test_that("read/write JSON functions work", {
  
  test_json <- function(md_object, debug = FALSE) {
    
    outfile <- tempfile(fileext = ".json")
    write_mudata_json(md_object, outfile)
    md2 <- read_mudata_json(outfile)
    
    if(debug) {
      browser()
    }
    
    # expect identical to original object
    expect_equal_mudata(md_object, md2)
    
    # expect to_ and from_ variants do the same
    md_json <- to_mudata_json(md_object)
    md3 <- from_mudata_json(md_json)
    expect_equal_mudata(md3, md_object)
    
    # cleanup file
    unlink(outfile)
  }
  
  test_json(kentvillegreenwood)
  test_json(subset(kentvillegreenwood, params = c("maxtemp", "mintemp", "meantemp")))
  test_json(pocmaj_md)
})
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


test_that("mudata_prepare_column and mudata_parse_column are opposites", {
  # create test df with all supported types
  test_df <- tibble::tibble(
    c1 = c(1, 2, 3),
    c1a = c(1L, 2L, 3L),
    c2 = c("one", "two", "three"),
    c3 = factor(c2, levels = c2),
    c4 = factor(c3, ordered = TRUE),
    c5 = as.Date(c(1, 2, 3), origin = Sys.Date()),
    c6 = as.POSIXct(c5),
    c7 = structure(list(list(1), list(2), list(3)), class = c("json_column", "list")),
    c8 = sf::st_as_sfc(c("POINT(0 0)", "POINT(1 1)", "POINT(2 2)")),
    c9 = hms::as.hms(1:3)
  )
  
  # get type strings, expected output classes
  type_strs <- generate_type_tbl(test_df) %>% tibble::deframe()
  output_clases <- vapply(type_strs, mudata:::parse_output_class, character(1))
  
  # get prepared columns
  prepared <- lapply(test_df, mudata_prepare_column, format = NA)
  prepared_json <- lapply(test_df, mudata_prepare_column, format = "json")
  prepared_csv <- lapply(test_df, mudata_prepare_column, format = "csv")
  
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

test_that("mudata_prepare_tbl works as intended", {
  # create test df with all supported types
  test_df <- tibble::tibble(
    c1 = c(1, 2, 3),
    c1a = c(1L, 2L, 3L),
    c2 = c("one", "two", "three"),
    c3 = factor(c2, levels = c2),
    c4 = factor(c3, ordered = TRUE),
    c5 = as.Date(c(1, 2, 3), origin = Sys.Date()),
    c6 = as.POSIXct(c5),
    c7 = list(list(1), list(2), list(3)),
    c8 = sf::st_as_sfc(c("POINT(0 0)", "POINT(1 1)", "POINT(2 2)")),
    c9 = hms::as.hms(1:3)
  )
  
  prepared <- mudata_prepare_tbl(test_df)
  # the last few columns should be character by default
  expect_is(prepared$c6, "character")
  expect_is(prepared$c7, "character")
  expect_is(prepared$c8, "character")
  expect_is(prepared$c9, "character")
  
  prepared_json <- mudata_prepare_tbl(test_df, format = "json")
  # the last few columns should be character usually
  expect_is(prepared_json$c6, "character")
  expect_is(prepared_json$c7, "list")
  expect_is(prepared_json$c8, "character")
  expect_is(prepared_json$c9, "character")
  
  # prepared csv should be same as defaults
  prepared_csv <- mudata_prepare_tbl(test_df, format = "csv")
  expect_identical(prepared_csv, prepared)
})

test_that("mudata_prepare_tbl and mudata_parse_tbl are opposites", {
  # create test df with all supported types
  test_df <- tibble::tibble(
    c1 = c(1, 2, 3),
    c1a = c(1L, 2L, 3L),
    c2 = c("one", "two", "three"),
    c3 = factor(c2, levels = c2),
    c4 = factor(c3, ordered = TRUE),
    c5 = as.Date(c(1, 2, 3), origin = Sys.Date()),
    c6 = as.POSIXct(c5),
    c7 = list(list(1), list(2), list(3)),
    c8 = sf::st_as_sfc(c("POINT(0 0)", "POINT(1 1)", "POINT(2 2)")),
    c9 = hms::as.hms(1:3)
  )
  
  # create prepared versions
  prepared <- mudata_prepare_tbl(test_df)
  prepared_csv <- mudata_prepare_tbl(test_df, format = "csv")
  prepared_json <- mudata_prepare_tbl(test_df, format = "json")
  
  # get type_str vector
  type_strs <- generate_type_tbl(test_df) %>% tibble::deframe()
  
  # create parsed versions
  parsed <- mudata_parse_tbl(prepared, type_str = type_strs)
  parsed_csv <- mudata_parse_tbl(prepared_csv, type_str = type_strs)
  parsed_json <- mudata_parse_tbl(prepared_json, type_str = type_strs)
  
  # define function to test equal-ness of columns
  # in this case dates loose some information, and the class of the JSON
  # column changes, but the dates still refer to the same moment in time
  col_equal <- function(val1, val2) {
    if(inherits(val1, "list") || inherits(val2, "list")) {
      # integers/numerics get confused here, so use == and mapply
      if(identical(unclass(val1), unclass(val2))) {
        return(TRUE)
      }
      all_equal <- try(all(mapply(`==`, val1, val2)), silent = TRUE)
      if(identical(all_equal, TRUE)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else if(inherits(val1, "POSIXct") && inherits(val2, "POSIXct")) {
      all_equal <- all(val1 == val2)
      tz1 <- attr(val1, "tzone")
      tz2 <- attr(val2, "tzone")
      if(is.null(tz1) || identical(tz1, "")) {
        if(is.null(tz2) || identical(tz2, "")) {
          tzequal <- TRUE
        } else {
          tzequal <- FALSE
        }
      } else {
        tzequal <- identical(tz1, tz2)
      }
      if(all_equal && !identical(val1, val2)) warning("date vectors not identical")
      all_equal && tzequal
    } else if(inherits(val1, "factor") || inherits(val2, "factor")) {
      identical(as.character(val1), as.character(val2))
    } else {
      identical(val1, val2)
    }
  }
  
  # expect true with all parsed dfs
  # currently time zone information isn't kept with the date vectors
  # this may be a problem later
  expect_true(all(mapply(col_equal, test_df, parsed)))
  expect_true(all(mapply(col_equal, test_df, parsed_csv)))
  expect_true(all(mapply(col_equal, test_df, parsed_json)))
})

test_that("datetimes are identical when read/written", {
  
  test_with_tz <- function(tz_name, debug = FALSE) {
    if(debug) {
      browser()
    }
    dt <- seq(as.POSIXct("1980-01-01 09:00"), as.POSIXct("1980-01-01 17:00"), by = 3600)
    dt <- lubridate::force_tz(dt, tz_name)
    expect_length(dt, 9)
    expect_equal(attr(dt, "tzone"), tz_name)
    
    # make sure prepared doesn't change when tzone is unspecified ("")
    prepared_dt <- mudata_prepare_column(dt)
    expect_equal(head(prepared_dt, 1), "1980-01-01T09:00:00")
    expect_equal(tail(prepared_dt, 1), "1980-01-01T17:00:00")
    
    # make sure times are read in identically
    type_str <- generate_type_str(dt)
    parsed_dt <- mudata_parse_column(prepared_dt, type_str = type_str)
    # try equality
    expect_equal(dt, parsed_dt)
    # check tzone attribute
    expect_equal(attr(dt, "tzone"), attr(parsed_dt, "tz"))
    # object should be identical
    expect_identical(dt, parsed_dt)
  }
  
  # test with lots of timezones
  test_with_tz("")
  test_with_tz("America/Halifax")
  test_with_tz("UTC")
  test_with_tz("America/Boise")
  test_with_tz("Pacific/Auckland")
  # lots of warnings, but no problems
  test_with_tz("UTC+4")
})

