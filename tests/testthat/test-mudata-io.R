
# expect identical mudata function
expect_equal_mudata <- function(md1, md2) {
  # expect names equal
  expect_true(setequal(names(md1), names(md2)))

  # expect attributes equal
  expect_identical(attributes(md1), attributes(md2))

  # expect all table values equal
  for (tbl_name in names(md1)) {
    tbl1 <- md1[[tbl_name]]
    tbl2 <- md2[[tbl_name]]
    expect_equal(colnames(tbl1), colnames(tbl2))
    for (col_name in colnames(tbl1)) {
      col1 <- tbl1[[col_name]]
      col2 <- tbl2[[col_name]]
      if (is.numeric(col1) && is.numeric(col2)) {
        expect_true(all(na.omit(dplyr::near(col1, col2))))
        expect_equal(is.na(col1), is.na(col2))
      } else {
        expect_equal(col1, col2)
      }
    }
  }
}

test_that("mudata_expect_equal works", {
  expect_silent(expect_equal_mudata(kentvillegreenwood, kentvillegreenwood))
})

# create test data
pocmaj_data <- pocmajsum %>%
  dplyr::select(core, depth, Ca, Ti, V) %>%
  tidyr::gather(Ca, Ti, V, key = "param", value = "value") %>%
  dplyr::select(location = core, param, depth, value)
pocmaj_md <- mudata(pocmaj_data)

test_that("read/write zip functions work", {
  outfile <- tempfile(fileext = ".zip")
  write_mudata_zip(kentvillegreenwood, outfile)
  md2 <- read_mudata_zip(outfile)
  expect_s3_class(md2, "mudata")
  expect_true(all(sapply(kentvillegreenwood, nrow) == sapply(md2, nrow)))
  expect_equal_mudata(kentvillegreenwood, md2)
  unlink(outfile)
})

test_that("write zip does not affect working directory", {
  wd <- getwd()
  outfile <- tempfile(fileext = ".zip")
  write_mudata_zip(kentvillegreenwood, outfile)
  expect_equal(getwd(), wd)
  unlink(outfile)
})

# this test fails on check farms because temp files are always
# in flux...
# test_that("read/write zip cleans up temporary files", {
#   tfiles <- list.files(tempdir())
#   outfile <- tempfile(fileext = ".zip")
#   write_mudata_zip(kentvillegreenwood, outfile)
#   read_mudata_zip(outfile)
#   unlink(outfile)
#   expect_true(setequal(tfiles, list.files(tempdir())))
# })

test_that("read_mudata throws an error when file/directory doesn't exist", {
  expect_error(read_mudata("NOT_A_FILE"), "Don't know which format to read file 'NOT_A_FILE'")
})

test_that("write_mudata_dir fails when asked to write to a file", {
  tf <- tempfile()
  file.create(tf)
  expect_error(write_mudata_dir(kentvillegreenwood, tf), "Not a directory:")
  unlink(tf)
})

test_that("possibly not valid JSON objects are generate correct warnings/errors", {
  tf <- tempfile()
  write_mudata_json(kentvillegreenwood, tf)

  json <- jsonlite::read_json(tf)
  json$locations <- 6
  tf2 <- tempfile()
  jsonlite::write_json(json, tf2)
  expect_error(read_mudata_json(tf2), "JSON objects of incorrect type: 'locations'")
  unlink(tf2)

  json <- jsonlite::read_json(tf)
  json$columns <- NULL
  tf2 <- tempfile()
  jsonlite::write_json(json, tf2)
  expect_error(read_mudata_json(tf2), "cannot read JSON to mudata without a columns table")

  unlink(tf)
})

test_that("type strs are guessed when inappropriate columns exist in columns tbl", {
  kg2 <- kentvillegreenwood
  kg2$columns$type <- NULL

  outfile <- tempfile()
  write_mudata_dir(kg2, outfile, update_columns = FALSE)
  kg3 <- read_mudata_dir(outfile)
  expect_equal_mudata(kg2, kg3)
  unlink(outfile, recursive = TRUE)

  kg2$columns <- NULL
  write_mudata_dir(kg2, outfile, update_columns = FALSE, overwrite = TRUE, validate = FALSE)
  kg3 <- read_mudata_dir(outfile)
  kg3$locations$stationid <- as.integer(kg3$locations$stationid)
  kg3$columns$type[kg3$columns$column == "stationid"] <- "integer"
  expect_equal_mudata(kg3, kentvillegreenwood)

  unlink(outfile, recursive = TRUE)
})

test_that("recursive reading is apparent to the user", {
  tf <- tempfile()
  dir.create(tf)
  tf2 <- file.path(tf, "internal_directory")
  write_mudata_dir(kentvillegreenwood, tf2)
  expect_message(read_mudata_dir(tf), "Reading from")
  unlink(tf, recursive = TRUE)
})

test_that("columns table is updated properly", {
  kg2 <- kentvillegreenwood
  kg2$columns[["type"]][1] <- "a_new_type"

  # check that message is produced, and not when quiet = TRUE
  expect_message(
    update_columns_table(kg2),
    paste0(
      "Replacing types ecclimate/data/dataset/character ",
      "with ecclimate/data/dataset/a_new_type"
    )
  )
  expect_silent(update_columns_table(kg2, quiet = TRUE))

  # check that column was updated
  updated <- update_columns_table(kg2)
  expect_identical(kg2$columns$type[-1], updated$columns$type[-1])
  expect_true("a_new_type" %in% kg2$columns$type)
  expect_false("a_new_type" %in% updated$columns$type)

  # check that quiet = TRUE doesn't affect updating
  expect_identical(
    update_columns_table(kg2),
    update_columns_table(kg2, quiet = TRUE)
  )

  # check that updating a columns table that doesn't need upating is quiet
  expect_identical(
    update_columns_table(kentvillegreenwood, quiet = FALSE),
    kentvillegreenwood
  )

  # check that updating a columns table without a type column works
  kg2$columns$type <- NULL
  expect_false("type" %in% colnames(kg2$columns))
  expect_true("type" %in% colnames(update_columns_table(kg2)$columns))
  expect_identical(
    update_columns_table(kg2)$columns %>% dplyr::select(-type),
    kg2$columns
  )
  # should be silent, since no information is replaced
  expect_silent(update_columns_table(kg2, quiet = FALSE))
})

test_that("update_columns_table works when columns are added", {
  kg2 <- kentvillegreenwood
  kg2$data$new_column <- "new_value"
  updated <- update_columns_table(kg2)$columns
  expect_true("new_column" %in% updated$column)
})

test_that("read/write JSON functions work", {
  test_json <- function(md_object, debug = FALSE) {
    outfile <- tempfile(fileext = ".json")
    write_mudata_json(md_object, outfile)
    md2 <- read_mudata_json(outfile)

    if (debug) {
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

test_that("odd json objects throw the appropriate error", {
  expect_error(from_mudata_json('"not a list"'), "not a list")
  expect_error(from_mudata_json("{}"), "missing the data table")
})

test_that("autodetection of read function filename extension works", {
  outfile_json <- tempfile(fileext = ".json")
  write_mudata(kentvillegreenwood, outfile_json)
  expect_true(file.exists(outfile_json))
  expect_s3_class(read_mudata_json(outfile_json), "mudata")
  unlink(outfile_json)

  outfile_zip <- tempfile(fileext = ".zip")
  write_mudata(kentvillegreenwood, outfile_zip)
  expect_true(file.exists(outfile_zip))
  expect_s3_class(read_mudata_zip(outfile_zip), "mudata")
  unlink(outfile_zip)

  outfile_dir <- tempfile()
  expect_message(
    write_mudata(kentvillegreenwood, outfile_dir),
    "Using write_mudata_dir.*"
  )
  expect_s3_class(read_mudata(outfile_dir), "mudata")
  unlink(outfile_dir, recursive = TRUE)
})

test_that("invalid objects are not written", {
  kentvillegreenwood$data <- rbind(kentvillegreenwood$data, kentvillegreenwood$data)
  expect_error(validate_mudata(kentvillegreenwood))
  expect_error(
    write_mudata_json(kentvillegreenwood, tempfile()),
    "Duplicate data in data table"
  )
  expect_error(
    write_mudata_zip(kentvillegreenwood, tempfile()),
    "Duplicate data in data table"
  )
})

test_that("invalid objects are not read", {
  kentvillegreenwood$data <- rbind(kentvillegreenwood$data, kentvillegreenwood$data)
  expect_error(validate_mudata(kentvillegreenwood))
  outfile_json <- tempfile(fileext = ".json")
  outfile_zip <- tempfile(fileext = ".zip")

  write_mudata(kentvillegreenwood, outfile_json, validate = FALSE)
  write_mudata(kentvillegreenwood, outfile_zip, validate = FALSE)

  expect_error(read_mudata(outfile_json), "Duplicate data in data table")
  expect_error(read_mudata(outfile_zip), "Duplicate data in data table")

  unlink(outfile_zip)
  unlink(outfile_json)
})

test_that("retyping on read/write works", {
  outfile <- tempfile(fileext = ".zip")
  data("kentvillegreenwood")
  kg2 <- kentvillegreenwood
  kg2$data$date <- as.POSIXct(kg2$data$date)
  write_mudata_zip(kg2, outfile)

  md2 <- read_mudata_zip(outfile)
  expect_s3_class(md2, "mudata")
  expect_s3_class(md2$data$date, "POSIXct")
  unlink(outfile)
})

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
    c9 = hms::as_hms(1:3)
  )

  # get type strings, expected output classes
  type_strs <- generate_type_tbl(test_df) %>% tibble::deframe()
  output_clases <- vapply(type_strs, mudata2:::parse_output_class, character(1))

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
    c9 = hms::as_hms(1:3)
  )

  prepared <- mudata_prepare_tbl(test_df)
  # the last few columns should be character by default
  expect_type(prepared$c6, "character")
  expect_type(prepared$c7, "character")
  expect_type(prepared$c8, "character")
  expect_type(prepared$c9, "character")

  prepared_json <- mudata_prepare_tbl(test_df, format = "json")
  # the last few columns should be character usually
  expect_type(prepared_json$c6, "character")
  expect_type(prepared_json$c7, "list")
  expect_type(prepared_json$c8, "character")
  expect_type(prepared_json$c9, "character")

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
    c6 = lubridate::force_tz(as.POSIXct(c5), "UTC"),
    c7 = list(list(1), list(2), list(3)),
    c8 = sf::st_as_sfc(c("POINT(0 0)", "POINT(1 1)", "POINT(2 2)")),
    c9 = hms::as_hms(1:3)
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
    if (inherits(val1, "list") || inherits(val2, "list")) {
      # integers/numerics get confused here, so use == and mapply
      if (identical(unclass(val1), unclass(val2))) {
        return(TRUE)
      }
      all_equal <- try(all(mapply(`==`, val1, val2)), silent = TRUE)
      if (identical(all_equal, TRUE)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else if (inherits(val1, "POSIXct") && inherits(val2, "POSIXct")) {
      all_equal <- all(val1 == val2)
      tz1 <- attr(val1, "tzone")
      tz2 <- attr(val2, "tzone")
      if (is.null(tz1) || identical(tz1, "")) {
        if (is.null(tz2) || identical(tz2, "")) {
          tzequal <- TRUE
        } else {
          tzequal <- FALSE
        }
      } else {
        tzequal <- identical(tz1, tz2)
      }
      if (all_equal && !identical(val1, val2)) abort("date vectors not identical")
      all_equal && tzequal
    } else if (inherits(val1, "factor") || inherits(val2, "factor")) {
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
    if (debug) {
      browser()
    }
    dt <- seq(as.POSIXct("1980-01-01 09:00"), as.POSIXct("1980-01-01 17:00"), by = 3600)
    dt <- lubridate::force_tz(dt, tz_name) # version of dt in the object
    dt_utc <- lubridate::with_tz(dt, "UTC") # version of dt we want to write

    expect_length(dt, 9)
    expect_equal(attr(dt, "tzone"), tz_name)

    # make sure prepared version is in UTC
    prepared_dt <- mudata_prepare_column(dt)
    expect_true(all(readr::parse_datetime(prepared_dt) == lubridate::with_tz(dt, "UTC")))

    # make sure times are read in and refer to the same moment in time
    type_str <- generate_type_str(dt)
    parsed_dt <- mudata_parse_column(prepared_dt, type_str = type_str)
    # try equality
    expect_equal(lubridate::with_tz(dt, "UTC"), parsed_dt)
    # check tzone attribute
    expect_equal(attr(parsed_dt, "tzone"), "UTC")
  }

  # test with lots of timezones
  # test_with_tz("") # "unspecified" does not work on all systems in the same way
  test_with_tz("America/Halifax")
  test_with_tz("UTC")
  test_with_tz("America/Boise")
  test_with_tz("Pacific/Auckland")
})

test_that("read/write mudatas containing datetime objects works", {
  slt <- second_lake_temp
  outfile <- tempfile(fileext = ".json")
  expect_silent(write_mudata(slt, outfile))
  expect_equal_mudata(slt, read_mudata(outfile))
  unlink(outfile)

  outfile <- tempfile(fileext = ".json")
  # set timezone attribute to halifax, not changing the moment in time
  slt$data$datetime <- lubridate::with_tz(slt$data$datetime, "America/Halifax")
  expect_message(write_mudata(slt, outfile), "Converting POSIXt column to UTC")
  dt <- read_mudata(outfile) %>%
    tbl_data() %>%
    dplyr::pull(datetime)
  expect_identical(attr(dt, "tzone"), "UTC")
  expect_equal(lubridate::with_tz(slt$data$datetime, "UTC"), dt)
  unlink(outfile)
})

test_that("write directory function doesn't overwrite without permission", {
  outfile <- tempfile()[1]
  write_mudata_dir(kentvillegreenwood, outfile)

  expect_error(
    write_mudata_dir(kentvillegreenwood, outfile),
    "Directory '.*?' exists. Use `overwrite = TRUE` to overwrite."
  )
  expect_silent(write_mudata_dir(kentvillegreenwood, outfile, overwrite = TRUE))
  unlink(outfile, recursive = TRUE)
})

test_that("write json function doesn't overwrite without permission", {
  outfile <- tempfile()[1]
  write_mudata_json(kentvillegreenwood, outfile)

  expect_error(
    write_mudata_json(kentvillegreenwood, outfile),
    "File '.*?' exists. Use `overwrite = TRUE` to overwrite."
  )
  expect_silent(write_mudata_json(kentvillegreenwood, outfile, overwrite = TRUE))
  unlink(outfile)
})

test_that("write zip function doesn't overwrite without permission", {
  outfile <- tempfile(fileext = ".zip")[1]
  write_mudata_zip(kentvillegreenwood, outfile)

  expect_error(
    write_mudata_zip(kentvillegreenwood, outfile),
    "File '.*?' exists. Use `overwrite = TRUE` to overwrite."
  )
  expect_silent(write_mudata_zip(kentvillegreenwood, outfile, overwrite = TRUE))
  unlink(outfile)
})

test_that("read_ functions throw errors when used on odd files", {
  expect_error(read_mudata_dir("not_anything"), "does not exist")
  expect_error(read_mudata_zip("not_anything"), "does not exist")
  expect_error(read_mudata_json("not_anything"), "does not exist")

  empty_file <- tempfile()
  file.create(empty_file)
  expect_error(read_mudata_dir(empty_file), "not a directory")
  unlink(empty_file)

  empty_directory <- tempfile()
  dir.create(empty_directory)
  expect_error(read_mudata_zip(empty_directory), "is a directory")
  expect_error(read_mudata_json(empty_directory), "is a directory")

  unlink(empty_directory, recursive = TRUE)
})

test_that("write functions throw errors when they have insufficient permissions", {
  # these tests are useful for development but aren't needed for CRAN
  skip_on_cran()
  skip_on_os("windows")
  empty_directory <- tempfile()
  dir.create(empty_directory)
  Sys.chmod(empty_directory, mode = "0444")
  expect_error(
    write_mudata_dir(kentvillegreenwood, file.path(empty_directory, "kg.mudata")),
    "Failed to create directory"
  )

  expect_error(
    suppressWarnings(write_mudata_dir(kentvillegreenwood, empty_directory, overwrite = TRUE)),
    "Error writing mudata to CSV"
  )

  expect_error(
    write_mudata_zip(kentvillegreenwood, file.path(empty_directory, "kg.mudata.zip")),
    "exited with status '15'"
  )

  expect_warning(
    expect_error(
      write_mudata_json(kentvillegreenwood, file.path(empty_directory, "kg.mudata.json")),
      "cannot open the connection"
    ),
    "Permission denied"
  )

  unlink(empty_directory)
})

test_that("read/write directory functions work", {
  test_dir <- function(md_object, debug = FALSE) {
    outfile <- tempfile(fileext = ".mudata")
    write_mudata_dir(md_object, outfile)
    md2 <- read_mudata_dir(outfile)

    if (debug) {
      browser()
    }

    # expect identical to original object
    expect_equal_mudata(md_object, md2)

    # cleanup file
    unlink(outfile, recursive = TRUE)
  }

  test_dir(kentvillegreenwood)
  test_dir(subset(kentvillegreenwood, params = c("maxtemp", "mintemp", "meantemp")))
  test_dir(pocmaj_md)
})

test_that("additional tbls can be included in mudata read/write", {
  # create a possible fictional table that might want to be included in a mudata object
  flags_dict <- kentvillegreenwood %>%
    tbl_data() %>%
    dplyr::filter(!is.na(flags)) %>%
    dplyr::distinct(param, flags) %>%
    dplyr::mutate(
      data_number = 1:4, data_date = as.Date("1970-01-01") + 1:4,
      data_chr = c("one", "two", "three", "four")
    )

  kg2 <- kentvillegreenwood
  kg2$flags_dict <- flags_dict
  kg2 <- update_columns_table(kg2)

  outfile <- tempfile(fileext = ".mudata")
  write_mudata_dir(kg2, outfile)
  kg3 <- read_mudata_dir(outfile)
  expect_identical(kg3, kg2)

  outfile_json <- tempfile(fileext = ".json")
  write_mudata_json(kg2, outfile_json)
  kg4 <- read_mudata_json(outfile_json)
  expect_identical(kg4, kg2)
})

test_that("mudata_read guesses column types when columns table is missing", {
  tf <- tempfile()
  write_mudata_dir(ns_climate, tf)
  unlink(file.path(tf, "columns.csv"))
  new_md <- read_mudata_dir(tf)
  expect_s3_class(new_md %>% tbl_data() %>% dplyr::pull(date), "Date")

  unlink(tf, recursive = TRUE)
})

test_that("mudata_read errors when data.csv is missing", {
  tf <- tempfile()
  write_mudata_dir(ns_climate, tf)
  unlink(file.path(tf, "data.csv"))
  expect_error(read_mudata_dir(tf), "'data\\.csv' not found")

  unlink(tf, recursive = TRUE)
})

test_that("when zero x_columns exist on purpose, no message occurs on read", {
  md_zero <- mudata(data.frame(param = c("p1", "p2"), value = c(1, 2)), x_columns = character(0))

  tf_json <- tempfile()
  write_mudata_json(md_zero, tf_json)
  expect_silent(read_mudata_json(tf_json))
  unlink(tf_json)

  # this test fails on CRAN but should be tested locally
  # and on travis
  skip_on_cran()
  tf_dir <- tempfile()
  write_mudata_dir(md_zero, tf_dir)
  expect_silent(read_mudata_dir(tf_dir))
  unlink(tf_dir, recursive = TRUE)
})
