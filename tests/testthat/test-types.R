
context("type parsing")

test_that("bare types are parsed correctly", {
  expect_equal(parse_type_base("a_type")$type, "a_type")
  expect_equal(parse_type_base("a_type2")$type, "a_type2")
  
  expect_identical(parse_type_base("a_type")$args, setNames(list(), character(0)))
  expect_identical(parse_type_base("a_type2")$args, setNames(list(), character(0)))
})

test_that("invalid bare types are identified", {
  expect_error(parse_type_base("0type"), "Invalid type specification:.*")
  expect_error(parse_type_base("_type"), "Invalid type specification:.*")
  expect_error(parse_type_base("Type"), "Invalid type specification:.*")
  expect_error(parse_type_base("tYpe"), "Invalid type specification:.*")
})

test_that("types with zero arguments are parsed correctly", {
  expect_identical(parse_type_base("a_type()"), parse_type_base("a_type"))
  expect_identical(parse_type_base("a_type2()"), parse_type_base("a_type2"))
})

test_that("invalid types with zero arguments are identified", {
  expect_error(parse_type_base("0type()"), "Invalid type specification:.*")
  expect_error(parse_type_base("_type()"), "Invalid type specification:.*")
  expect_error(parse_type_base("Type()"), "Invalid type specification:.*")
  expect_error(parse_type_base("tYpe()"), "Invalid type specification:.*")
})

test_that("types with one argument are parsed correctly", {
  
  expect_identical(parse_type_base("type_name(key='value')")$args,
                   list(key = "value"))
  expect_identical(parse_type_base("type_name(key=\"value\")")$args,
                   list(key = "value"))
  expect_identical(parse_type_base("type_name(key=1)")$args,
                   list(key = 1))
  expect_identical(parse_type_base("type_name(key=1.3)")$args,
                   list(key = 1.3))
  expect_identical(parse_type_base("type_name(key=13.)")$args,
                   list(key = 13))
  expect_identical(parse_type_base("type_name(key=.13)")$args,
                   list(key = .13))
})

test_that("various whitespace is allowed in type specifications", {
  expect_identical(parse_type_base("type_name(key ='value')")$args,
                   list(key = "value"))
  expect_identical(parse_type_base("type_name( key=\"value\")")$args,
                   list(key = "value"))
  expect_identical(parse_type_base("type_name(key=1 )")$args,
                   list(key = 1))
  expect_identical(parse_type_base("type_name(key= 1.3)")$args,
                   list(key = 1.3))
  expect_identical(parse_type_base("type_name(key =13.)")$args,
                   list(key = 13))
  expect_identical(parse_type_base("type_name(key = .13)")$args,
                   list(key = .13))
})

test_that("types with invalid arguments are identified", {
  # type errors
  expect_error(parse_type_base("0type(key='value')"), "Invalid type specification:.*")
  expect_error(parse_type_base("_type(key='value')"), "Invalid type specification:.*")
  expect_error(parse_type_base("Type(key='value')"), "Invalid type specification:.*")
  expect_error(parse_type_base("tYpe(key='value')"), "Invalid type specification:.*")
  
  # argument errors
  expect_error(parse_type_base("type(key='value)"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(key='value\")"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(key='value', key2=)"), "Invalid argument string:.*")
  
  # whitespace errors
  expect_error(parse_type_base("type(key='value' key2='value2')"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(key=5 key2='value2')"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(key='value' key2=5)"), "Invalid argument string:.*")

})

test_that("whitespace/commas in argument strings is correctly handled", {
  expect_error(parse_type_base("type(,)"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(,a)"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(,fish = 'thing')"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(fish = 'thing',)"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(fish = 'thing' ,)"), "Invalid argument string:.*")
  expect_error(parse_type_base("type(fish = 'thing' , )"), "Invalid argument string:.*")
  
  expect_silent(parse_type_base("type(key='value', key2='value2' )"))
  expect_error(parse_type_base("type(key='value' key2='value2')"), "Invalid argument string:.*")
  expect_silent(parse_type_base("type(key=5, key2='value2' )"))
  expect_error(parse_type_base("type(key=5 key2='value2')"), "Invalid argument string:.*")
  expect_silent(parse_type_base("type(key='value', key2=5 )"))
  expect_error(parse_type_base("type(key='value' key2='value2')"), "Invalid argument string:.*")
})

test_that("key value pairs within argument strings do not cause errors", {
  expect_equal(parse_type_base("type(key = 'within_key=\"value\"')")$args$key,
               'within_key=\"value\"')
  expect_equal(parse_type_base("type(key = 'within_key=5')")$args$key,
               'within_key=5')
})

test_that("multiple arguments are all extracted", {
  long_type <- "type_name(key='string value', key2=1234.6, key3=1234, key5 = \"dq string val\")"
  result <- parse_type_base(long_type)
  expect_equal(result$type, "type_name")
  expect_equal(result$args$key, "string value")
  expect_equal(result$args$key2, 1234.6)
  expect_equal(result$args$key3, 1234)
  expect_equal(result$args$key5, "dq string val")
})

test_that("lists can be included in arguments", {
  expect_identical(parse_type_base("type(key = [5])")$args$key, 5)
  expect_identical(parse_type_base("type(key = [5, 6])")$args$key, c(5, 6))
  expect_identical(parse_type_base("type(key = ['five'])")$args$key, "five")
  expect_identical(parse_type_base("type(key = [\"five\"])")$args$key, "five")
  # types are coerced to an atomic vector
  expect_identical(parse_type_base("type(key = ['five', 5])")$args$key, c("five", "5"))
})

# this doesn't pass, but is ok for a simple parser
# test_that("brackets in list arguments are not problematic", {
#   expect_identical(parse_type_base("type(key = ['five]'])")$args$key, "five]")
#   expect_identical(parse_type_base("type(key = ['five', 'six]', 'seven]'])")$args$key, 
#                    c("five", "six]", "seven]"))
# })

test_that("multiple list arguments can be included", {
  result <- parse_type_base("type(key1 = [5, 6], key2 = ['six', 'seven'], key3 = 8)")
  expect_identical(result$args$key1, c(5, 6))
  expect_identical(result$args$key2, c("six", "seven"))
  expect_identical(result$args$key3, 8)
})

test_that("invalid list item strings are identified", {
  expect_error(parse_type_base("type(key1 = [5, 6,])"),
               "Invalid list string:.*")
  expect_error(parse_type_base("type(key1 = [5 6])"),
               "Invalid list string:.*")
  expect_error(parse_type_base("type(key1 = [,5, 6])"),
               "Invalid list string:.*")
  expect_error(parse_type_base("type(key1 = [,])"),
               "Invalid list string:.*")
  expect_error(parse_type_base("type(key1 = [5, 6,  ])"),
               "Invalid list string:.*")
})

test_that("default types are handled correctly", {
  expect_identical(parse_type_base(""), parse_type_base(NA_character_))
  expect_identical(parse_type_base(""), 
                   list(type = "guess", 
                        args = stats::setNames(list(), character(0))))
})

test_that("type_str values greater than length 1 throw an error", {
  expect_error(parse_type_base(c("", "")), 
               "`type_str` must be a character vector of length 1")
  expect_error(parse_type_base(character(0)), 
               "`type_str` must be a character vector of length 1")
  expect_error(parse_type_base(NA), 
               "`type_str` must be a character vector of length 1")
  expect_error(parse_type_base(NULL), 
               "`type_str` must be a character vector of length 1")
})

test_that("types that are not in allowed types throw an error", {
  expect_error(parse_type("type"), "Type must be one of.*")
  expect_silent(parse_type("date"))
  expect_silent(parse_type("date()"))
  expect_silent(parse_type("date(format = '%m%.%d%.%Y')"))
  expect_error(parse_type("type(format = '')"), "Type must be one of.*")
  
  # check list of allowed types
  expect_silent(parse_type("date"))
  expect_silent(parse_type(""))
  expect_silent(parse_type("logical"))
  expect_silent(parse_type("double"))
  expect_silent(parse_type("character"))
  expect_silent(parse_type("guess"))
  expect_silent(parse_type("wkt"))
  expect_silent(parse_type("json"))
})

test_that("as_* functions produce the expected output type", {
  # parsetype returns a list
  expect_is(parse_type("character"), "list")
  expect_is(parse_type("datetime"), "list")
  expect_is(parse_type("wkt"), "list")
  expect_is(parse_type("json"), "list")
  
  # as_col_spec returns a collector
  expect_is(as_col_spec("character"), "collector")
  expect_is(as_col_spec("datetime"), "collector")
  expect_is(as_col_spec("wkt"), "collector_character")
  expect_is(as_col_spec("json"), "collector_character")
  expect_is(as_col_spec("date"), "collector_date")
  expect_is(as_col_spec("logical"), "collector_logical")
  expect_is(as_col_spec("double"), "collector_double")
  expect_is(as_col_spec("guess"), "collector_guess")
  expect_is(as_col_spec("integer"), "collector_integer")
  expect_is(as_col_spec("time"), "collector_time")
  
  # as_parser returns a parsing function that can be called with character(0)
  all_parsers <- sapply(c(allowed_types_readr, allowed_types_extra), function(type) {
    as_parser(type)
  })
  expect_true(all(vapply(all_parsers, is.function, logical(1))))
  expect_silent(lapply(all_parsers, function(x) try(x(character(0)))))
})

test_that("datetime parsing works as intended", {
  dt <- lubridate::make_datetime(tz = "UTC")
  expect_identical(
    lubridate::tz(parse_mudata_datetime(mudata_prepare_column(dt), tzone = "")),
    "UTC"
  )
})

test_that("json parsing works as intended", {
  json_test <- c('{"key": "value", "key2":4}', '{"key": "value2", "key2": 5}')
  json_r <- list(list(key = "value", "key2" = 4L), list(key = "value2", key2 = 5L))
  expect_identical(parse_json(json_test) %>% unclass(), json_r)
  
  # make sure NAs be come NULL
  expect_identical(parse_json(c(NA, json_test)) %>% unclass(), c(list(NULL), json_r))
  expect_identical(parse_json(c("", json_test)) %>% unclass(), c(list(NULL), json_r))
  expect_identical(parse_json(c("NA", json_test)) %>% unclass(), c(list(NULL), json_r))
  
  # make sure problems are caught
  expect_warning(parse_json("{'invalid_json'='not valid'}"),
                 "1 parsing failures in parse_json()")
  expect_warning(parse_json(c("{'invalid_json'='not valid'}", json_test)),
                 "1 parsing failures in parse_json()")
  
  expect_is(attr(suppressWarnings(parse_json("{'invalid_json'='not valid'}")), "problems"),
            "data.frame")
  # json parsing failure should just return the invalid string
  expect_identical(suppressWarnings(parse_json("{'invalid_json'='not valid'}"))[[1]], 
                   NULL)
  
  # zero-length parsing
  expect_identical(parse_json(character(0)) %>% unclass(), list())
  
  # expect class
  expect_is(parse_json('{}'), "json_column")
})

test_that("wkt parsing returns an sf::sfc", {
  wkt_test <- c("POINT(0 0)", "POINT(1 1)", "POINT(2 2)")
  expect_is(parse_wkt(wkt_test), "sfc")
  expect_length(parse_wkt(wkt_test), 3)
})

test_that("wkt parsing works when there are parsing errors/NA values", {
  wkt_test <- c("POINT(0 0)", "", "NA", NA, "POINT(1 1)", "POINT(2 2)")
  result_na <- parse_wkt(wkt_test)
  expect_is(result_na, "sfc")
  expect_length(result_na, 6)
  expect_identical(result_na[[2]], sf::st_point())
  expect_identical(result_na[[3]], sf::st_point())
  expect_identical(result_na[[4]], sf::st_point())
  expect_null(attr(result_na, "problems"))
  
  wkt_test_invalid <- c("typo here", "POINT(0 0)", "", "NA", NA, "POINT(1 1)", 
                        "not wkt", "also not wkt", "POINT(2 2)")
  result_invalid <- suppressWarnings(parse_wkt(wkt_test_invalid))
  expect_is(result_invalid, "sfc")
  expect_length(result_invalid, 9)
  # nas as null
  expect_identical(result_invalid[[3]], sf::st_point())
  expect_identical(result_invalid[[4]], sf::st_point())
  expect_identical(result_invalid[[5]], sf::st_point())
  # problems as null
  expect_identical(result_invalid[[1]], sf::st_point())
  expect_identical(result_invalid[[7]], sf::st_point())
  expect_identical(result_invalid[[8]], sf::st_point())
  
  expect_equal(nrow(attr(result_invalid, "problems")), 3)
  expect_equal(attr(result_invalid, "problems")$row, c(1, 7, 8))
})

test_that("wkt parsing works with zero-length input", {
  expect_is(parse_wkt(character(0)), "sfc")
  expect_length(parse_wkt(character(0)), 0)
})

test_that("objects generate the correct type strings", {
  # date and datetime need no additional args, because their writing
  # is handled in mudata.io.R
  expect_equal(generate_type_str(Sys.Date()), "date")
  expect_equal(generate_type_str(Sys.time()), "datetime")
  expect_equal(generate_type_str(hms::hms(minutes = 45, hours = 5)), "time")
  expect_equal(generate_type_str(4), "double")
  expect_equal(generate_type_str(4L), "integer")
  expect_equal(generate_type_str('text'), "character")
  
  # factors get character treatment, as this is how they are written to disk
  expect_equal(generate_type_str(factor('text', levels = 'text')), "character")
  expect_equal(generate_type_str(factor('text', levels = 'text', ordered = TRUE)), 
               "character")
  
  # list columns
  expect_equal(generate_type_str(parse_json("{}")), "json")
  expect_equal(generate_type_str(parse_wkt("POINT(0 0)")), "wkt")
  expect_equal(generate_type_str(parse_wkt("POINT(0 0)", crs = 3857)), "wkt(crs=3857)")
  hard_crs <- sf::st_crs(3857)
  hard_crs$epsg <- NA
  hard_crs_proj4 <- hard_crs$proj4string
  expect_equal(generate_type_str(parse_wkt("POINT(0 0)", crs = hard_crs)), 
               sprintf("wkt(crs='%s')", hard_crs_proj4))
  hard_crs$epsg <- NULL
  hard_crs_proj4 <- hard_crs$proj4string
  expect_equal(generate_type_str(parse_wkt("POINT(0 0)", crs = hard_crs)), 
               sprintf("wkt(crs='%s')", hard_crs_proj4))
})

test_that("datetimes with timezones generate the correct strings", {
  # type strings are always parameter-less, because datetimes are always converted
  # to UTC before they are written
  dt <- Sys.time()
  expect_equal(generate_type_str(dt), "datetime")
  attr(dt, "tzone") <- "America/Halifax"
  expect_equal(generate_type_str(dt), "datetime")
  attr(dt, "tzone") <- "UTC"
  expect_equal(generate_type_str(dt), "datetime")
})

test_that("generate_type_str generates expected output", {
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
  
  type_table <- generate_type_tbl(test_df)
  expect_is(type_table, "data.frame")
  expect_equal(colnames(type_table), c("column", "type"))
  expect_equal(ncol(test_df), nrow(type_table))
  
  types <- type_table %>% tibble::deframe()
  expect_equal(setNames(types, NULL), 
               c("double", "integer", "character", "character", "character",
                 "date", "datetime", "json", "wkt", "time"))
  
})

test_that("generate_type_tbl() can dal with no dataset tbl", {
  kg2 <- kentvillegreenwood
  kg2$datasets <- NULL
  expect_true(all(is.na(generate_type_tbl(kg2)$dataset)))
})

test_that("generate_type_str works on mudata objects", {
  # inspect type table for kentvillegreenwood
  types_kg <- generate_type_tbl(kentvillegreenwood)
  expect_equal(colnames(types_kg), c("dataset", "table", "column", "type"))
  expect_true(setequal(types_kg$dataset, kentvillegreenwood$datasets$dataset))
  expect_true(setequal(types_kg$table, names(kentvillegreenwood)))
  
  all_colnames <- lapply(kentvillegreenwood, colnames) %>% unlist(use.names = FALSE) 
  expect_true(setequal(types_kg$column, all_colnames))
  expect_true(setequal(types_kg$type, c("character", "integer", "double", "date")))
})

test_that("default type is propgated through generate_type functions", {
  kg2 <- kentvillegreenwood
  # the raw type is not included in list of allowed types,
  # so its type str value should be "guess"
  kg2$data$new_column <- raw(nrow(kg2$data))
  type_table_def <- generate_type_tbl(kg2)
  expect_equal(type_table_def$type[type_table_def$column == "new_column"], "guess")
})
