
#' Generate type descriptors
#'
#' These functions generate type descriptors for use in the [mudata]
#' columns table. They are used in [mudata_parse_column] to deserialize
#' a vector read from csv or json.
#'
#' @param x An object
#' @param default The default type descriptor (should usually be 'guess')
#'
#' @return A type descriptor or tibble of type descriptors
#' @keywords internal
#'
generate_type_tbl <- function(x, default = "guess") UseMethod("generate_type_tbl")

#' @rdname generate_type_tbl
generate_type_tbl.mudata <- function(x, default = "guess") {
  # empty columns table
  empty <- tibble::tibble(dataset = character(0), table = character(0),
                          column = character(0))

  if(.isempty(x$data)) {
    return(empty)
  }
  
  # generate a table of all columns from all tables
  # use all datasets, or if there is no datasets table use NA_character_
  if("datasets" %in% names(x)) {
    dataset_ids <- distinct_datasets(x, table = "datasets")
  } else {
    dataset_ids <- NA_character_
  }
  
  # generate all combinations of dataset_ids and table
  # use generate_type_str() to generate column specs
  allcols <- expand.grid(dataset = dataset_ids, table = names(x),
                         stringsAsFactors = FALSE)
  allcols$.specs <- lapply(allcols$table, function(table) {
    generate_type_tbl(x[[table]], default = default)
  })
  
  # unnest the .data column
  if(nrow(allcols) == 0) {
    # no datasets/tbls, empty auto-generated columns table
    columns <- empty
  } else {
    # CMD hack
    .specs <- NULL; rm(.specs)
    columns <- allcols %>% tidyr::unnest(.specs)
  }
  
  # return columns
  columns
}

# generate a type table for a data.frame
#' @rdname generate_type_tbl
generate_type_tbl.tbl <- function(x, default = "guess") {
  df <- x %>% utils::head()
  vapply(df, generate_type_str, default = default, FUN.VALUE = character(1)) %>%
    tibble::enframe(name = "column", value = "type")
}

#' @rdname generate_type_tbl
generate_type_tbl.data.frame <- function(x, default = "guess") {
  generate_type_tbl.tbl(x = x, default = default)
}

# generate a type for an object (e.g. column in data frame)
#' @rdname generate_type_tbl
generate_type_str <- function(x, default = "guess") {
  # list of types
  class_to_type <- c("character" = "character", "factor" = "character",
                     "ordered" = "character",
                     "numeric" = "double", "integer" = "integer",
                     "logical" = "logical", "Date" = "date", 
                     "POSIXct" = "datetime", "POSIXlt" = "datetime",
                     "POSIXt" = "datetime", "hms" = "time", "sfc" = "wkt",
                     "json_column" = "json", "list" = "json")
  
  # check for first class that inherits from the above
  type_string <- default
  for(cls in names(class_to_type)) {
    if(inherits(x, cls)) {
      type_string <- class_to_type[cls]
      break
    }
  }
  
  # type strings for wkt, datetime columns require some information from the object
  if(type_string == "wkt") {
    crs <- sf::st_crs(x)
    if(is.na(crs) || is.null(crs$epsg) || is.na(crs$epsg)) {
      # don't do anything for NA crs or undefined epsg
    } else if(!is.na(crs$epsg)) {
      type_string <- sprintf("wkt(crs=%s)", crs$epsg)
    }
  }
  
  # return type_string unnamed
  stats::setNames(type_string, NULL)
}

#' Describe column types for use in the columns table
#' 
#' Type descriptions are basically calls to `readr::col_*` or 
#' `readr::parse_*`, and can 
#' contain arguments for the sake of completeness (but this shouldn't normally be necessary).
#'
#' @param type_str A type string, one of date, datetime, logical, double, character, guess,
#'   time, integer, wkt, or json.
#'
#' @return A parsed version of the type_str, a column specification or parsing function
#' @keywords internal
#'
#' @examples
#' # mostly type specs are just type names
#' # parse_type("character")
#' # as_col_spec("character")
#' # as_parser("character")
#' 
#' # can also pass arguments if needed
#' # parse_type("datetime(format='%m%.%d%.%Y')")
#' # as_col_spec("datetime(format='%m%.%d%.%Y')")
#' # as_parser("datetime(format='%m%.%d%.%Y')")
#' 
as_col_spec <- function(type_str) {
  # parse, verify type_str
  type_obj <- parse_type(type_str)
  
  if(type_obj$type %in% allowed_types_readr) {
    # get readr funcion and call with type_obj$args
    do.call(get_readr_fun(type_obj$type, "col"), type_obj$args)
  } else {
    # can't do column specification for json or wkt, but these columns should be
    # read as character (ignoring args)
    readr::col_character()
  }
}

#' @rdname as_col_spec
as_parser <- function(type_str) {
  # parse, verify type_str
  type_obj <- parse_type(type_str)
  
  if(type_obj$type %in% allowed_types_readr) {
    # get readr funcion
    parse_fun <- get_readr_fun(type_obj$type, "parse")
  } else if(type_obj$type == "json") {
    parse_fun <- parse_json
  } else if(type_obj$type == "wkt") {
    parse_fun <- parse_wkt
  } else if(type_obj$type == "datetime") {
    parse_fun <- parse_mudata_datetime
  }
  
  # return a partial wrapper using type_obj$args
  function(x) {
    do.call(parse_fun, c(list(quote(x)), type_obj$args))
  }
}

# define allowed types in one place
allowed_types_readr <- c("date", "logical", "double", "character", 
                         "guess", "integer", "time")
allowed_types_extra <- c("wkt", "json", "datetime")

# define output class types from parsing functions
parse_output_class <- function(type_str) {
  # parse type obj
  type_obj <- parse_type(type_str)
  
  output_class_types <- c("date" = "Date", "datetime" = "POSIXct", "logical" = "logical",
                          "double" = "numeric", "character" = "character",
                          "integer" = "integer", "time" = "hms", "wkt" = "sfc", 
                          "json" = "list")
  
  # return with no names
  stats::setNames(output_class_types[type_obj$type], NULL)
}

# custom date parser that allows for a tz argument
parse_mudata_datetime <- function(x, tzone = "UTC", ...) {
  if(identical(tzone, "") || is.null(tzone) || is.na(tzone)) {
    tzone <- "UTC"
  }
  result <- readr::parse_datetime(x, ...)
  # if tzone is not "", pass to lubirdate::force_tz to set timezone
  lubridate::force_tz(result, tzone = tzone)
}


# json parser using jsonlite, simplifying only vectors
parse_json <- function(x, na = c("NA", ""), ...) {
  # use parse_lapply to apply jsonlite::fromJSON to the column
  col <- parse_lapply(x, jsonlite::fromJSON, na = na, simplifyVector = TRUE, 
                      simplifyMatrix = FALSE, simplifyDataFrame = FALSE, ...)
  # give result the class json_column
  class(col) <- c("json_column", "list")
  col
  
}

# wkt parser using sf::st_as_sfc
parse_wkt <- function(x, na = c("NA", ""), crs = sf::NA_crs_, ...) {
  # check if sf is installed, if not, return character vector with a warning
  if(!requireNamespace("sf", quietly = TRUE)) {
    rlang::warn("Package 'sf' required to read wkt columns. Keeping column as is.") # nocov
    return(x) # nocov
  }
  
  # make x a character vector
  x <- as.character(x)
  
  col <- try(sf::st_as_sfc(x, crs = crs, ...), silent = TRUE)
  if(inherits(col, "try-error")) {
    # no way to tell which value was the culprit here, so use parse_wkt_lapply
    parse_wkt_lapply(x, na = na, crs = crs, ...)
  } else {
    col
  }
}

# this parses values one-by-one, which is better for identifying errors
# but is much slower
parse_wkt_lapply <- function(x, na = c("NA", ""), crs = sf::NA_crs_, ...) {
  # make x a character vector
  x <- as.character(x)
  
  # use st_as_sfc to parse WKT from non-NA values
  # NAs are handled by parse_lapply
  col <- parse_lapply(x, function(element) {
    sf::st_as_sfc(element, na = na, ...)[[1]]
  })
  
  # make col an sf::sfc (nulls result in the correct empty geometry as of version 0.5.5)
  col_sfc <- do.call(sf::st_sfc, c(list(col), list(crs = crs)))
  
  # copy attributes and class from col_sfc to col
  parsing_problems <- attr(col, "problems")
  attr(col_sfc, "problems") <- parsing_problems
  
  # return col
  col_sfc
}

parse_lapply <- function(x, fun, na = c("NA", ""), ...) {
  # make x a character vector
  x <- as.character(x)
  
  # safely lapply
  col <- lapply(x, function(element) {
    # NAs become NULL
    if(is.na(element) || (element %in% na)) {
      return(NULL) # literal NULL
    }
    
    # apply fun to elemement
    try(fun(element), silent = TRUE)
  })
  
  # check for problems
  has_error <- vapply(col, inherits, "try-error", FUN.VALUE = logical(1))
  if(any(has_error)) {
    error_rows <- which(has_error)
    error_messages <- vapply(col[has_error], as.character, FUN.VALUE = character(1))
    error_values <- x[has_error]
    
    problems <- tibble::tibble(row = error_rows, col = NA_integer_, 
                               expected = error_messages, actual = error_values)
    attr(col, "problems") <- problems
    warning(sprintf("%s parsing failures in parse_json()", length(error_rows)))
    # error values become NULL
    col[has_error] <- list(NULL)
  }
  
  # return column
  col
}

# there is probably a better way to do this, essentially
# getting readr::prefix_fun
get_readr_fun <- function(fun, prefix) {
  if(prefix == "col") {
    if(fun == "date") {
      readr::col_date
    } else if(fun == "logical") {
      readr::col_logical
    } else if(fun == "double") {
      readr::col_double
    } else if(fun == "character") {
      readr::col_character
    } else if(fun == "guess") {
      readr::col_guess
    } else if(fun == "integer") {
      readr::col_integer
    } else if(fun == "time") {
      readr::col_time
    }
  } else if(prefix == "parse") {
    if(fun == "date") {
      readr::parse_date
    } else if(fun == "logical") {
      readr::parse_logical
    } else if(fun == "double") {
      readr::parse_double
    } else if(fun == "character") {
      readr::parse_character
    } else if(fun == "guess") {
      readr::parse_guess
    } else if(fun == "integer") {
      readr::parse_integer
    } else if(fun == "time") {
      readr::parse_time
    }
  }
}

# type with args: type_name(key='string value', key2=1234.6, key3=1234, key5 = "dq string val")
# type without args: type_name

# first group is type, second optional group are the args
type_regex_args <- "^([a-z][a-z_0-9]+)\\s*\\(\\s*(.*?)\\s*\\)$"
type_regex <- "^([a-z][a-z_0-9]+)$"
# first group is key, second group is value
string_arg_regex <- "([a-z_0-9]+)\\s*=\\s*(['\"])(.*?)\\2(\\s*,?\\s*)"
num_arg_regex <- "([a-z_0-9]+)\\s*=\\s*([0-9.]+)(\\s*,?\\s*)"

# list arguments are a pain, but needed for col_factor() or multiple NA values
list_arg_regex <- "([a-z_0-9]+)\\s*=\\s*\\[(.*?)\\](\\s*,?\\s*)"
numeric_list_item_regex <- "\\s*([0-9.]+)(\\s*,?\\s*)"
string_list_item_regex <- "\\s*(['\"])(.*?)\\1(\\s*,?\\s*)"

#' @rdname as_col_spec
parse_type <- function(type_str) {
  # do base parsing of type_str
  type_obj <- parse_type_base(type_str)
  
  if(!type_obj$type %in% c(allowed_types_extra, allowed_types_readr)) {
    allowed_types <- paste0("'", c(allowed_types_readr, allowed_types_extra), "'", collapse = ", ")
    abort(glue::glue("Type must be one of {allowed_types}"))
  }
  
  # return type obj
  type_obj
} 

parse_type_base <- function(type_str) {
  # check that type_str is a character vector of length 1
  if(!is.character(type_str) || (length(type_str) != 1)) {
    abort("`type_str` must be a character vector of length 1")
  }
  
  # default is a type of character with no arguments
  if(is.na(type_str) || (type_str == "")) {
    list(type = "guess", args = stats::setNames(list(), character(0)))
  } else if(grepl(type_regex_args, type_str)) {
    # extract type and args
    
    type_str_match <- stringr::str_match(type_str, type_regex_args)[1, , drop = TRUE]
    type <- type_str_match[2]
    arg_string <- type_str_match[3]
    
    # match numerics, non-numerics, and lists
    arg_regex <- paste(string_arg_regex, num_arg_regex, list_arg_regex, sep = "|")
    arg_match <- stringr::str_match_all(arg_string, arg_regex)[[1]]
    
    # ensure the entire string has been used
    all_matches <- arg_match[, 1, drop = TRUE]
    if(stringr::str_length(paste(all_matches, collapse = "")) !=
       stringr::str_length(arg_string)) {
      abort(glue::glue("Invalid argument string: '{arg_string}'"))
    }
    
    # check for commas at the end of the string
    if(grepl(",\\s*$", arg_string)) {
      abort(glue::glue("Invalid argument string: '{arg_string}'"))
    }
    
    # check separator values for things that aren't "" or commas
    seps <- dplyr::coalesce(arg_match[, 5, drop = TRUE],
                            arg_match[, 8, drop = TRUE],
                            arg_match[, 11, drop = TRUE])
    bad_seps <- seps[!grepl(",", seps) & (seps != "")]
    if(length(bad_seps) > 0) {
      abort(glue::glue("Invalid argument string: '{arg_string}'"))
    }
    
    # extract names, arguments, which argument type
    arg_names <- dplyr::coalesce(arg_match[, 2, drop = TRUE],
                                 arg_match[, 6, drop = TRUE],
                                 arg_match[, 9, drop = TRUE])
    arg_values <- dplyr::coalesce(arg_match[, 4, drop = TRUE],
                                  arg_match[, 7, drop = TRUE],
                                  arg_match[, 10, drop = TRUE])
    arg_type <- ifelse(!is.na(arg_match[, 7, drop = TRUE]), "numeric", "character")
    arg_type <- ifelse(!is.na(arg_match[, 10, drop = TRUE]), "list", arg_type)
    
    # apply parsing functions to types
    args <- as.list(arg_values) %>% stats::setNames(arg_names)
    args[arg_type == "numeric"] <- lapply(args[arg_type == "numeric"], as.numeric)
    args[arg_type == "list"] <- lapply(args[arg_type == "list"], parse_list_string)
    
    # return type and args
    list(type = type, args = args)
    
  } else if(grepl(type_regex, type_str)) {
    # type_str is type with no args
    list(type = type_str, args = stats::setNames(list(), character(0)))
  } else {
    abort(glue::glue("Invalid type specification: '{type_str}'"))
  }
}

parse_list_string <- function(list_str) {
  # parse list items into an atomic vector
  list_item_regex <- paste(string_list_item_regex, numeric_list_item_regex,
                           sep = "|")
  item_matches <- stringr::str_match_all(list_str, list_item_regex)[[1]]
  
  # ensure the entire item string has been used
  all_items <- item_matches[, 1, drop = TRUE]
  if(stringr::str_length(paste(all_items, collapse = "")) !=
     stringr::str_length(list_str)) {
    abort(glue::glue("Invalid list string: '{list_str}'"))
  }
  
  # check for commas at the end of the string
  if(grepl(",\\s*$", list_str)) {
    abort(glue::glue("Invalid list string: '{list_str}'"))
  }
  
  # check separator values for things that aren't "" or commas
  seps <- dplyr::coalesce(item_matches[, 4, drop = TRUE],
                          item_matches[, 6, drop = TRUE])
  bad_seps <- seps[!grepl(",", seps) & (seps != "")]
  if(length(bad_seps) > 0) {
    abort(glue::glue("Invalid list string: '{list_str}'"))
  }
  
  # extract values, types
  item_values <- dplyr::coalesce(item_matches[, 3, drop = TRUE],
                                 item_matches[, 5, drop = TRUE])
  item_type <- ifelse(!is.na(item_matches[, 5, drop = TRUE]), "numeric", "character")
  
  # apply parsing functions to types
  items <- as.list(item_values)
  items[item_type == "numeric"] <- lapply(items[item_type == "numeric"], as.numeric)
  
  # return arg values as an atomic vector
  unlist(items)
}
