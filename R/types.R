
#' Describe column types for use in the columns table
#' 
#' Type descriptions are basically calls to \code{readr::col_*} or 
#' \code{readr::parse_*}, and can 
#' contain arguments for the sake of completeness (but this shouldn't normally be necessary).
#'
#' @param type_str A type string, one of date, datetime, logical, numeric, character, guess,
#'   geometry, or json.
#'
#' @return A parsed version of the type_str, a column specification or parsing function
#' @export
#'
#' @examples
#' # mostly type specs are just type names
#' parse_type("character")
#' as_col_spec("character")
#' as_parser("character")
#' 
#' # can also pass arguments if needed
#' parse_type("datetime(format='%m%.%d%.%Y')")
#' as_col_spec("datetime(format='%m%.%d%.%Y')")
#' as_parser("datetime(format='%m%.%d%.%Y')")
#' 
as_col_spec <- function(type_str) {
  # parse, verify type_str
  type_obj <- parse_type(type_str)
  
  allowed_types_readr <- c("date", "datetime", "logical", "numeric", "character", 
                           "guess")
  if(type_obj$type %in% allowed_types_readr) {
    # get readr funcion and call with type_obj$args
    do.call(get_readr_fun(type_obj$type, "col"), type_obj$args)
  } else {
    # can't do column specification for json or geometry, but these columns should be
    # read as character (ignoring args)
    readr::col_character()
  }
}

#' @rdname as_col_spec
#' @export
as_parser <- function(type_str) {
  # parse, verify type_str
  type_obj <- parse_type(type_str)
  
  allowed_types_readr <- c("date", "datetime", "logical", "numeric", "character", 
                           "guess")
  if(type_obj$type %in% allowed_types_readr) {
    # get readr funcion
    parse_fun <- get_readr_fun(type_obj$type, "parse")
  } else if(type_obj$type == "json") {
    parse_fun <- parse_json
  } else if(type_obj$type == "geometry") {
    parse_fun <- parse_geometry
  }
  
  # return a partial wrapper using type_obj$args
  function(x) {
    do.call(parse_fun, c(list(x), type_obj$args))
  }
}

# json parser using jsonlite, simplifying only vectors
parse_json <- function(x, na = c("NA", ""), ...) {
  # make x a character vector
  x <- as.character(x)
  
  # safely lapply
  col <- lapply(x, function(element) {
    # NAs become NULL
    if(is.na(element) || (element %in% na)) return(NULL) # literal NULL
    
    try(jsonlite::fromJSON(element, simplifyVector = TRUE, simplifyMatrix = FALSE, 
                           simplifyDataFrame = FALSE, ...), silent = TRUE)
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
    col[has_error] <- as.list(error_values)
  }
  
  # return column
  col
}

# geometry parser using sf::st_as_sfc
parse_geometry <- function(x, ...) {
  col <- try(sf::st_as_sfc(x, ...), silent = TRUE)
  if(inherits(col, "try-error")) {
    # no way to tell which value was the culprit here
    warning("parse_geometry() failed: ", as.character(col))
    x
  } else {
    col
  }
}

# there is probably a better way to do this, essentially
# getting readr::prefix_fun
get_readr_fun <- function(fun, prefix) {
  if(prefix == "col") {
    if(fun == "date") {
      readr::col_date
    } else if(fun == "datetime") {
      readr::col_datetime
    } else if(fun == "logical") {
      readr::col_logical
    } else if(fun == "numeric") {
      readr::col_number
    } else if(fun == "character") {
      readr::col_character
    } else if(fun == "guess") {
      readr::col_guess
    }
  } else if(prefix == "col") {
    if(fun == "date") {
      readr::parse_date
    } else if(fun == "datetime") {
      readr::parse_datetime
    } else if(fun == "logical") {
      readr::parse_logical
    } else if(fun == "numeric") {
      readr::parse_number
    } else if(fun == "character") {
      readr::parse_character
    } else if(fun == "guess") {
      readr::parse_guess
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
#' @export
parse_type <- function(type_str) {
  # do base parsing of type_str
  type_obj <- parse_type_base(type_str)
  
  # check that type is in the list of allowed types
  allowed_types_readr <- c("date", "datetime", "logical", "numeric", "character", 
                           "guess")
  allowed_types_extra <- c("geometry", "json")
  if(!type_obj$type %in% c(allowed_types_extra, allowed_types_readr)) {
    stop("Type must be one of ", 
         paste(c(allowed_types_readr, allowed_types_extra), collapse = ", "))
  }
  
  # return type obj
  type_obj
} 

parse_type_base <- function(type_str) {
  # check that type_str is a character vector of length 1
  if(!is.character(type_str) || (length(type_str) != 1)) {
    stop("type_str must be a character vector of length 1")
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
      stop("Invalid argument string: ", arg_string)
    }
    
    # check for commas at the end of the string
    if(grepl(",\\s*$", arg_string)) stop("Invalid argument string: ", arg_string)
    
    # check separator values for things that aren't "" or commas
    seps <- dplyr::coalesce(arg_match[, 5, drop = TRUE],
                            arg_match[, 8, drop = TRUE],
                            arg_match[, 11, drop = TRUE])
    bad_seps <- seps[!grepl(",", seps) & (seps != "")]
    if(length(bad_seps) > 0) stop("Invalid argument string: ", arg_string)
    
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
    stop("Invalid type specification: ", type_str)
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
    stop("Invalid list string: ", list_str)
  }
  
  # check for commas at the end of the string
  if(grepl(",\\s*$", list_str)) stop("Invalid list string: ", list_str)
  
  # check separator values for things that aren't "" or commas
  seps <- dplyr::coalesce(item_matches[, 4, drop = TRUE],
                          item_matches[, 6, drop = TRUE])
  bad_seps <- seps[!grepl(",", seps) & (seps != "")]
  if(length(bad_seps) > 0) stop("Invalid list string: ", list_str)
  
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
