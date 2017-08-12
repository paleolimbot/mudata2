
as_col_spec <- function(type_str) {
  
}

as_parser <- function(type_str) {
  
}


# types are basically calls to readr::col_* or readr::parse_*, and can 
# contain arguments for the sake of completeness (but shouldn't normally)
# this parsing is basic and doesn't account for things like escaping ' or "

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

parse_type <- function(type_str) {
  # check that type_str is a character vector of length 1
  if(!is.character(type_str) || (length(type_str) != 1)) {
    stop("type_str must be a character vector of length 1")
  }
  
  if(grepl(type_regex_args, type_str)) {
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
