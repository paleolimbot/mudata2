
# types are basically calls to readr::col_* or readr::parse_*, and can 
# contain arguments for the sake of completeness (but shouldn't normally)
# this parsing is basic and doesn't account for things like escaping

# type with args: type_name(key='string value', key2=1234.6, key3=1234, key5 = "dq string val")
# type without args: type_name

# first group is type, second optional group are the args
type_regex_args <- "^([a-z][a-z_0-9]+)\\s*\\(\\s*(.*?)\\s*\\)$"
type_regex <- "^([a-z][a-z_0-9]+)$"
type_regex_noargs <- "^([a-z][a-z_0-9]+)\\(\\)$"
# first group is key, second group is value
string_arg_regex <- "([a-z_0-9]+)\\s*=\\s*(['\"])(.*?)\\2(\\s*,?\\s*)"
num_arg_regex <- "([a-z_0-9]+)\\s*=\\s*([0-9.]+)(\\s*,?\\s*)"

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
    
    # match numerics and non-numerics
    string_arg_match <- stringr::str_match_all(arg_string, string_arg_regex)[[1]]
    numeric_arg_match <- stringr::str_match_all(arg_string, num_arg_regex)[[1]]
    
    # ensure the entire string has been used
    all_matches <- c(string_arg_match[, 1, drop = TRUE], c(numeric_arg_match[, 1, drop = TRUE]))
    if(stringr::str_length(paste(all_matches, collapse = "")) !=
       stringr::str_length(arg_string)) {
      stop("Invalid argument string: ", arg_string)
    }
    
    # check for commas at the end of the string
    if(grepl(",\\s*$", arg_string)) stop("Invalid argument string: ", arg_string)
    
    # check separator values for things that aren't "" or commas
    seps <- c(string_arg_match[, 5, drop = TRUE], c(numeric_arg_match[, 4, drop = TRUE]))
    bad_seps <- seps[!grepl(",", seps) & (seps != "")]
    if(length(bad_seps) > 0) stop("Invalid argument string: ", arg_string)
    
    # extract string arguments
    . <- NULL; rm(.)
    string_args <- string_arg_match %>% 
      .[,c(2, 4), drop = FALSE] %>% tibble::as_tibble() %>%
      tibble::deframe()
    
    # extract numeric arguments
    V2 <- NULL; rm(V2)
    numeric_args <- numeric_arg_match %>% 
      .[,c(2, 3), drop = FALSE] %>% tibble::as_tibble() %>%
      dplyr::mutate(V2 = as.numeric(V2)) %>%
      tibble::deframe()
    
    # return type and args
    list(type = type, args = c(as.list(string_args), as.list(numeric_args)))
    
  } else if(grepl(type_regex_noargs, type_str)) {
    # type_str first group is type with no args
    list(type = stringr::str_match(type_str, type_regex_noargs)[, 2, drop = TRUE],
         args = list())
  } else if(grepl(type_regex, type_str)) {
    # type_str is type with no args
    list(type = type_str, args = list())
  } else {
    stop("Invalid type specification: ", type_str)
  }
}
