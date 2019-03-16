
#' Biplot a parameter-long data frame
#' 
#' Use either the ggplot framework (autobiplot) or base plotting to biplot a 
#' parameter-long data frame, like that of the data table in a \link{mudata} object.
#' 
#' @param x the object to biplot
#' @param id_vars the columns that identify a single row in x
#' @param name_var The column where names_x and names_y are to be found
#' @param names_x The names to be included in the x axes, or all the names to be included
#' @param names_y The names to be included on the y axes, or NULL for all possible combinations
#'   of \code{namesx}.
#' @param measure_var The column containing the values to plot
#' @param error_var The column containing values for error bars (plus or minus error_var).
#' @param labeller The labeller to use to label facets (may want to use \code{label_parsed}
#'   to use plotmath-style labels)
#' @param validate Ensure id_vars identify unique rows
#' @param max_names When guessing which parameters to biplot/pair, use only the first
#'   max_names (or FALSE to use all names)
#' @param na.rm Should NA values in measure_var be removed?
#' @param ... passed to \code{aes_string()}
#' 
#' @importFrom stats biplot
#' @export
#' @examples 
#' library(tidyr)
#' library(dplyr)
#' 
#' # create a long, summarised representation of pocmaj data
#' pocmaj_long <- pocmajsum %>%
#'   select(core, depth, Ca, Ti, V) %>%
#'   gather(Ca, Ti, V, key = "param", value = "value")
#' 
#' # biplot using base plotting
#' long_biplot(pocmaj_long, id_vars = c("core", "depth"), name_var = "param")
#' 
#' # biplot using ggplot
#' autobiplot(pocmaj_long, id_vars = c("core", "depth"), name_var = "param")
#' 
#' # get the raw data used
#' long_pairs(pocmaj_long, id_vars = c("core", "depth"), name_var = "param")
#'
long_pairs <- function(x, id_vars, name_var, names_x = NULL, 
                       names_y = NULL, validate = TRUE, max_names = 5) {
  
  # CMD hack for dplyr names
  .name <- NULL; rm(.name); . <- NULL; rm(.); .name_x <- NULL; rm(.name_x)
  .name_y <- NULL; rm(.name_y)
  
  # id_vars shouldn't contain name_var
  id_vars <- setdiff(id_vars, name_var)
  
  # check columns
  .checkcols(x, "x", c(id_vars, name_var))
  
  # check that c(id_vars, name_var) identify unique values
  if(validate) {
    .checkunique(x, "x", id_vars, name_var)
  }
  
  # make data with known name column
  data <- x %>% 
    dplyr::rename(.name = !!name_var)
  
  # make a list of parameter names
  all_params <- dplyr::ungroup(data) %>%
    dplyr::distinct(.name) %>% 
    dplyr::collect() %>% 
    dplyr::pull(.name) %>%
    as.character()
  
  # make name combinations
  if(is.null(names_x) && is.null(names_y)) {
    # check that all_params isn't too long
    if(!identical(max_names, FALSE) && (length(all_params) > max_names)) {
      all_params <- all_params[1:max_names]
      message(sprintf("Only using first %s names. ", max_names),
              "Use max_names = FALSE to use all combinations of names.")
    }
    
    # all combinations of all_params
    name_x <- rev(all_params[1:length(all_params)-1])
    name_y <- rev(all_params[2:length(all_params)])
    
    message("Using names_x = c(", 
            paste0('"', name_x, '"', collapse = ", "), 
            "), names_y = c(", 
            paste0('"', name_y, '"', collapse = ", "), 
            ")")
    
    # use unique_combinations to generate matches, such that none are duplicated
    name_combinations <- unique_combinations(all_params)
  } else if(is.null(names_y)) {
    # check for missing names
    missing_names <- setdiff(names_x, all_params)
    if(length(missing_names) > 0) stop("The following names were missing from ", name_var, ": ",
                                       paste(missing_names, collapse = ", "))
    # all combinations of names_x
    name_x <- rev(names_x[1:length(names_x)-1])
    name_y <- rev(names_x[2:length(names_x)])
    
    message("Using names_x = c(", 
            paste0('"', name_x, '"', collapse = ", "), 
            "), names_y = c(", 
            paste0('"', name_y, '"', collapse = ", "), 
            ")")
    
    # use unique_combinations to generate matches, such that none are duplicated
    name_combinations <- unique_combinations(names_x) %>% tibble::as_tibble()
  } else {
    # check for missing names
    missing_names <- setdiff(c(names_x, names_y), all_params)
    if(length(missing_names) > 0) stop("The following names were missing from ", name_var, ": ",
                                       paste(missing_names, collapse = ", "))
    
    # combinations are already specified
    name_x <- names_x
    name_y <- names_y
    
    # use expand.grid to generate name combinations
    name_combinations <- expand.grid(.name_x = name_x, .name_y = name_y, 
                                     stringsAsFactors = FALSE) %>%
      # don't include combinations with the same parameter
      dplyr::filter(.name_x != .name_y) %>%
      tibble::as_tibble()
  }
  
  # use name combinations to filter and join data, rbinding into a single long
  # data frame at the end
  name_combinations$.data <- lapply(seq_len(nrow(name_combinations)), function(i) {
    combination <- name_combinations[i,]
    # filter data to get data_x and data_y
    data_x <- dplyr::ungroup(data) %>% dplyr::filter(.name == !!combination$.name_x) %>% dplyr::collect()
    data_y <- dplyr::ungroup(data) %>% dplyr::filter(.name == !!combination$.name_y) %>% dplyr::collect()
    # join using join_vars
    data_both <- dplyr::inner_join(data_x, data_y,
                                   by = id_vars, suffix = c("_x", "_y"))
    
    # return data_both
    data_both
  })
  data_pairs <- dplyr::bind_rows(name_combinations$.data)
  
  # make name_x and name_y factors with the levels specified
  data_pairs$.name_x <- factor(data_pairs$.name_x, levels = name_x)
  data_pairs$.name_y <- factor(data_pairs$.name_y, levels = name_y)
  
  # return data_both as a tibble
  tibble::as_tibble(data_pairs)
}

#' @rdname long_pairs
#' @export
long_biplot <- function(x, id_vars, name_var, measure_var = "value", 
                        names_x = NULL, na.rm = FALSE, max_names = 5, ...) {
  
  # CMD hack for dplyr names
  .name <- NULL; rm(.name); .value <- NULL; rm(.value)
  
  # id_vars shouldn't contain name_var
  id_vars <- setdiff(id_vars, name_var)
  
  # check columns
  .checkcols(x, "x", c(id_vars, name_var, measure_var))
  
  # rename value and name column
  x <- x %>%
    dplyr::rename(.name = !!name_var, .value = !!measure_var)
  
  # filter to name %in% names_x
  if(!is.null(names_x)) {
    x <- x %>%
      dplyr::filter(.name %in% names_x)
  }
  
  # remove nas if specified
  if(na.rm) {
    x <- x %>%
      dplyr::filter(!is.na(.value))
  }
  
  # collect data
  x <- dplyr::collect(x)
  
  # check for no data
  if(.isempty(x)) {
    stop("Zero rows were found for names_x = c(", 
         paste0('"', names_x, '"', collapse = ", "),
         ")")
  }
  
  # use spread to make a wide data frame
  x_wide <- x %>%
    dplyr::select(dplyr::one_of(c(id_vars, ".name", ".value"))) %>%
    tidyr::spread(.name, .value) %>%
    dplyr::select(-dplyr::one_of(id_vars))
  
  # check that there aren't an unreasonable number of parameters
  if(!identical(max_names, FALSE) && (ncol(x_wide) > max_names)) {
    x_wide <- x_wide[, 1:max_names, drop = FALSE]
    message(sprintf("Only using first %s names. ", max_names),
            "Use max_names = FALSE to use all combinations of names.")
  }
  
  # use graphics::plot to produce the biplot
  graphics::plot(x_wide, ...)
}

#' @rdname long_pairs
#' @export
autobiplot <- function(x, ...) UseMethod("autobiplot")

#' @rdname long_pairs
#' @export
autobiplot.data.frame <- function(x, id_vars, name_var, measure_var = "value", names_x = NULL, 
                                  names_y = NULL, error_var = NULL, na.rm = TRUE, validate = TRUE,
                                  max_names = 5, labeller = ggplot2::label_value, ...) {
  
  # CMD hack for dplyr names
  .value <- NULL; rm(.value)
  
  # id_vars shouldn't contain name_var
  id_vars <- setdiff(id_vars, name_var)
  
  # check columns
  .checkcols(x, "x", c(id_vars, name_var, measure_var))
  
  # rename value name and error column
  if(is.null(error_var)) {
    x <- x %>%
      dplyr::rename(.name = !!name_var, .value = !!measure_var)
  } else {
    x <- x %>%
      dplyr::rename(.name = !!name_var, .value = !!measure_var, 
                    .error = !!error_var)
  }
  
  # remove nas if specified
  if(na.rm) {
    x <- x %>%
      dplyr::filter(!is.na(.value))
  }
  
  # create data pairs
  data_pairs <- long_pairs(x, id_vars = id_vars, name_var = ".name",
                           names_x = names_x, names_y = names_y, validate = validate,
                           max_names = max_names)
  
  # assign error geometry, if error column is specified
  ggerror <- NULL
  if(!is.null(error_var)) {
    ggerror <- list(
      ggplot2::geom_errorbar(
        ggplot2::aes_string(ymax=".value_y+.error_y", ymin=".value_y-.error_y")),
      ggplot2::geom_errorbarh(
        ggplot2::aes_string(xmax=".value_x+.error_x", xmin=".value_x-.error_x"))
    )
  }
  
  # construct ggplot
  ggplot2::ggplot(data_pairs, ggplot2::aes_string(x=".value_x", y=".value_y", ...)) + 
    ggplot2::geom_point() + 
    ggerror +
    ggplot2::facet_grid(.name_y~.name_x, scales="free", labeller = labeller) +
    ggplot2::labs(x=NULL, y=NULL)
}

#' Biplot a mudata object
#' 
#' Uses \link{autobiplot} and \link{long_biplot} to produce parameter vs.
#' parameter plots contained in a \link{mudata} object.
#' 
#' @param x A mudata object
#' @param ... passed to plotting methods
#'   
#' @return A \link[ggplot2]{ggplot} object (autobiplot) or the result of
#'   \link[graphics]{plot.default}.
#' @export
#' 
#' @examples 
#' kvtemp <- kentvillegreenwood %>% select_params(contains("temp"))
#' 
#' # use base plotting for regular biplot function
#' biplot(kvtemp)
#' 
#' # use ggplot and facet_grid to biplot
#' autobiplot(kvtemp, col = "location")
#' 
biplot.mudata <- function(x, ...) {
  # id variables
  id_vars <- c("dataset", "location", "param", attr(x, "x_columns"))
  # use long_biplot() to do biplotting
  long_biplot(x$data, id_vars = id_vars, name_var = "param", measure_var = "value",
              ...)
}

#' @rdname biplot.mudata
#' @export
autobiplot.mudata <- function(x, ...) {
  autobiplot(x$data, id_vars=c("dataset", "location", attr(x, "x_columns")),
             name_var = "param", measure_var="value", ...)
}

# a function to generate unique combination of things
unique_combinations <- function(vect) {
  if(length(vect) == 0) {
    return(tibble::tibble(.name_x = character(0), .name_y = character(0)))
  }
  
  # use utils::combn() to generate combinations
  comb_matrix <- utils::combn(as.character(vect), m = 2, simplify = TRUE)
  tibble::tibble(.name_x = comb_matrix[1, , drop = TRUE], .name_y = comb_matrix[2, , drop = TRUE])
}

