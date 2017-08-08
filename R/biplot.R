
#' Biplot a molten data frame using facet_grid
#' 
#' Uses the ggplot framework and \code{facet_grid} to produce biplots of a molten
#' data frame.
#' 
#' @param x the object to biplot
#' @param id_vars the columns that identify a single value
#' @param name_var The column where namesx and namesy are to be found
#' @param names_x The names to be included in the x axes, or all the names to be included
#' @param names_y The names to be included on the y axes, or NULL for all possible combinations
#'   of \code{namesx}.
#' @param measure_var The column containing the values to plot
#' @param error_var The column containing the errors. Use \code{NULL} for default ("err" if column
#'   exists or none otherwise), or \code{NA} to suppress.
#' @param labeller The labeller to use to label facets (may want to use \code{label_parsed}
#'   to use plotmath-style labels)
#' @param validate Ensure id_vars identify unique values
#' @param ... passed to \code{aes_string()}
#' 
#' @importFrom stats biplot
#' @export
#' @examples 
#' library(tidyr)
#' library(dplyr)
#' data(pocmajsum)
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
                       names_y = NULL, validate = TRUE) {
  # check variables
  if(!inherits(x, "data.frame") && !dplyr::is.tbl(x)) stop("x must be a tbl or data.frame")
  missing_vars <- setdiff(c(id_vars, name_var), colnames(x))
  if(length(missing_vars) > 0) {
    stop("Some of id_vars/measure_var/name_var are not in colnames(x): ",
         paste(missing_vars, collapse = ", "))
  }
  
  # id_vars shouldn't contain name_var
  id_vars <- setdiff(id_vars, name_var)
  
  # check that c(id_vars, name_var) identify unique values
  if(validate) {
    .checkunique(x, "x", id_vars, name_var)
  }
  
  # make data with known name column
  data <- x %>% 
    dplyr::rename_(.name = name_var)

  
  # make a list of parameter names
  all_params <- dplyr::distinct(data, .name) %>% dplyr::collect() %>% .$.name %>%
    as.character()
  
  # make name combinations
  if(is.null(names_x) && is.null(names_y)) {
    # all combinations of all_params
    name_x <- rev(all_params[1:length(all_params)-1])
    name_y <- rev(all_params[2:length(all_params)])
    
    message("Using names_x = c(", 
            paste0('"', name_x, '"', collapse = ", "), 
            "), names_y = c(", 
            paste0('"', name_y, '"', collapse = ", "), 
            ")")
  } else if(is.null(names_y)) {
    # check for missing names
    missing_names <- setdiff(names_x, all_params)
    if(length(missing_names) > 0) stop("The following names were missing from ", name_var, ":",
                                       paste(missing_names, collapse = ", "))
    # all combinations of names_x
    name_x <- rev(names_x[1:length(names_x)-1])
    name_y <- rev(names_x[2:length(names_x)])
    
    message("Using names_x = c(", 
            paste0('"', name_x, '"', collapse = ", "), 
            "), names_y = c(", 
            paste0('"', name_y, '"', collapse = ", "), 
            ")")
  } else {
    # check for missing names
    missing_names <- setdiff(c(names_x, names_y), all_params)
    if(length(missing_names) > 0) stop("The following names were missing from ", name_var, ":",
                                       paste(missing_names, collapse = ", "))
    
    # combinations are already specified
    name_x <- names_x
    name_y <- names_y
  }
  
  # create name combinations
  name_combinations <- expand.grid(.name_x = name_x, .name_y = name_y, 
                                   stringsAsFactors = FALSE) %>%
    # don't include combinations with the same parameter
    dplyr::filter(.name_x != .name_y)
  
  # use name combinations to filter and join data, rbinding into a single long
  # data frame at the end
  join_vars <- setdiff(id_vars, name_var)
  
  data_pairs <- plyr::adply(name_combinations, .margins = 1, function(combination) {
    # filter data to get data_x and data_y
    data_x <- data %>% filter(.name == combination$.name_x) %>% dplyr::collect()
    data_y <- data %>% filter(.name == combination$.name_y) %>% dplyr::collect()
    # join using join_vars
    data_both <- dplyr::inner_join(data_x, data_y,
                                   by = join_vars, suffix = c("_x", "_y"))
    
    # return data_both
    data_both
  })
  
  # make name_x and name_y factors with the levels specified
  data_pairs$.name_x <- factor(data_pairs$.name_x, levels = name_x)
  data_pairs$.name_y <- factor(data_pairs$.name_y, levels = name_y)
  
  # return data_both as a tibble
  tibble::as_tibble(data_pairs)
}

#' @rdname long_pairs
#' @export
long_biplot <- function(x, id_vars, name_var, measure_var = "value", 
                        names_x = NULL, na.rm = FALSE, ...) {
  
  # id_vars shouldn't contain name_var
  id_vars <- setdiff(id_vars, name_var)
  
  # rename value and name column
  x <- x %>%
    dplyr::rename_(.name = name_var, .value = measure_var)
  
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
  
  # use spread to use automatic biplotting functionality in base plot
  x %>%
    dplyr::select(dplyr::one_of(c(id_vars, ".name", ".value"))) %>%
    tidyr::spread(.name, .value) %>%
    dplyr::select(-dplyr::one_of(id_vars)) %>%
    graphics::plot(...)
}

#' @rdname long_pairs
#' @export
autobiplot <- function(x, ...) UseMethod("autobiplot")

#' @rdname long_pairs
#' @export
autobiplot.data.frame <- function(x, id_vars, name_var, measure_var = "value", names_x = NULL, 
                                  names_y = NULL, error_var = NULL, na.rm = TRUE, validate = TRUE,
                                  labeller = ggplot2::label_value, ...) {
  
  # id_vars shouldn't contain name_var
  id_vars <- setdiff(id_vars, name_var)
  
  # rename value name and error column
  if(is.null(error_var)) {
    x <- x %>%
      dplyr::rename_(.name = name_var, .value = measure_var)
  } else {
    x <- x %>%
      dplyr::rename_(.name = name_var, .value = measure_var, .error = error_var)
  }
  
  # remove nas if specified
  if(na.rm) {
    x <- x %>%
      dplyr::filter(!is.na(.value))
  }
  
  # create data pairs
  data_pairs <- long_pairs(x, id_vars = id_vars, name_var = ".name",
                           names_x = names_x, names_y = names_y, validate = validate)
  
  # assign error geometry, if error column is specified
  ggerror <- NULL
  if(!is.null(error_var)) {
    ggerror <- list(
      ggplot2::geom_errorbar(
        ggplot2::aes_string(ymax=".value_y+.error_y"), ymin=".value_y-.error_y"),
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
#' Uses the ggplot framework and \code{facet_grid} to produce a biplot of a mudata object.
#'
#' @param x A mudata object
#' @param ... passed to plotting methods
#'
#' @return a ggplot object
#' @export
#' 
#' @examples 
#' data(kentvillegreenwood)
#' kvtemp <- subset(kentvillegreenwood, params = c("mintemp", "maxtemp", "meantemp"))
#' 
#' # use base plotting for regular biplot function
#' biplot(kvtemp)
#' 
#' # use ggplot and facet_grid to biplot
#' autobiplot(kvtemp, col = "location")
#'
biplot.mudata <- function(x, ...) {
  # id variables are used twice
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


