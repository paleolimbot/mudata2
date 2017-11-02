
#' Combine mudata objects
#'
#' @param ... Mudata objects to combine
#' @param validate Flag to validate final object
#'
#' @return A Mudata object
#' @export
#'
rbind.mudata <- function(..., validate=TRUE) {
  mudatas <- list(...)
  x_columns <- unique(unlist(lapply(mudatas, attr, "x_columns")))
  mudata(
    data = do.call(dplyr::bind_rows, lapply(mudatas, function(m) m$data)),
    locations = unique(do.call(dplyr::bind_rows, lapply(mudatas, function(m) m$locations))),
    params = unique(do.call(dplyr::bind_rows, lapply(mudatas, function(m) m$params))),
    datasets = unique(do.call(dplyr::bind_rows, lapply(mudatas, function(m) m$datasets))),
    columns = unique(do.call(dplyr::bind_rows, lapply(mudatas, function(m) m$columns))),
    x_columns = x_columns, validate = validate
  )
}

#' Subset (filter) a MuData object
#'
#' @param x,.data The object to subset
#' @param datasets Vector of datasets to include
#' @param params  Vector of parameters to include
#' @param locations Vector of locations to include
#' @param .factor Maintain order of selected components by converting them to factors
#' @param ... Aguments to/from methods
#'
#' @return A subsetted mudata object
#' @export
#'
subset.mudata <- function(x, ..., datasets = NULL, params = NULL, 
                          locations = NULL) {
  # enquos ...
  filter_args <- rlang::quos(...)
  
  # filter data
  new_x <- filter_data(x, rlang::UQS(filter_args))
  if(!is.null(datasets)) {
    new_x <- filter_data(new_x, .data$dataset %in% datasets)
  }
  if(!is.null(locations)) {
    new_x <- filter_data(new_x, .data$location %in% locations)
  }
  if(!is.null(params)) {
    new_x <- filter_data(new_x, .data$param %in% params)
  }
  
  # return filtered object
  new_x
}

#' @rdname subset.mudata
#' @export
select_datasets <- function(.data, ..., .factor = FALSE) {
  # quo-ify datasets
  datasets <- rlang::quos(...)
  # use tidyselect to get dataset names
  datasets <- tidyselect::vars_select(.tidyselect_vars(.data, "dataset"), rlang::UQS(datasets))
  new_datasets <- names(datasets)
  # use subset() to do the subsetting
  md_out <- filter_data(.data, .data$dataset %in% datasets)
  # rename datasets using rename_dataset
  if(any(new_datasets != datasets)) {
    renamer <- datasets[new_datasets != datasets]
    md_out <- rename_datasets_base(md_out, stats::setNames(names(renamer), renamer))
  }
  
  if(.factor) {
    md_out <- .factorize(md_out, "dataset", new_datasets)
  }
  
  md_out
}

#' @rdname subset.mudata
#' @export
select_locations <- function(.data, ..., .factor = FALSE) {
  # quo-ify locations
  locations <- rlang::quos(...)
  # use tidyselect to get location names
  locations <- tidyselect::vars_select(.tidyselect_vars(.data, "location"), rlang::UQS(locations))
  new_locations <- names(locations)
  # use subset() to do the subsetting
  md_out <- filter_data(.data, .data$location %in% locations)
  # rename datasets using rename_locations_base
  if(any(new_locations != locations)) {
    renamer <- locations[new_locations != locations]
    md_out <- rename_locations_base(md_out, stats::setNames(names(renamer), renamer))
  } 
  
  if(.factor) {
    md_out <- .factorize(md_out, "location", new_locations)
  }
  
  md_out
}

#' @rdname subset.mudata
#' @export
select_params <- function(.data, ..., .factor = FALSE) {
  # quo-ify params
  params <- rlang::quos(...)
  # use tidyselect to get location names
  params <- tidyselect::vars_select(.tidyselect_vars(.data, "param"), rlang::UQS(params))
  new_params <- names(params)
  # use subset() to do the subsetting
  md_out <- filter_data(.data, .data$param %in% params)
  # rename datasets using rename_params_base
  if(any(new_params != params)) {
    renamer <- params[new_params != params]
    md_out <- rename_params_base(md_out, stats::setNames(names(renamer), renamer))
  }
  
  if(.factor) {
    md_out <- .factorize(md_out, "param", new_params)
  }
  
  md_out
}

#' @rdname subset.mudata
#' @export
filter_datasets <- function(.data, ...) {
  filter_args <- rlang::quos(...)
  if(length(filter_args) == 0) return(.data)
  
  datasets <- .data$datasets %>%
    dplyr::filter(rlang::UQS(filter_args)) %>%
    dplyr::distinct("dataset") %>%
    dplyr::collect() %>%
    dplyr::pull("dataset")
  filter_data(.data, .data$dataset %in% datasets)
}

#' @rdname subset.mudata
#' @export
filter_data <- function(.data, ...) {
  filter_args <- rlang::quos(...)
  if(length(filter_args) == 0) return(.data)
  
  # lazily filter data
  dta <- dplyr::filter(.data$data, rlang::UQS(filter_args))
  
  # redefine params, locations, datasets to reflect subsetted data
  params <- dplyr::distinct(dta, "param")$param
  locations <- dplyr::distinct(dta, "location")$location
  datasets <- dplyr::distinct(dta, "dataset")$dataset
  
  pm <- dplyr::filter(.data$params, .data$param %in% params)
  lc <- dplyr::filter(.data$locations, .data$location %in% locations)
  cl <- dplyr::filter(.data$columns, .data$dataset %in% datasets)
  ds <- dplyr::filter(.data$datasets, .data$dataset %in% datasets)
  
  # keep class of original
  new_mudata(list(data=dta, locations=lc, params=pm, datasets=ds, 
                  columns=cl), x_columns = x_columns(.data))
}

#' @rdname subset.mudata
#' @export
filter_locations <- function(.data, ...) {
  filter_args <- rlang::quos(...)
  if(length(filter_args) == 0) return(.data)
  
  locations <- .data$locations %>%
    dplyr::filter(rlang::UQS(filter_args)) %>%
    dplyr::mutate(.id = paste(.data$dataset, .data$location, sep = "/////")) %>%
    dplyr::pull(".id")
  filter_data(.data, paste(.data$dataset, .data$location, sep = "/////") %in% locations)
}

#' @rdname subset.mudata
#' @export
filter_params <- function(.data, ...) {
  filter_args <- rlang::quos(...)
  if(length(filter_args) == 0) return(.data)
  
  params <- .data$params %>%
    dplyr::filter(rlang::UQS(filter_args)) %>%
    dplyr::mutate(.id = paste(.data$dataset, .data$param, sep = "/////")) %>%
    dplyr::pull(".id")
  filter_data(.data, paste(.data$dataset, .data$param, sep = "/////") %in% params)
}

.tidyselect_vars <- function(x, type) {
  singular <- type
  plural <- paste0(type, "s")
  vars <- as.character(.distinct_vector(x[[plural]], singular))
  attr(vars, "type") <- c(singular, plural)
  vars
}

.factorize <- function(md, column, levels) {
  for(tbl in src_tbls(md)) {
    if(column %in% colnames(md[[tbl]])) {
      md[[tbl]][[column]] <- factor(md[[tbl]][[column]], levels = levels)
    }
  }
  md
}

