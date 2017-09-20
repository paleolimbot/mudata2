
#' Get distinct params, locations, and datasets from a mudata object
#'
#' @param x A mudata object
#' @param table The table to use to calculate the distinct values. Using
#'   the "data" table is safest, but for large datasets that are not
#'   in memory, using the meta table (params, locations, or datasets)
#'   may be useful.
#'
#' @return A character vector of distinct parameter names
#' @export
#'
#' @examples
#' distinct_params(kentvillegreenwood)
#' distinct_locations(kentvillegreenwood)
#' distinct_datasets(kentvillegreenwood)
#' 
distinct_params <- function(x, table = "data") {
  dplyr::collect(dplyr::distinct(x[[table]], .data$param))$param
}

#' @rdname distinct_params
#' @export
distinct_locations <- function(x, table = "data") {
  dplyr::collect(dplyr::distinct(x[[table]], .data$location))$location
}

#' @rdname distinct_params
#' @export
distinct_datasets <- function(x, table = "data") {
  dplyr::collect(dplyr::distinct(x[[table]], .data$dataset))$dataset
}

#' @rdname distinct_params
#' @export
distinct_columns <- function(x, table) {
  colnames(x[[table]])
}

#' Access components of a mudata object
#'
#' @param x A mudata object
#'
#' @return The appropriate component
#' @export
#'
#' @examples
#' data_tbl(kentvillegreenwood)
#' 
data_tbl <- function(x) {
  x$data
}

#' @rdname data_tbl
#' @export
params_tbl <- function(x) {
  x$params
}

#' @rdname data_tbl
#' @export
locations_tbl <- function(x) {
  x$locations
}

#' @rdname data_tbl
#' @export
datasets_tbl <- function(x) {
  x$datasets
}

#' @rdname data_tbl
#' @export
columns_tbl <- function(x) {
  x$columns
}

#' @rdname data_tbl
#' @export
x_columns <- function(x) {
  attr(x, "x_columns")
}



