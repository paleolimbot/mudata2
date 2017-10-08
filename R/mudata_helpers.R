
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
  .distinct_vector(x[[table]], "param")
}

#' @rdname distinct_params
#' @export
distinct_locations <- function(x, table = "data") {
  .distinct_vector(x[[table]], "location")
}

#' @rdname distinct_params
#' @export
distinct_datasets <- function(x, table = "data") {
  .distinct_vector(x[[table]], "dataset")
}

#' @rdname distinct_params
#' @export
distinct_columns <- function(x, table = names(x)) {
  all_names <- lapply(x[table], colnames)
  unique(unlist(all_names, use.names = FALSE))
}

#' @rdname distinct_params
#' @export
unique_params <- function(x, table = "data") {
  distinct_params(x, table = table)
}

#' @rdname distinct_params
#' @export
unique_locations <- function(x, table = "data") {
  distinct_locations(x, table = table)
}

#' @rdname distinct_params
#' @export
unique_datasets <- function(x, table = "data") {
  distinct_datasets(x, table = table)
}

#' @rdname distinct_params
#' @export
unique_columns <- function(x, table = names(x)) {
  distinct_columns(x, table = table)
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



