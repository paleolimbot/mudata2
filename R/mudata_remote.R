

#' @export
rbind.mudata_sql <- function(...) {
  stop("combining of mudata_sql objects is not implemented")
}

#' @export
select_locations.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
select_params.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
select_datasets.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
filter_locations.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
filter_params.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
filter_datasets.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
filter_data.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
rename_locations.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
rename_params.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
rename_datasets.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

#' @export
rename_columns.mudata_sql <- function(.data, ...) {
  error_no_subset_sql()
}

error_no_subset_sql <- function() {
  stop("subsetting of mudata_sql objects is not implemented")
}
