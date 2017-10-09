
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
#' tbl_data(kentvillegreenwood)
#' 
tbl_data <- function(x) {
  x$data
}

#' @rdname tbl_data
#' @export
tbl_params <- function(x) {
  x$params
}

#' @rdname tbl_data
#' @export
tbl_locations <- function(x) {
  x$locations
}

#' @rdname tbl_data
#' @export
tbl_datasets <- function(x) {
  x$datasets
}

#' @rdname tbl_data
#' @export
tbl_columns <- function(x) {
  x$columns
}

#' @rdname tbl_data
#' @export
x_columns <- function(x) {
  attr(x, "x_columns")
}


#' Add documentation to mudata objects
#'
#' @param x A mudata object
#' @param datasets One or more datasets to update
#' @param locations One or more locations to update
#' @param params One or more params to update
#' @param tables One or more tables to update (columns table)
#' @param columns One or more columns to update (columns table)
#' @param ... Key/value pairs (values of length 1)
#'
#' @return A modified version of x
#' @export
#'
#' @examples
#' kentvillegreenwood %>%
#'   update_datasets("ecclimate", new_key = "new_value") %>%
#'   tbl_datasets()
#' 
update_datasets <- function(x, datasets, ...) {
  if(missing(datasets)) {
    datasets <- distinct_datasets(x, table = "datasets")
  }
  # find rows to update
  rows <- x$datasets$dataset %in% datasets
  if(any(rows)) {
    .update_rows(x, "datasets", rows, ...)
  } else {
    stop("Zero rows were found for dataset: ", paste(datasets, collapse = ", "))
  }
}

#' @rdname update_datasets
#' @export
update_locations <- function(x, locations, ..., datasets) {
  if(missing(datasets)) {
    datasets <- distinct_datasets(x, table = "locations")
  }
  if(missing(locations)) {
    locations <- distinct_locations(x, table = "locations")
  }
  # find rows to update
  rows <- (x$locations$dataset %in% datasets) & (x$locations$location %in% locations)
  if(any(rows)) {
    .update_rows(x, "locations", rows, ...)
  } else {
    stop("Zero rows were found for locations: ", 
         paste(locations, collapse = ", "),
         " (datasets: ", 
         paste(datasets, collapse = ", "),
         ")")
  }
}

#' @rdname update_datasets
#' @export
update_params <- function(x, params, ..., datasets) {
  if(missing(datasets)) {
    datasets <- distinct_datasets(x, table = "params")
  }
  if(missing(params)) {
    params <- distinct_params(x, table = "params")
  }
  # find rows to update
  rows <- (x$params$dataset %in% datasets) & (x$params$param %in% params)
  if(any(rows)) {
    .update_rows(x, "params", rows, ...)
  } else {
    stop("Zero rows were found for params: ", 
         paste(params, collapse = ", "),
         " (datasets: ", 
         paste(datasets, collapse = ", "),
         ")")
  }
}

#' @rdname update_datasets
#' @export
update_columns <- function(x, tables, columns, ..., datasets) {
  if(missing(datasets)) {
    datasets <- distinct_datasets(x, table = "columns")
  }
  if(missing(tables)) {
    tables <- x$columns %>% dplyr::select("table") %>% dplyr::distinct() %>%
      dplyr::pull("table")
  }
  if(missing(columns)) {
    columns <- x$columns %>% dplyr::select("column") %>% dplyr::distinct() %>%
      dplyr::pull("column")
  }
  # find rows to update
  rows <- (x$columns$dataset %in% datasets) & (x$columns$table %in% tables) &
    (x$columns$column %in% columns)
  
  if(any(rows)) {
    .update_rows(x, "columns", rows, ...)
  } else {
    stop("Zero rows were found for columns: ", 
         paste(columns, collapse = ", "),
         " (datasets: ", 
         paste(datasets, collapse = ", "),
         "; tables: ",
         paste(tables, collapse = ", "),
         ")")
  }
}

.update_rows <- function(x, tbl, rows, ...) {
  # assign vals, check that it is one row
  vals <- tibble::tibble(...)
  # no rows mean there is nothign to update
  if(nrow(vals) == 0) return(x)
  if(nrow(vals) != 1) stop("values to update must all be of length 1")
  
  # bind vals to end of table with dummy dataset, to ensure types are correct
  # and all columns exist
  new_tbl <- dplyr::bind_rows(x[[tbl]], dplyr::mutate(vals, .dummy_variable. = TRUE))
  new_tbl[c(rows, FALSE), names(vals)] <- vals
  # one more cmd hack
  .dummy_variable. <- NULL; rm(.dummy_variable.)
  x[[tbl]] <- new_tbl %>%
    dplyr::filter(is.na(.dummy_variable.)) %>%
    dplyr::select(-.dummy_variable.)
  x
}
