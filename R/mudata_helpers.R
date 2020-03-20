
#' Get distinct params, locations, and datasets from a mudata object
#'
#' @param x A mudata object
#' @param table The table to use to calculate the distinct values. Using
#'   the "data" table is safest, but for large datasets that are not
#'   in memory, using the meta table (params, locations, or datasets)
#'   may be useful.
#' @param ... Passed to other methods
#'
#' @return A character vector of distinct parameter names
#' @export
#'
#' @examples
#' distinct_params(kentvillegreenwood)
#' distinct_locations(kentvillegreenwood)
#' distinct_datasets(kentvillegreenwood)
distinct_params <- function(x, ...) {
  UseMethod("distinct_params")
}

#' @rdname distinct_params
#' @export
distinct_params.default <- function(x, table = "data", ...) {
  as.character(sort(.distinct_vector(x[[table]], "param")))
}

#' @rdname distinct_params
#' @export
distinct_locations <- function(x, ...) {
  UseMethod("distinct_locations")
}

#' @rdname distinct_params
#' @export
distinct_locations.default <- function(x, table = "data", ...) {
  as.character(sort(.distinct_vector(x[[table]], "location")))
}

#' @rdname distinct_params
#' @export
distinct_datasets <- function(x, ...) {
  UseMethod("distinct_datasets")
}

#' @rdname distinct_params
#' @export
distinct_datasets.default <- function(x, table = "data", ...) {
  as.character(sort(.distinct_vector(x[[table]], "dataset")))
}

#' @rdname distinct_params
#' @export
distinct_columns <- function(x, ...) {
  UseMethod("distinct_columns")
}

#' @rdname distinct_params
#' @export
distinct_columns.default <- function(x, table = names(x), ...) {
  all_names <- lapply(x[table], colnames)
  sort(unique(unlist(all_names, use.names = FALSE)))
}

#' @rdname distinct_params
#' @importFrom dplyr src_tbls
#' @export
src_tbls.mudata <- function(x, ...) {
  names(x)
}

#' Access components of a mudata object
#'
#' @param x,src A mudata object
#' @param which Which tbl to extract
#' @param key,value Passed to [spread][tidyr::spread]
#' @param ... Passed to other methods
#'
#' @return The appropriate component
#' @export
#'
#' @examples
#' tbl_data(kentvillegreenwood)
tbl_data <- function(x) {
  UseMethod("tbl_data")
}

#' @rdname tbl_data
#' @export
tbl_data.default <- function(x) {
  x$data
}

#' @rdname tbl_data
#' @export
tbl_data_wide <- function(x, ...) {
  UseMethod("tbl_data_wide")
}

#' @rdname tbl_data
#' @export
tbl_data_wide.default <- function(x, key = "param", value = "value", ...) {
  key_quo <- enquo(key)
  value_quo <- enquo(value)
  x %>%
    tbl_data() %>%
    dplyr::select(one_of(c("dataset", "location", "param", x_columns(x))), 
                  !!key_quo, !!value_quo) %>%
    tidyr::spread(key = !!key_quo, value = !!value_quo, ...)
}

#' @rdname tbl_data
#' @export
tbl_params <- function(x) {
  UseMethod("tbl_params")
}

#' @rdname tbl_data
#' @export
tbl_params.default <- function(x) {
  x$params
}

#' @rdname tbl_data
#' @export
tbl_locations <- function(x) {
  UseMethod("tbl_locations")
}

#' @rdname tbl_data
#' @export
tbl_locations.default <- function(x) {
  x$locations
}

#' @rdname tbl_data
#' @export
tbl_datasets <- function(x) {
  UseMethod("tbl_datasets")
}

#' @rdname tbl_data
#' @export
tbl_datasets.default <- function(x) {
  x$datasets
}

#' @rdname tbl_data
#' @export
tbl_columns <- function(x) {
  UseMethod("tbl_columns")
}

#' @rdname tbl_data
#' @export
tbl_columns <- function(x) {
  x$columns
}

#' @rdname tbl_data
#' @importFrom dplyr tbl
#' @export
tbl.mudata <- function(src, which, ...) {
  src[[which]]
}

#' @rdname tbl_data
#' @export
x_columns <- function(x) {
  UseMethod("x_columns")
}

#' @rdname tbl_data
#' @export
x_columns.default <- function(x) {
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
update_datasets <- function(x, ...) {
  UseMethod("update_datasets")
}

#' @rdname update_datasets
#' @export
update_datasets.default <- function(x, datasets, ...) {
  if(missing(datasets)) {
    datasets <- distinct_datasets(x, table = "datasets")
  }
  # find rows to update
  rows <- x$datasets$dataset %in% datasets
  if(any(rows)) {
    .update_rows(x, "datasets", rows, ...)
  } else {
    bad_datasets <- paste0("'", datasets, "'", collapse = ", ")
    abort(glue::glue("Zero rows were found for dataset {bad_datasets}"))
  }
}

#' @rdname update_datasets
#' @export
update_locations <- function(x, ...) {
  UseMethod("update_locations")
}

#' @rdname update_datasets
#' @export
update_locations.default <- function(x, locations, datasets, ...) {
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
    bad_locations <- paste0("'", locations, "'", collapse = ", ")
    bad_datasets <- paste0("'", datasets, "'", collapse = ", ")
    abort(glue::glue("Zero rows were found for locations {bad_locations} (datasets: {bad_datasets})"))
  }
}

#' @rdname update_datasets
#' @export
update_params <- function(x, ...) {
  UseMethod("update_params")
}

#' @rdname update_datasets
#' @export
update_params.default <- function(x, params, datasets, ...) {
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
    bad_params <- paste0("'", params, "'", collapse = ", ")
    bad_datasets <- paste0("'", datasets, "'", collapse = ", ")
    abort(glue::glue("Zero rows were found for params {bad_params} (datasets: {bad_datasets})"))
  }
}

#' @rdname update_datasets
#' @export
update_columns <- function(x, ...) {
  UseMethod("update_columns")
}

#' @rdname update_datasets
#' @export
update_columns.default <- function(x, columns, tables, datasets, ...) {
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
    bad_columns <- paste0("'", columns, "'", collapse = ", ")
    bad_datasets <- paste0("'", datasets, "'", collapse = ", ")
    bad_tables <- paste0("'", tables, "'", collapse = ", ")
    abort(
      glue::glue(
        "Zero rows were found for columns {bad_columns} (datasets: {bad_datasets}; tables: {bad_tables})"
      )
    )
  }
}

.update_rows <- function(x, tbl, rows, ...) {
  # assign vals, check that it is one row
  vals <- tibble::tibble(...)
  # no rows mean there is nothing to update
  # this gets handled by wrapper functions
  if(nrow(vals) == 0) {
    return(x) # nocov
  }
  
  if(nrow(vals) != 1) {
    abort("values to update must all be of length 1")
  }
  
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


#' Modify mudata tables
#'
#' @param x A mudata object
#' @param tbl The table name to modify
#' @param ... Passed to [mutate][dplyr::mutate]
#'
#' @return A modified mudata object
#' @export
#'
#' @examples
#' library(lubridate)
#' second_lake_temp %>% 
#'   mutate_data(datetime = with_tz(datetime, "America/Halifax"))
#' 
mutate_data <- function(x, ...) {
  mutate_tbl(x, "data", ...)
}

#' @rdname mutate_data
#' @export
mutate_params <- function(x, ...) {
  mutate_tbl(x, "params", ...)
}

#' @rdname mutate_data
#' @export
mutate_locations <- function(x, ...) {
  mutate_tbl(x, "locations", ...)
}

#' @rdname mutate_data
#' @export
mutate_datasets <- function(x, ...) {
  mutate_tbl(x, "datasets", ...)
}

#' @rdname mutate_data
#' @export
mutate_columns <- function(x, ...) {
  mutate_tbl(x, "columns", ...)
}

#' @rdname mutate_data
#' @export
mutate_tbl <- function(x, ...) {
  UseMethod("mutate_tbl")
}

#' @rdname mutate_data
#' @export
mutate_tbl.default <- function(x, tbl, ...) {
  x[[tbl]] <- dplyr::mutate(x[[tbl]], ...)
  x
}

