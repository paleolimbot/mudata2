
#' Create a MuData object
#' 
#' Create an object describing multi-parameter spatiotemporal data in the (mostly) universal
#' data format. This format is a collection of tables as described below. For an example
#' of data already in this format, see the \link{kentvillegreenwood} dataset.
#'
#' @param data The data table, which is a molten data frame containing the columns (at least)
#'   'dataset', 'location', 'x', 'param', and 'value'. The 'dataset' column can be omitted
#'   if there is only one dataset contained in the object (its name can be specified by
#'   passing the parameter \code{dataset.id}). The 'location' column can be omitted if
#'   there is only data for one dataset and one location (its name can be specified by
#'   passing the parameter \code{location.id}).
#' @param locations The locations table, which is a data frame containing the columns (at least)
#'   'dataset', and 'location'. If omitted, it will be created automatically using all unique
#'   dataset/location combinations.
#' @param params The params table, which is a data frame containing the columns (at least)
#'   'dataset', and 'param'. If omitted, it will be created automatically using all unique
#'   dataset/param combinations.
#' @param datasets The datasets table, which is a data frame containing the column (at least)
#'   'dataset'. If omitted, it will be generated automatically using all unique datasets.
#' @param columns The columns table, which is a data frame containing the columns (at least)
#'   'dataset', 'table', and 'column'. If omitted, it will be created automatically using 
#'   all dataset/table/column combinations.
#' @param x_columns A vector of column names from the data table that in combination with
#'   'dataset', 'location', and 'param' identify unique rows.
#' @param ...,more_tbls More tbls (as named arguments) to be included in the mudata object
#' @param dataset_id The dataset id to use if a datasets column is omitted.
#' @param location_id The location id if a location column is omitted.
#' @param validate Pass \code{FALSE} to skip validation of input tables.
#'
#' @return A \code{mudata} object
#' @export
#' 
#' @examples
#' library(tidyr)
#' library(dplyr)
#' data(pocmaj)
#' 
#' # gather columns and summarise replicates
#' datatable <- pocmaj %>%
#'   gather(Ca, Ti, V, key = "param", value = "param_value") %>%
#'   group_by(core, param, depth) %>%
#'   summarise(value=mean(param_value), sd=mean(param_value)) %>%
#'   rename(location = core)
#'
#' # create mudata object
#' mudata(datatable)
#' 
mudata <- function(data, locations=NULL, params=NULL, datasets=NULL, columns=NULL,
                   x_columns = NULL, ..., more_tbls = NULL,
                   dataset_id='default', location_id = 'default', validate = TRUE) {
  # check validity of extra tbls
  more_tbls <- c(list(...), as.list(more_tbls))
  if(length(more_tbls) > 0 && (is.null(names(more_tbls)) || any(names(more_tbls) == ""))) {
    stop("more_tbls must only contain named tbls")
  }
  if(!all(vapply(more_tbls, function(x) dplyr::is.tbl(x) || is.data.frame(x), logical(1)))) {
    stop("more_tbls must only contain tbls")
  }
  
  # check data object
  .checkcols(data, 'data', c('param', 'value'))
  # check for x_columns if necessary
  if(is.null(x_columns)) {
    x_columns <- guess_x_columns(data)
  } else {
    # x_columns should be able to be character(0), for the case where there is no axis other than
    # dataset, location, and param
    # if(length(x_columns) == 0) stop("x_columns must be a character vector of length > 0")
    if(!is.character(x_columns)) stop("x_columns must be a character vector of length > 0")
    .checkcols(data, 'data', x_columns)
  }
  
  data <- data %>%
    # grouped tbls cause problems in other methods
    dplyr::ungroup() %>%
    # add default location if necessary
    .addlocation(location_id = location_id) %>%
    # add default dataset if necessary
    .adddataset(dataset_id = dataset_id) %>%
    # move dataset, location, param, x to the front
    .movetofront(c("dataset", "location", "param", x_columns, "value"))
  
  # check datasets object
  if(is.null(datasets)) {
    # create a derived table for datasets
    datasets <- dplyr::distinct_(dplyr::ungroup(data), "dataset")
  } else {
    # if there is no dataset column, use mutate to create one
    .checkcols(datasets, 'datasets', character(0)) # this just checks type of datasets
    datasets <- .adddataset(datasets, dataset_id = dataset_id)
    # check columns, move datasets to front
    .checkcols(datasets, 'datasets', 'dataset')
    datasets <- .movetofront(datasets, 'dataset')
  }
  
  # check locations object
  if(is.null(locations)) {
    locations <- dplyr::distinct_(dplyr::ungroup(data), "dataset", "location")
  } else {
    # if there is no dataset or location column, use mutate to create one
    .checkcols(locations, 'locations', character(0)) # this just checks type of locaitons
    locations <- locations %>%
      .adddataset(dataset_id = dataset_id) %>%
      .addlocation(location_id = location_id)
    
    # check columns, move dataset and location to the front
    .checkcols(locations, 'locations', c('dataset', 'location'))
    locations <- .movetofront(locations, c("dataset", "location"))
  }
  
  # check params object
  if(is.null(params)) {
    params <- dplyr::distinct_(dplyr::ungroup(data), "dataset", "param")
  } else {
    # if there is no dataset column, use mutate to create one
    .checkcols(params, 'params', 'param')
    params <- .adddataset(params, dataset_id = dataset_id)
    # check columns, move dataset and param to the front
    .checkcols(params, 'params', c('dataset', 'param'))
    params <- .movetofront(params, c("dataset", "param"))
  }
  
  # check columns object
  if(is.null(columns)) {
    # autogenerate columns table
    columns <- c(list(data = data, locations = locations, 
                      params = params, datasets = datasets),
                 more_tbls) %>%
      new_mudata(x_columns = x_columns) %>%
      generate_type_tbl(default = "guess")
  } else {
    # if there is no dataset column, use mutate to create one
    .checkcols(columns, 'columns', c('table', 'column'))
    columns <- .adddataset(columns, dataset_id = dataset_id)
    .checkcols(columns, 'columns', c('dataset', 'table', 'column'))
    columns <- .movetofront(columns, c('dataset', 'table', 'column'))
  }
  
  # create a list of tables
  mdlist <- c(list(data = data, locations = locations, params = params,
                   datasets = datasets, columns = columns),
              more_tbls)
  
  # coerce to tbls using dplyr
  mdlist <- lapply(mdlist, dplyr::as.tbl)
  
  # create object using new_mudata
  md <- new_mudata(mdlist, x_columns = x_columns)
  
  # validate object
  if(identical(validate, TRUE)) {
    validate <- stop
  }
  if(!identical(validate, FALSE)) {
    validate_mudata(md, action = validate)
  }
  
  # return object
  md
}

#' Create, validate a MUData object
#' 
#' Validates a MUData object by calling \code{stop} when an error is found.
#'
#' @param md An object of class 'mudata'
#' @param check_unique Check if columns identify unique values in the appropriate tables
#' @param check_references Check the referential integrity of the mudata object
#' @param x_columns A vector of column names from the data table that in combination with
#'   'dataset', 'location', and 'param' identify unique rows.
#' @param action the function to be called when errors are detected in validate_mudata
#'
#' @export
#' 
#' @examples 
#' data(kentvillegreenwood)
#' validate_mudata(kentvillegreenwood)
#' new_mudata(kentvillegreenwood, x_columns = "date")
#' 
new_mudata <- function(md, x_columns) {
  # check base type of md
  if(!is.list(md)) stop("Base type of md is not a list")
  # check for sql tables in md
  is_sql <- vapply(md, inherits, "tbl_sql", FUN.VALUE = logical(1))
  if(any(is_sql)) {
    classes <- c("mudata_sql", "mudata", "list")
  } else {
    classes <- c("mudata", "list")
  }
  # return classed list
  structure(md, x_columns = x_columns, class = classes)
}

#' @rdname new_mudata
#' @export
validate_mudata <- function(md, check_unique = TRUE, check_references = TRUE,
                            action = stop) {
  # check that it is a mudata object
  if(!inherits(md, "mudata")) action("Object is not a 'mudata' object")
  
  # check base type
  if(!is.list(md)) action("Base type of md is not a list")
  if(is.null(names(md))) action("Base type of md is not a named list")
  if(any(names(md) == "")) action("All members of md must be named")
  
  # check names
  missing_names <- setdiff(c("data", "locations", "params", "datasets", "columns"),
                           names(md))
  if(length(missing_names) > 0) action("The following tables were missing from md: ",
                                     paste(missing_names, collapse = " "))
  
  # check types (all members of md must be tbls)
  wrong_type_names <- !vapply(md, function(x) dplyr::is.tbl(x) || is.data.frame(x), 
                              logical(1))
  if(any(wrong_type_names)) action("The following tables were not a tbl or data.frame: ",
                                   paste(names(md)[wrong_type_names], collapse = " "))
  
  # check attributes
  x_columns <- attr(md, "x_columns")
  if(is.null(x_columns)) action("md is missing attribute x_columns")
  if(!is.character(x_columns)) action("attr(md, 'x_columns') is not a character vector")
  
  # check columns/classes
  .checkcols(md$locations, 'locations', c('dataset', 'location'), action = action)
  .checkcols(md$data, 'data', c('dataset', 'location', 'param', x_columns, 'value'), action = action)
  .checkcols(md$datasets, 'datasets', 'dataset', action = action)
  .checkcols(md$columns, 'columns', c('dataset', 'table', 'column'), action = action)
  .checkcols(md$params, 'params', c('dataset', 'param'), action = action)
  
  if(check_unique) {
    # ensure data is summarised
    .checkunique(md$data, "data", "dataset", "location", "param", x_columns, action = action)
  }
  
  if(check_references) {
    # get unique params, locations, and datasets from the data table
    params <- .distinct_vector(md$data, "param")
    locations <- .distinct_vector(md$data, "location")
    datasets <- .distinct_vector(md$data, "dataset")
    
    # get unique params, locations, and datasets from the meta tables
    table_locs <- .distinct_vector(md$locations, "location")
    table_params <- .distinct_vector(md$params, "param")
    table_datasets <- .distinct_vector(md$datasets, "dataset")
    
    # ensure locations in data are in the locations table
    noinflocs <- setdiff(table_locs, locations)
    if(length(noinflocs) > 0) action("Locations not included in location table: ", 
                                     paste(noinflocs, collapse=' '))
    noinfparams <- setdiff(table_params, params)
    if(length(noinfparams) > 0) action("Params not included in param table: ", 
                                       paste(noinfparams, collapse=' '))
    noinfds <- setdiff(table_datasets, datasets)
    if(length(noinfds) > 0) action("Datasets not included in dataset table: ", 
                                   paste(noinfds, collapse=' '))
    
    # ensure there are no extraneous information in information tables
    noinflocs <- setdiff(locations, table_locs)
    if(length(noinflocs) > 0) action("Locations ", paste(noinflocs, collapse=' '), 
                                     " not included in data")
    noinfparams <- setdiff(params, table_params)
    if(length(noinfparams) > 0) action("Parameters ", paste(noinfparams, collapse=' '), 
                                       " not included in data")
    noinfds <- setdiff(datasets, table_datasets)
    if(length(noinfds) > 0) action("Datasets ", paste(noinfds, collapse=' '), 
                                   " not included in data")
  }
  
  if(check_unique) {
    # ensure no duplicates in locations, datasets, params, columns
    .checkunique(md$locations, "locations", "dataset", "location", action = action)
    .checkunique(md$params, "params", "dataset", "param", action = action)
    .checkunique(md$datasets, "datasets", "dataset", action = action)
    .checkunique(md$columns, "columns", "dataset", "table", "column", action = action)
  }
  
  # return the object, invisibly
  invisible(md)
}

#' Test if an object is a mudata object
#'
#' @param x An object
#'
#' @return TRUE if the object is a mudata, FALSE otherwise
#' @export
#'
#' @examples
#' is_mudata(kentvillegreenwood)
is_mudata <- function(x) {
  inherits(x, "mudata")
}

#' @rdname is_mudata
#' @export
is.mudata <- function(x) {
  is_mudata(x)
}

#' Coerce objects to mudata
#'
#' @param x An object
#' @param ... Passed to other methods
#'
#' @return A \link{mudata} object or an error
#' @export
#' 
as_mudata <- function(x, ...) UseMethod("as_mudata")

#' @rdname as_mudata
#' @export
as.mudata <- function(x, ...) UseMethod("as_mudata")

#' @rdname as_mudata
#' @export
as_mudata.mudata <- function(x, ...) {
  x
}

#' @rdname as_mudata
#' @export
as_mudata.data.frame <- function(x, ...) {
  mudata(data = x, ...)
}

#' @rdname as_mudata
#' @export
as_mudata.tbl <- function(x, ...) {
  mudata(data = x, ...)
}

#' @rdname as_mudata
#' @export
as_mudata.src_sql <- function(x, ...) {
  mudata_sql(db = x, ...)
}

#' @rdname as_mudata
#' @export
as_mudata.list <- function(x, ...) {
  mudata(data = x$data, locations = x$locations,
         params = x$params, datasets = x$datasets, columns = x$columns,
         x_columns = attr(x, "x_columns"), ...)
}

.checkunique <- function(tbl, context, ..., action = stop) {
  # empty tables can be considered unique
  if(.isempty(tbl)) return()
  
  lengths <- tbl %>% 
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(c(...))) %>%
    dplyr::group_by_all() %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$n) %>%
    dplyr::distinct() %>%
    dplyr::collect()
  
  if(!identical(lengths[[1]], 1L)) {
    action(sprintf("Duplicate %s in %s table", context, context))
  }
}

# gets distinct values of a single column as a vector
.distinct_vector <- function(tbl, col) {
  col <- tbl %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(col)) %>%
    dplyr::distinct() %>%
    dplyr::collect()
  col[[1]]
}

# checks for emtpy tbls
.isempty <- function(tbl) {
  nrow(dplyr::collect(utils::head(tbl))) == 0
}

# ensures all columns in required_cols are in df, and that df has colnames to begin with
.checkcols <- function(df, name, required_cols, action = stop) {
  if(!inherits(df, "data.frame") && !inherits(df, "tbl")) {
    action(sprintf("Table '%s' is not a data.frame", name))
  }
  missingcols <- setdiff(required_cols, colnames(df))
  if(length(missingcols)>0) action(sprintf("Table '%s' is missing columns %s",
                                           name,
                                           paste0("'", missingcols, "'", collapse=", ")))
}

# moves required_cols to the front of tbl
.movetofront <- function(tbl, required_cols) {
  dplyr::select(tbl, dplyr::one_of(required_cols), dplyr::everything())
}

# adds a default dataset to a tbl
.adddataset <- function(tbl, dataset_id) {
  # if there is no dataset column, use mutate to create one
  if(!('dataset' %in% colnames(tbl))) {
    # can't add a dataset to a table with zero rows (ambiguous)
    if(.isempty(tbl)) stop("Can't add a dataset to a table with zero rows!")
    tbl <- dplyr::mutate(tbl, dataset = dataset_id)
  }
  tbl
}

# adds a default location to a tbl
.addlocation <- function(tbl, location_id) {
  # if there is no location column, use mutate to create one
  if(!('location' %in% colnames(tbl))) {
    # can't add a location to a table with zero rows (ambiguous)
    if(.isempty(tbl)) stop("Can't add a location to a table with zero rows!")
    tbl <- dplyr::mutate(tbl, location = location_id)
  }
  tbl
}

# guesses the "x" column, or the column along which the data are aligned
guess_x_columns <- function(df, quiet = FALSE) {
  # make sure value is a column
  if(!("value" %in% colnames(df))) stop("Could not guess x columns: no 'value' column")
  
  # looking for the column name(s) before 'value'
  value <- which(colnames(df) == "value")[1]
  cols <- setdiff(colnames(df)[1:value], c("dataset", "location", "param", "value"))
  
  # x_columns should be able to be character(0), for the case where there is no axis other than
  # dataset, location, and param
  #if(length(cols) == 0) stop("Could not guess x columns from names: ",
  #                           paste(colnames(df), collapse = ", "))
  
  if(!quiet) message("Guessing x columns: ",
                     paste(cols, collapse = ", "))
  
  # return cols
  cols
}

#' Create a mudata object using a database source
#'
#' @param db An src_sql as generated by dplyr
#' @param data The name of the data table
#' @param locations The name of the locations table
#' @param params The name of the params table
#' @param datasets The name of the datasets table
#' @param columns The name of the columns table
#'
#' @return A mudata object
#' @export
#'
mudata_sql <- function(db, data = "data", locations = NA, params = NA, 
                      datasets = NA, columns = NA) {
  if(!inherits(db, "src_sql")) stop("'db' must be an 'src_sql'")
  
  # get tables from source
  src_tbls <- dplyr::src_tbls(db)
  
  # set default values for non-data tbls
  if(identical(locations, NA) && "locations" %in% src_tbls) {
    locations <- "locations"
  }
  if(identical(params, NA) && "params" %in% src_tbls) {
    params <- "params"
  }
  if(identical(datasets, NA) && "datasets" %in% src_tbls) {
    datasets <- "datasets"
  }
  if(identical(columns, NA) && "columns" %in% src_tbls) {
    columns <- "columns"
  }
  
  mudata(
    data = dplyr::tbl(db, data),
    locations = if(is.null(locations)) NULL else dplyr::tbl(db, locations),
    params = if(is.null(params)) NULL else dplyr::tbl(db, params),
    datasets = if(is.null(datasets)) NULL else dplyr::tbl(db, datasets),
    columns = if(is.null(columns)) NULL else dplyr::tbl(db, columns)
  )
}

#' @export
#' @importFrom tidyselect everything matches starts_with ends_with num_range
#' @importFrom tidyselect contains one_of last_col
tidyselect::everything

#' @export
tidyselect::matches

#' @export
tidyselect::starts_with

#' @export
tidyselect::ends_with

#' @export
tidyselect::ends_with

#' @export
tidyselect::contains

#' @export
tidyselect::num_range

#' @export
tidyselect::one_of

#' @export
tidyselect::last_col

#' @export
magrittr::`%>%`

#' @rdname subset.mudata
#' @importFrom dplyr collect
#' @export
collect.mudata <- function(x, ...) {
  new_mudata(lapply(x, dplyr::collect), x_columns = attr(x, "x_columns"))
}
