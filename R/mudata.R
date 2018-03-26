
#' Create a mudata object
#' 
#' Create a mudata object, which is a collection of five tables: data,
#' locations, params, datasets, and columns. You are only required to provide
#' the data table, which must contain columns "param" and "value", but will more
#' typically contain columns "location", "param", "datetime" (or "date"), and
#' "value". See \link{ns_climate}, \link{kentvillegreenwood}, \link{alta_lake},
#' \link{long_lake}, and \link{second_lake_temp} for examples of data in this
#' format.
#' 
#' @param data A data.frame/\link[tibble]{tibble} containing columns "param" and
#'   "value" (at least), but more typically columns "location", "param",
#'   "datetime" (or "date", depending on the type of data), and "value".
#' @param locations The locations table, which is a data frame containing the
#'   columns (at least) "dataset", and "location". If omitted, it will be
#'   created automatically using all unique dataset/location combinations.
#' @param params The params table, which is a data frame containing the columns
#'   (at least) "dataset", and "param". If omitted, it will be created
#'   automatically using all unique dataset/param combinations.
#' @param datasets The datasets table, which is a data frame containing the
#'   column (at least) "dataset". If omitted, it will be generated automatically
#'   using all unique datasets.
#' @param columns The columns table, which is a data frame containing the
#'   columns (at least) "dataset", "table", and "column". If omitted, it will be
#'   created automatically using all dataset/table/column combinations.
#' @param x_columns A vector of column names from the data table that in
#'   combination with "dataset", "location", and "param" identify unique rows.
#'   These will typically be guessed using the column names between "param" and
#'   "value".
#' @param ...,more_tbls More tbls (as named arguments) to be included in the
#'   mudata object
#' @param dataset_id The dataset to use if a "dataset" column is omitted.
#' @param location_id The location if a "location" column is omitted.
#' @param validate Pass \code{FALSE} to skip validation of input tables using
#'   \link{validate_mudata}.
#'   
#' @return An object of class "mudata", which is a \link{list} with components
#'   data, locations, params, datasets, columns, and any other tables provided
#'   in \code{more_tbls}. All list components must be tbls.
#' @export
#' 
#' @references Dunnington DW and Spooner IS (2018). "Using a linked table-based
#' structure to encode self-describing multiparameter spatiotemporal data".
#' FACETS. doi:10.1139/facets-2017-0026 
#' \url{http://www.facetsjournal.com/doi/10.1139/facets-2017-0026}
#' 
#' @examples
#' # use the data table from kentvillegreenwood as a template
#' kg_data <- tbl_data(kentvillegreenwood)
#' # create mudata object using just the data table
#' mudata(kg_data)
#' 
#' # create a mudata object starting from a parameter-wide data frame
#' library(tidyr)
#' library(dplyr)
#' 
#' # gather columns and summarise replicates
#' datatable <- pocmaj %>%
#'   gather(Ca, Ti, V, key = "param", value = "param_value") %>%
#'   group_by(core, param, depth) %>%
#'   summarise(value = mean(param_value), sd = mean(param_value)) %>%
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
    datasets <- data %>% dplyr::select("dataset") %>% dplyr::distinct()
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
    locations <- data %>%
      dplyr::select("dataset", "location") %>%
      dplyr::distinct()
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
    params <- data %>%
      dplyr::select("dataset", "param") %>%
      dplyr::distinct()
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

#' Validate, create a mudata object
#' 
#' Validates a mudata object by calling \link{stop} when an error is found;
#' creates a mudata object from a \link{list}. Validation is generally performed
#' when objects are created using \link{mudata}, or when objects are read/writen
#' using \link{read_mudata} and \link{write_mudata}.
#'
#' @param md An object of class 'mudata'
#' @param check_unique Check if columns identify unique values in the appropriate tables
#' @param check_references Check the referential integrity of the mudata object
#' @param x_columns The x_columns attribute (see \link{mudata}).
#' @param action The function to be called when errors are detected in validate_mudata
#'
#' @export
#' 
#' @examples 
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
  
  # check column types
  .checktypes(md$locations, 'locations', c('dataset', 'location'), c("character", "factor"), action = action)
  .checktypes(md$data, 'data', c('dataset', 'location', 'param'), c("character", "factor"), action = action)
  .checktypes(md$datasets, 'datasets', 'dataset', c("character", "factor"), action = action)
  .checktypes(md$columns, 'columns', 'dataset', c("character", "factor"), action = action)
  .checktypes(md$columns, 'columns', c('table', 'column'), "character", action = action)
  .checktypes(md$params, 'params', c('dataset', 'param'), c("character", "factor"), action = action)
  
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
#' @return TRUE if the object is a mudata object, FALSE otherwise
#' @export
#'
#' @examples
#' is_mudata(kentvillegreenwood)
#' 
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
         x_columns = attr(x, "x_columns"), 
         more_tbls = x[setdiff(names(x), c("data", "locations", "params", "datasets", "columns"))],
         ...)
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

.checktypes <- function(df, name, cols, types, action = stop) {
  df_head <- utils::head(dplyr::collect(df))
  wrong_type_cols <- !vapply(cols, 
                             function(col_name) any(class(df_head[[col_name]]) %in% types), 
                             logical(1))
  if(any(wrong_type_cols)) {
    action(sprintf("Table '%s' has columns of incorrect type: %s",
                   name,
                   paste0("'", cols[wrong_type_cols], "'", collapse = ", ")))
  }
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
#' @keywords internal
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


#' Collect all mudata components
#' 
#' Objects created by \link{mudata} are generally assumed to be local data frames,
#' but some methods may function on database tbls (especially in the future). 
#' This function applies \link[dplyr]{collect} to all component tables.
#'
#' @param x A mudata object
#' @param ... Passed to \link[dplyr]{collect}
#'
#' @return A mudata object with all components as local data frames.
#' @export
#' @importFrom dplyr collect
#'
collect.mudata <- function(x, ...) {
  new_mudata(lapply(x, dplyr::collect), x_columns = attr(x, "x_columns"))
}


#' Print a mudata object
#'
#' @param x,object A mudata object
#' @param width The number of characters to use as console width
#' @param ... Passed to other methods
#'
#' @return print returns x (invisibly); summary returns a data frame with summary information.
#' @export
#'
#' @examples
#' print(kentvillegreenwood)
#' 
print.mudata <- function(x, ..., width = NULL) {

  cat(format_vector(prefix = "A mudata object aligned along ", x_columns(x), width = width))
  cat("\n")
  cat(format_vector(prefix = "  distinct_datasets():  ", distinct_datasets(x), width = width))
  cat("\n")
  cat(format_vector(prefix = "  distinct_locations(): ", distinct_locations(x), width = width))
  cat("\n")
  cat(format_vector(prefix = "  distinct_params():    ", distinct_params(x), width = width))
  cat("\n")
  cat(format_vector(prefix = "  src_tbls():           ", src_tbls(x), width = width))
  
  cat("\n\ntbl_data() %>% head():\n")
  
  print(utils::head(tbl_data(x)), width = width)
  invisible(x)
}

#' @export
#' @rdname print.mudata
summary.mudata <- function(object, ...) {
  data <- tbl_data(object)
  data_head <- utils::head(data) %>% dplyr::collect()
  
  # empty data, empty summary
  if(nrow(data_head) == 0) {
    return(tibble::tibble(param = character(0), location = character(0), dataset = character(0)))
  }
  
  # numeric, non sql data
  n <- NULL; rm(n); value <- NULL; rm(value); sd <- NULL; rm(sd)
  if(is.numeric(data_head$value) && !inherits(data, "tbl_sql")) {
    df <- data %>%
      dplyr::group_by_at(dplyr::vars("param", "location", "dataset")) %>%
      dplyr::summarise(mean_value = mean(value, na.rm = TRUE), 
                       sd_value = stats::sd(value, na.rm = TRUE), 
                       n = n(), 
                       n_NA = sum(is.na(value)))
  } else if(is.numeric(data_head$value) && inherits(data, "tbl_sql")) {
    df <- data %>%
      dplyr::group_by_at(dplyr::vars("param", "location", "dataset")) %>%
      dplyr::summarise(mean_value = mean(value), 
                       sd_value = sd(value), 
                       n = n(), 
                       n_NA = sum(is.na(value)))
  } else {
    df <- data %>%
      dplyr::group_by_at(dplyr::vars("param", "location", "dataset")) %>%
      dplyr::summarise(n = n())
  }
  
  df %>% dplyr::ungroup() %>% dplyr::collect()
}

format_vector <- function(x, width = NULL, quote = '"', prefix = "") {
  if(is.null(width)) {
    width <- getOption("width")
  }
  # use only non-prefix space
  effective_width <- width - nchar(prefix)
  
  # if zero length, use <none>
  if(length(x) == 0) return(paste0(prefix, "<none>"))
  
  # try to fit as many possible values into to one line as possible
  out_len <- length(x)
  while(out_len > 0) {
    out_x <- x[seq_len(out_len)]
    if((length(x) - length(out_x)) > 0) {
      out_more <- sprintf(" ... and %s more", length(x) - length(out_x))
    } else {
      out_more <- ""
    }
    out_chars <- paste0(quote, out_x, quote, collapse = ", ")
    out_text <- paste0(prefix, out_chars, out_more)
    if(nchar(out_text) <= effective_width) return(out_text)
    
    # exponential backoff in case lists are extra long
    if(out_len <= 2) {
      # never print less than 2 values, regardless of width
      return(out_text)
    } else {
      out_len <- round(out_len * 0.75)
    }
  }
  
  # no values will fit within width
  # (should never happen)
  return(sprintf("%s... %s values", prefix, length(x)))
}

