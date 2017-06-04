
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
#'   'datset', and 'location'. If omitted, it will be created automatically using all unique
#'   dataset/location combinations.
#' @param params The params table, which is a data frame containing the columns (at least)
#'   'datset', and 'param'. If omitted, it will be created automatically using all unique
#'   dataset/param combinations.
#' @param datasets The datasets table, which is a data frame containing the column (at least)
#'   'dataset'. If omitted, it will be generated automatically using all unique datasets.
#' @param columns The columns table, which is a data frame containing the columns (at least)
#'   'dataset', 'table', and 'column'. If omitted, it will be created automatically using 
#'   all dataset/table/column combinations.
#' @param dataset.id The dataset id to use if the datasets table is omitted.
#' @param location.id The location id if the locations table is omitted.
#' @param defactorize Pass \code{FALSE} to suppress coersion of 'dataset', 'location', and 'param'
#'   columns to type 'character'.
#' @param validate Pass \code{FALSE} to skip validation of input tables.
#' @param expand.tags Pass \code{FALSE} to collapse non-required columns to a single column
#'   (called 'tags'), with key/value pairs in JSON format. See \link{expand.tags}.
#' @param retype Pass \code{TRUE} to retype columns based on the 'type' column of the 'columns'
#'   table. This is useful when reading data from disk, where date/time columns may be stored
#'   as text.
#'
#' @return A \code{mudata} object
#' @export
#' 
#' @examples
#' library(reshape2)
#' library(dplyr)
#' data(pocmaj)
#' 
#' # melt data and summarise replicates
#' datatable <- pocmaj %>%
#'   melt(id.vars=c("core", "depth"), variable.name="param") %>%
#'   group_by(core, param, depth) %>%
#'   summarise(sd=mean(value), value=mean(value)) %>%
#'   rename.cols("depth"="x", "core"="location")
#'
#' # create mudata object
#' md <- mudata(datatable)
#' summary(md)
#' plot(md, yvar="x", geom=c("path", "point"))
#' 
mudata <- function(data, locations=NULL, params=NULL, datasets=NULL, 
                   columns=NULL, dataset.id='default', location.id='default', 
                   defactorize=TRUE, validate=TRUE, expand.tags=TRUE, retype=FALSE) {
  # ok to be missing cols at first
  .checkcols(data, 'data', NULL)
  if(!('dataset' %in% names(data))) {
    data$dataset <- dataset.id
  } else if(defactorize) {
    data$dataset <- as.character(data$dataset)
  }
  
  if(!('location' %in% names(data))) {
    data$location <- location.id
  } else if(defactorize) {
    data$location <- as.character(data$location)
  }
  
  if(!('param' %in% names(data))) {
    stop('Column "param" required in data')
  } else if(defactorize) {
    data$param <- as.character(data$param)
  }
  
  # now it is not ok to be missing any cols
  .checkcols(data, 'data', c('dataset', 'location', 'param', 'x', 'value'))
  data <- .tagify(data, exnames = c('dataset', 'location', 'param', 'x', 'value'), expand=expand.tags)
  tagnames <- names(data)[!(names(data) %in% c('dataset', 'location', 'param', 'x', 'value'))]
  # reorder columns in data
  data <- data[c('dataset', 'location', 'param', 'x', 'value', tagnames)]
  
  if(is.null(datasets)) {
    datasets <- data.frame(dataset=unique(as.character(data$dataset)), stringsAsFactors = FALSE)
    if(!expand.tags) {
      datasets$tags <- '{}'
    }
  } else {
    # sometimes datasets can just be a character vector because of weird R
    # subsetting operaions. if this is true, create a data frame
    if(!("data.frame" %in% class(datasets))) {
      datasets <- data.frame(dataset=datasets, stringsAsFactors = FALSE)
    }
    # check columns before tagify
    .checkcols(datasets, 'datasets', 'dataset')
    datasets <- .tagify(datasets, exnames = 'dataset', expand=expand.tags)
    datasets$dataset <- as.character(datasets$dataset)
  }
  
  if(is.null(locations)) {
    locations <- data.frame(dplyr::do(dplyr::group_by_(data, "dataset", "location"), .dummy=1))
    locations$.dummy <- NULL
    if(!expand.tags) {
      locations$tags <- '{}'
    }
  } else {
    # check columns before tagify
    .checkcols(locations, 'locations', c('dataset', 'location'))
    locations <- .tagify(locations, exnames = c('dataset', 'location'), expand=expand.tags)
    locations$dataset <- as.character(locations$dataset)
    locations$location <- as.character(locations$location)
  }
  
  if(is.null(params)) {
    params <- data.frame(dplyr::do(dplyr::group_by_(data, "dataset", "param"), .dummy=1))
    params$.dummy <- NULL
    if(!expand.tags) {
      params$tags <- '{}'
    }
  } else {
    # check columns before tagify
    .checkcols(params, 'params', c('dataset', 'param'))
    params <- .tagify(params, exnames = c('dataset', 'param'), expand=expand.tags)
    params$param <- as.character(params$param)
    params$dataset <- as.character(params$dataset)
  }
  
  mdlist <- list(data=data, locations=locations, params=params, 
                 datasets=datasets)
  if(is.null(columns)) {
    allcols <- expand.grid(dataset=datasets$dataset, 
                           table=c("data", "locations", "params", "datasets"),
                           stringsAsFactors = FALSE)
    allcols <- plyr::adply(allcols, .margins=1, .fun=function(row) {
      df <- mdlist[[row$table]]
      df <- df[df$dataset==row$dataset,]
      cols <- names(df)
      cols <- cols[sapply(cols, function(col) !all(is.na(df[[col]])))]
      types <- sapply(cols, function(col) class(df[[col]])[1])
      return(data.frame(column=cols, type=types, stringsAsFactors = FALSE))
    })
    columns <- data.frame(allcols, stringsAsFactors = FALSE)
    columns <- columns[!(columns$column %in% c('dataset', 'param', 'location')),]
    columns <- .tagify(columns, exnames=c('dataset', 'table', 'column'), expand=expand.tags)
    mdlist$columns <- columns
  } else {
    .checkcols(columns, 'columns', c('dataset', 'table', 'column'))
    columns <- .tagify(columns, exnames = c('dataset', 'table', 'column'), expand=expand.tags)
    columns$column <- as.character(columns$column)
    columns$dataset <- as.character(columns$dataset)
    
    # try to retype data based on column types
    if(retype && ("type" %in% names(columns))) {
      mdlist <- sapply(names(mdlist), function(table) {
        typesdf <- unique(columns[columns$table == table, c("table", "column", "type")])
        df <- mdlist[[table]]
        for(col in names(df)) {
          types <- typesdf$type[typesdf$column == col]
          if((length(types) == 1) && !(types %in% class(df[[col]])) && 
             methods::existsFunction(paste0("as.", types))) {
            message("Retyping column '", col, "' from '", class(df[[col]])[1], "' to '", types, "'")
            df[[col]] <- get(paste0("as.", types))(df[[col]])
          }
        }
        return(df)
      }, USE.NAMES = TRUE, simplify=FALSE)
    }
    mdlist$columns <- columns
  }
  
  md <- structure(.Data = mdlist,
                    class=c('mudata', 'list'))
  if(validate) {
    validate.mudata(md)
  }
  return(md)
}

#' @rdname mudata
#' @export
mudata_remote <- function(data, locations=NULL, params=NULL, datasets=NULL, columns=NULL,
                          dataset.id='default') {
  # check data object
  .checkcols(data, 'data', c('location', 'param', 'x', 'value'))
  # if there is no dataset column, use mutate to create one
  if(!('dataset' %in% colnames(data))) {
    data <- dplyr::mutate(data, dataset = dataset.id)
  }
  # move dataset, location, param, x to the front
  data <- dplyr::select_(data, "dataset", "location", "param", "x", "value", dplyr::everything())
  tagnames <- colnames(data)[!(colnames(data) %in% c('dataset', 'location', 'param', 'x', 'value'))]
  
  # check datasets object
  if(is.null(datasets)) {
    # create a derived table for datasets
    datasets <- dplyr::distinct(dplyr::select_(data, "dataset"))
  } else {
    .checkcols(datasets, 'datasets', 'dataset')
    datasets <- dplyr::select_(datasets, "dataset", dplyr::everything())
  }
  
  # check locations object
  if(is.null(locations)) {
    locations <- dplyr::distinct(dplyr::select_(data, "dataset", "location"))
  } else {
    .checkcols(locations, 'locations', c('dataset', 'location'))
    locations <- dplyr::select_(locations, "dataset", "location", dplyr::everything())
  }
  
  # check params object
  if(is.null(params)) {
    params <- dplyr::distinct(dplyr::select_(data, "dataset", "param"))
  } else {
    .checkcols(params, 'params', c('dataset', 'param'))
  }
  
  # check columns object
  if(is.null(columns)) {
    # generate a table of all columns
    dataset_ids <- dplyr::collect(dplyr::distinct(dplyr::select_(datasets, "dataset")))$dataset
    allcols <- expand.grid(dataset=dataset_ids, 
                           table=c("data", "locations", "params", "datasets"),
                           stringsAsFactors = FALSE)
    columns <- plyr::adply(allcols, 1, function(row) {
      table <- dplyr::collect(utils::head(get(row$table)))
      tibble::tibble(column = colnames(table), 
                     type = vapply(table, function(x) class(x)[1], character(1)))
    })
  } else {
    .checkcols(columns, 'columns', c('dataset', 'table', 'column'))
  }
  
  # return list of tables
  structure(list(data = data, locations = locations, params = params,
                 datasets = datasets, columns = columns),
            class = c("mudata_remote", "mudata", "list"))
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
mudata_db <- function(db, data = "data", locations = "locations", params = "params", 
                      datasets = "datasets", columns = "columns") {
  if(!inherits(db, "src_sql")) stop("'db' must be an 'src_sql'")
  mudata_remote(
    data = dplyr::tbl(db, data),
    locations = if(is.null(locations)) NULL else dplyr::tbl(db, locations),
    params = if(is.null(params)) NULL else dplyr::tbl(db, params),
    datasets = if(is.null(datasets)) NULL else dplyr::tbl(db, datasets),
    columns = if(is.null(columns)) NULL else dplyr::tbl(db, columns)
  )
}

#' Validate a MUData object
#' 
#' Validates a MUData object by calling \code{stop} when an error is found.
#'
#' @param md An object of class 'mudata'
#' 
#' @return TRUE, invisibly
#'
#' @export
#' 
#' @examples 
#' data(kentvillegreenwood)
#' validate.mudata(kentvillegreenwood)
#' 
validate.mudata <- function(md) {
  # check that it is a mudata object
  if(!inherits(md, "mudata")) stop("Object is not a 'mudata' object")
  # check columns/classes
  .checkcols(md$locations, 'locations', c('dataset', 'location'))
  .checkcols(md$data, 'data', c('dataset', 'location', 'param', 'x', 'value'))
  .checkcols(md$datasets, 'datasets', 'dataset')
  .checkcols(md$columns, 'columns', c('dataset', 'table', 'column'))
  .checkcols(md$params, 'params', c('dataset', 'param'))
  
  # ensure data is summarised
  .checkunique(md$data, "data", "dataset", "location", "param", "x")
  
  # get unique params, locations, and datasets from the data table
  params <- dplyr::collect(dplyr::distinct(dplyr::select_(md$data, "param")))$param
  locations <- dplyr::collect(dplyr::distinct(dplyr::select_(md$data, "location")))$location
  datasets <- dplyr::collect(dplyr::distinct(dplyr::select_(md$data, "dataset")))$dataset
  
  # get unique params, locations, and datasets from the meta tables
  table_locs <- dplyr::collect(dplyr::distinct(dplyr::select_(md$locations, "location")))$location
  table_params <- dplyr::collect(dplyr::distinct(dplyr::select_(md$params, "param")))$param
  table_datasets <- dplyr::collect(dplyr::distinct(dplyr::select_(md$datasets, "dataset")))$dataset
  
  # ensure locations in data are in the locations table
  noinflocs <- locations[!(locations %in% table_locs)]
  if(length(noinflocs) > 0) stop("Locations not included in location table: ", paste(noinflocs, collapse=' '))
  noinfparams <- params[!(params %in% table_params)]
  if(length(noinfparams) > 0) stop("Params not included in param table: ", paste(noinfparams, collapse=' '))
  noinfds <- datasets[!(datasets %in% table_datasets)]
  if(length(noinfds) > 0) stop("Datasets not included in dataset table: ", paste(noinfds, collapse=' '))
  
  # ensure there are no extraneous information in information tables
  noinflocs <- table_locs[!(table_locs %in% locations)]
  if(length(noinflocs) > 0) stop("Locations ", paste(noinflocs, collapse=' '), " not included in data")
  noinfparams <- table_params[!(table_params %in% params)]
  if(length(noinfparams) > 0) stop("Parameters ", paste(noinfparams, collapse=' '), " not included in data")
  noinfds <- table_datasets[!(table_datasets %in% datasets)]
  if(length(noinfds) > 0) stop("Datasets ", paste(noinfds, collapse=' '), " not included in data")
  
  # ensure no duplicates in locations, datasets, params, columns
  .checkunique(md$locations, "locations", "dataset", "location")
  .checkunique(md$params, "params", "dataset", "param")
  .checkunique(md$datasets, "datasets", "dataset")
  .checkunique(md$columns, "columns", "dataset", "table", "column")
  
  # return TRUE
  invisible(TRUE)
}

.checkunique <- function(tbl, context, ...) {
  lengths <- dplyr::collect(dplyr::distinct(
    dplyr::select_(dplyr::ungroup(dplyr::count_(tbl, c(...))), "n")))$n
  
  if((length(lengths) != 1) || (lengths[1] != 1)) {
    stop(sprintf("Duplicate %s in %s table", context, context))
  }
}

.checkcols <- function(df, name, required_cols) {
  if(!inherits(df, "data.frame") && !inherits(df, "tbl")) {
    stop(sprintf("Table '%s' is not a data.frame", name))
  }
  missingcols <- required_cols[!(required_cols %in% colnames(df))]
  if(length(missingcols)>0) stop(sprintf("Table '%s' is missing columns %s",
                                         name,
                                         paste0("'", missingcols, "'", collapse=", ")))
}

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
  mudata(
    data = do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$data)),
    locations = unique(do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$locations))),
    params = unique(do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$params))),
    datasets = unique(do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$datasets))),
    columns = unique(do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$columns))),
    validate = validate, retype=FALSE
  )
}

#' Subset (filter) a MuData object
#'
#' @param x The object to subset
#' @param datasets Vector of datasets to include
#' @param params  Vector of parameters to include
#' @param locations Vector of locations to include
#' @param ... Aguments to/from methods
#'
#' @return A filtered MuData object
#' @export
#' 
#' @importFrom dplyr filter
#'
filter.mudata <- function(x, ..., datasets=NULL, params=NULL, locations=NULL) {
  
  # cmd hack
  dataset <- NULL; rm(dataset); location <- NULL; rm(location); param <- NULL; rm(param)
  
  # lazily filter data
  dta <- dplyr::filter(x$data, ...)
  if(!is.null(datasets)) {
    dta <- dplyr::filter(x$data, dataset %in% datasets)
  }
  if(!is.null(locations)) {
    dta <- dplyr::filter(x$data, location %in% locations)
  }
  if(!is.null(params)) {
    dta <- dplyr::filter(x$data, param %in% params)
  }
  
  # redefine params, locations, datasets to reflect subsetted data
  params <- dplyr::distinct(dplyr::select(dta, param))$param
  locations <- dplyr::distinct(dplyr::select(dta, location))$location
  datasets <- dplyr::distinct(dplyr::select(dta, dataset))$dataset
  
  pm <- dplyr::filter(x$params, param %in% params)
  lc <- dplyr::filter(x$locations, location %in% locations)
  cl <- dplyr::filter(x$columns, dataset %in% datasets)
  ds <- dplyr::filter(x$datasets, dataset %in% datasets)
  
  # keep class of original
  structure(list(data=dta, locations=lc, params=pm, datasets=ds, 
                 columns=cl), class = class(x))
}

#' @rdname filter.mudata
#' @export
subset.mudata <- function(x, ..., datasets=NULL, params=NULL, locations=NULL) {
  filter.mudata(x, ..., datasets=datasets, params=params, locations=locations)
}


#' Object summary for a mudata object
#' 
#' Returns a data.frame containing summary (by dataset, location, and param)
#' statistics.
#'
#' @param object A \link{mudata} object
#' @param x A \link{mudata} object
#' @param ... Unused
#' @param digits The number of digits to be used for rounding, or NA to suppress rounding.
#'
#' @return A data.frame containing the summary
#' @export
#'
#' @examples
#' data(kentvillegreenwood)
#' summary(kentvillegreenwood)
#' print(kentvillegreenwood, digits=2)
#' 
summary.mudata <- function(object, ..., digits=NA) {
  # cmd hack
  value <- NULL; rm(value); n <- NULL; rm(n)
  df <- data.frame(dplyr::summarise(dplyr::group_by_(object$data, "dataset", "location", "param"),
                         Min=min(value, na.rm=TRUE), 
                         Mean=mean(value, na.rm=TRUE),
                         Max=max(value, na.rm=TRUE),
                         n=n(), ...), check.names = FALSE)
  if(!is.na(digits)) {
    numbers <- data.frame(lapply(df[4:ncol(df)], round, digits))
    return(cbind(df[1:3], numbers))
  } else {
    return(df)
  }
}

#' @rdname summary.mudata
#' @export
summary.mudata_remote <- function(object, ...) {
  # cmd hack
  value <- NULL; rm(value); n <- NULL; rm(n)
  dplyr::summarise(dplyr::group_by_(object$data, "dataset", "location", "param"),
                   Min=min(value), 
                   Mean=mean(value),
                   Max=max(value),
                   n=n(), ...)
}

#' @rdname summary.mudata
#' @export
print.mudata <- function(x, ..., digits=4) {
  if(nrow(dplyr::collect(utils::head(x$data))) == 0) {
    cat("An empty mudata object")
    return(invisible(x))
  }
  
  if(is.numericish(dplyr::collect(utils::head(dplyr::select_(x$data, "value")))$value)) {
    min_func <- if(inherits(x, "mudata_remote")) "min(value)" else "min(value, na.rm=TRUE)"
    max_func <- if(inherits(x, "mudata_remote")) "max(value)" else "max(value, na.rm=TRUE)"
    sumobj <- dplyr::collect(dplyr::summarise_(dplyr::group_by_(x$data, "param"), 
                                min=min_func, max=max_func))
    paramsummary <- paste("... ... ", sumobj$param, " from ", 
                          format(sumobj$min, digits=digits), " to ", 
                          format(sumobj$max, digits=digits))
  } else {
    sumobj <- dplyr::summarise_(dplyr::group_by_(x$data, "param"), 
                                vals="paste(utils::head(value), collapse=', ')")
    paramsummary <- paste("... ... ", sumobj$param, ": ", sumobj$vals, "...")
  }
  
  # get unique params, locations, and datasets from the meta tables
  locations <- dplyr::collect(dplyr::distinct(dplyr::select_(x$locations, "location")))$location
  params <- dplyr::collect(dplyr::distinct(dplyr::select_(x$params, "param")))$param
  datasets <- dplyr::collect(dplyr::distinct(dplyr::select_(x$datasets, "dataset")))$dataset
  
  x_vals <- dplyr::collect(dplyr::distinct(dplyr::select_(x$data, "x")))$x
  
  if(is.numericish(x_vals)) {
    xrange <- range(x_vals)
    xrangesum <- sprintf("from %s to %s", xrange[1], xrange[2])
  } else {
    xrangesum <- paste(paste(utils::head(x$data$x), collapse = ", "), "...")
  }
  
  lines <- c(sprintf("A %s object with %d dataset(s), %d location(s), %d param(s), and %d data points",
                     class(x)[1], length(datasets), length(locations), length(params), nrow(x$data)),
             sprintf("... datasets: %s", paste(datasets, collapse=", ")),
             sprintf("... locations: %s", paste(locations, collapse=", ")),
             sprintf("... x: %s", xrangesum),
             "... params:", 
             paramsummary)
  cat(paste(lines, collapse="\n"))
  invisible(x)
}

#' @rdname summary.mudata
#' @export
collect.mudata_remote <- function(x, ...) {
  structure(lapply(x, dplyr::collect), class = c("mudata", "list"))
}
