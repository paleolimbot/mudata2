
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
    .validate(md)
  }
  return(md)
}

.checkcols <- function(df, name, required_cols) {
  if(!inherits(df, "data.frame")) stop(sprintf("Table '%s' is not a data.frame", name))
  missingcols <- required_cols[!(required_cols %in% names(df))]
  if(length(missingcols)>0) stop(sprintf("Table '%s' is missing columns %s",
                                         name,
                                         paste0("'", missingcols, "'", collapse=", ")))
}

.validate <- function(md) {
  # check that it is a mudata object
  if(!inherits(md, "mudata")) stop("Object is not a 'mudata' object")
  # check columns/classes
  .checkcols(md$locations, 'locations', c('dataset', 'location'))
  .checkcols(md$data, 'data', c('dataset', 'location', 'param', 'x', 'value'))
  .checkcols(md$datasets, 'datasets', 'dataset')
  .checkcols(md$columns, 'columns', c('dataset', 'table', 'column'))
  .checkcols(md$params, 'params', c('dataset', 'param'))
  
  # ensure data is summarised
  lengths <- dplyr::summarise_(dplyr::group_by_(md$data, "dataset", "location", "param", "x"),
                               lengths="length(value)")
  lengths <- lengths[lengths$lengths > 1,]
  if(nrow(lengths) > 0) {
    lengths <- lengths[1:min(nrow(lengths), 10),]
    stop("dataset, location, param, and x do not identify unique rows for:\n",
         paste(lengths$dataset, lengths$location, lengths$param, lengths$x, sep="->", collapse="\n"))
  } 

  locs <- unique(as.character(md$data$location))
  params <- unique(as.character(md$data$param))
  datasets <- unique(as.character(md$data$dataset))
  
  # ensure locs/params/datasets are in the tables
  noinflocs <- locs[!(locs %in% md$locations$location)]
  if(length(noinflocs) > 0) stop("Locations not included in location table: ", paste(noinflocs, collapse=' '))
  noinfparams <- params[!(params %in% md$params$param)]
  if(length(noinfparams) > 0) stop("Params not included in param table: ", paste(noinfparams, collapse=' '))
  noinfds <- datasets[!(datasets %in% md$datasets$dataset)]
  if(length(noinfds) > 0) stop("Datasets not included in dataset table: ", paste(noinfds, collapse=' '))
  
  # ensure there are no extraneous information in information tables
  noinflocs <- md$locations$location[!(md$locations$location %in% locs)]
  if(length(noinflocs) > 0) stop("Locations ", paste(noinflocs, collapse=' '), " not included in data")
  noinfparams <- md$params$param[!(md$params$param %in% params)]
  if(length(noinfparams) > 0) stop("Parameters ", paste(noinfparams, collapse=' '), " not included in data")
  noinfds <- md$datasets$dataset[!(md$datasets$dataset %in% datasets)]
  if(length(noinfds) > 0) stop("Datasets ", paste(noinfds, collapse=' '), " not included in data")
  
  # ensure no duplicates in locations, datasets, params, columns
  . <- NULL; rm(.) # CMD check hack
  if(length(unique(md$datasets$dataset)) != length(md$datasets$dataset)) stop("Duplicate dataset in datasets table")
  dplyr::do(dplyr::group_by_(md$locations, "dataset", "location"), {
    if(nrow(.) > 1) stop("Duplicate location in locations table: ", unique(.$dataset), '->', unique(.$location))
    .
  })
  dplyr::do(dplyr::group_by_(md$params, "dataset", "param"), {
    if(nrow(.) > 1) stop("Duplicate parameter in parameters table: ", unique(.$dataset), '->', unique(.$param))
    .
  })
  dplyr::do(dplyr::group_by_(md$columns, "dataset", "table", "column"), {
    if(nrow(.) > 1) stop("Duplicate column in columns table: ", unique(.$dataset), '->', unique(.$table),  
                         '->', unique(.$column))
    .
  })
  TRUE
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
  invisible(.validate(md))
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

#' Subset a MuData object
#'
#' @param x The object to subset
#' @param datasets Vector of datasets to include
#' @param params  Vector of parameters to include
#' @param locations Vector of locations to include
#' @param validate Flag to validate output
#' @param defactorize Pass \code{FALSE} to keep input columns as factors (may cause errors).
#' @param ... Aguments to/from methods
#'
#' @return A subsetted MuData object
#' @export
#'
subset.mudata <- function(x, datasets=NULL, params=NULL, locations=NULL, validate=TRUE, 
                          defactorize=FALSE, ...) {
  if(is.null(datasets)) {
    datasets <- unique(x$datasets$dataset)
  }
  if(is.null(locations)) {
    locations <- unique(x$locations$location)
  }
  if(is.null(params)) {
    params <- unique(x$params$param)
  }
  dta <- x$data[(x$data$dataset %in% datasets) & 
                  (x$data$location %in% locations) & 
                  (x$data$param %in% params),]
  dta$dataset <- factor(dta$dataset, levels=datasets)
  dta$param <- factor(dta$param, levels=params)
  dta$location <- factor(dta$location, levels=locations)
  
  # redefine params, locations, datasets to reflect subsetted data
  params <- unique(dta$param)
  locations <- unique(dta$location)
  datasets <- unique(dta$dataset)
  
  pm <- x$params[x$params$param %in% params,]
  lc <- x$locations[x$locations$location %in% locations,]
  cl <- x$columns[x$columns$dataset %in% datasets,]
  ds <- x$datasets[x$datasets$dataset %in% datasets,]
  
  mudata(data=dta, locations=lc, params=pm, datasets=ds, 
         columns=cl, validate=validate, defactorize = defactorize, retype=FALSE)
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
#' summary(kentvillegreenwood, digits=2)
#' summary(kentvillegreenwood)
#' print(kentvillegreenwood, digits=2)
#' 
summary.mudata <- function(object, ..., digits=NA) {
  df <- data.frame(dplyr::summarise_(dplyr::group_by_(object$data, "dataset", "location", "param"),
                    Min="min(value, na.rm=TRUE)", 
                    Median="stats::median(value, na.rm=TRUE)",
                    Mean="mean(value, na.rm=TRUE)",
                    Max="max(value, na.rm=TRUE)",
                    n="length(value)",
                    NAs="sum(is.na(value))"),
             check.names = FALSE)
  if(!is.na(digits)) {
    numbers <- data.frame(lapply(df[4:9], round, digits))
    return(cbind(df[1:3], numbers))
  } else {
    return(df)
  }
}

#' @rdname summary.mudata
#' @export
print.mudata <- function(x, ..., digits=4) {
  if(is.numericish(x$data$value)) {
    sumobj <- dplyr::summarise_(dplyr::group_by_(x$data, "param"), min="min(value, na.rm=TRUE)",
                                max="max(value, na.rm=TRUE)")
    paramsummary <- paste("... ... ", sumobj$param, " from ", 
                          format(sumobj$min, digits=digits), " to ", 
                          format(sumobj$max, digits=digits))
  } else {
    sumobj <- dplyr::summarise_(dplyr::group_by_(x$data, "param"), 
                                vals="paste(utils::head(value), collapse=', ')")
    paramsummary <- paste("... ... ", sumobj$param, ": ", sumobj$vals, "...")
  }
  datasets <- x$datasets$dataset
  locations <- x$locations$location
  params <- unique(x$params$param)
  if(is.numericish(x$data$x)) {
    xrange <- range(x$data$x)
    xrangesum <- sprintf("from %s to %s", xrange[1], xrange[2])
  } else {
    xrangesum <- paste(paste(utils::head(x$data$x), collapse = ", "), "...")
  }
  
  lines <- c(sprintf("A mudata object with %d dataset(s), %d location(s), %d param(s), and %d data points",
                     length(datasets), length(locations), length(params), nrow(x$data)),
             sprintf("... datasets: %s", paste(datasets, collapse=", ")),
             sprintf("... locations: %s", paste(locations, collapse=", ")),
             sprintf("... x: %s", xrangesum),
             "... params:", 
             paramsummary)
  cat(paste(lines, collapse="\n"))
  invisible(x)
}
