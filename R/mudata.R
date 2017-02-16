
#' Create a MuData object
#'
#' @param data The data table
#' @param locations The locations table (can be omitted)
#' @param params The params table (can be omitted)
#' @param datasets The datasets table (can be omitted)
#' @param columns The columns table (can be omitted)
#' @param dataset.id The dataset id (if datasets is unspecified)
#' @param location.id The location id (if locations is unspecified)
#' @param defactorize Pass \code{FALSE} to keep input columns as factors (may cause errors).
#' @param validate Flag to validate input
#' @param expand.tags Flag to expand JSON tags to columns
#'
#' @return A \code{mudata} object
#' @export
#' 
mudata <- function(data, locations=NULL, params=NULL, datasets=NULL, 
                   columns=NULL, dataset.id='default', location.id='default', 
                   defactorize=TRUE, validate=TRUE, expand.tags=TRUE) {
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
  
  # maintain exisiting qualifiers if present
  quals <- unique(c('dataset', 'location', attr(data, "qualifiers"), 'x', 'param'))
  
  data <- .tagify(data, exnames = c(quals, 'value'), expand=expand.tags)
  tagnames <- names(data)[!(names(data) %in% c(quals, 'value'))]
  data <- as.qtag(data,
                  .qualifiers = quals,
                  .values='value',
                  .tags=tagnames)
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
  
  if(is.null(columns)) {
    cols <- c(names(data), names(params), 
                 names(locations), names(datasets))
    cols <- cols[!(cols %in% c('dataset', 'location', 'param', 'value'))]
    # should maybe check by dataset as different datasets may have
    # unique columns. leaving this out of the .validate for now.
    columns <- expand.grid(dataset=datasets$dataset, column=cols,
                           stringsAsFactors = FALSE)
  } else {
    .checkcols(columns, 'columns', c('dataset', 'column'))
    columns <- .tagify(columns, exnames = c('dataset', 'column'), expand=expand.tags)
    columns$column <- as.character(columns$column)
    columns$dataset <- as.character(columns$dataset)
  }
  
  md <- structure(.Data = list(data=data, locations=locations, params=params, 
                               datasets=datasets, columns=columns),
                    class=c('mudata', 'list'))
  if(validate) {
    .validate(md)
  }
  return(md)
}

.checkcols <- function(df, name, required_cols) {
  missingcols <- required_cols[!(required_cols %in% names(df))]
  if(length(missingcols)>0) stop(sprintf("Table '%s' is missing columns %s",
                                         name,
                                         paste0("'", missingcols, "'", collapse=", ")))
}

.validate <- function(md) {
  # ensure data is summarised
  if(!is.summarised(md$data)) stop('Duplicate data detected')
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
  
  # ensure no duplicates in locations, datasets, params
  . <- NULL; rm(.) # CMD check hack
  if(length(unique(md$datasets$dataset)) != length(md$datasets$dataset)) stop("Duplicate datasets in dataset table")
  dplyr::do(dplyr::group_by_(md$locations, "dataset", "location"), {
    if(nrow(.) > 1) stop("Duplicate location in location table: ", unique(.$dataset), '->', unique(.$location))
    .
  })
  dplyr::do(dplyr::group_by_(md$params, "dataset", "param"), {
    if(nrow(.) > 1) stop("Duplicate parameter in parameters table: ", unique(.$dataset), '->', unique(.$param))
    .
  })
  NULL
}

#' Validate a MUData object
#' 
#' Validates a MUData object by calling \code{stop} when an error is found.
#'
#' @param md An object of class 'mudata'
#'
#' @export
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
    validate = validate
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
         columns=cl, validate=validate, defactorize = defactorize)
}


#' Object summary for a mudata object
#' 
#' Returns a data.frame containing summary (by dataset, location, and param)
#' statistics.
#'
#' @param object A \link{mudata} object
#' @param ... Unused
#' @param digits The number of digits to be used for rounding, or NA to suppress rounding.
#'
#' @return A data.frame containing the summary
#' @export
#'
#' @examples
#' data(longlake2016)
#' summary(longlake2016, digits=NA)
#' summary(longlake2016)
#' 
summary.mudata <- function(object, ..., digits=2) {
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
