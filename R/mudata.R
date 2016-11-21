
#' Create a MuData object
#'
#' @param data The data table
#' @param locations The locations table (can be omitted)
#' @param params The params table (can be omitted)
#' @param datasets The datasets table (can be omitted)
#' @param dataset.id The dataset id (if datasets is unspecified)
#' @param location.id The location id (if locations is unspecified)
#' @param validate Flag to validate input
#'
#' @return A \code{mudata} object
#' @export
#' 
mudata <- function(data, locations, params, datasets, 
                   dataset.id='default', location.id='default', 
                   defactorize=TRUE, validate=TRUE) {
  if(!('dataset' %in% names(data))) {
    data$dataset <- dataset_id
  } else if(defactorize) {
    data$dataset <- as.character(data$dataset)
  }
  if(!('location' %in% names(data))) {
    data$location <- location_id
  } else if(defactorize) {
    data$location <- as.character(data$location)
  }
  
  data <- as.qtag(.tagify(data, exnames = c('dataset', 'location', 'x', 'param', 'value')),
                  .qualifiers = c('dataset', 'location', 'x', 'param'),
                  .values='value',
                  .tags='tags')
  if(missing(datasets)) {
    datasets <- data.frame(dataset=unique(data$dataset), tags='{}', stringsAsFactors = FALSE)
  } else {
    datasets <- .tagify(datasets, exnames = 'dataset')
    datasets$dataset <- as.character(datasets$dataset)
  }
  
  if(missing(locations)) {
    locations <- expand.grid(dataset=datasets$dataset, location=unique(data$location),
                             stringsAsFactors = FALSE)
    locations$tags <- '{}'
  } else {
    locations <- .tagify(locations, exnames = c('dataset', 'location'))
    locations$dataset <- as.character(locations$dataset)
    locations$location <- as.character(locations$location)
  }
  
  if(missing(params)) {
    params <- expand.grid(dataset=datasets$dataset, param=unique(data$param), 
                          stringsAsFactors = FALSE)
    params$tags <- '{}'
  } else {
    params <- .tagify(params, exnames = c('dataset', 'param'))
    params$param <- as.character(params$param)
    params$datset <- as.character(params$dataset)
  }
  md <- structure(.Data = list(data=data, locations=locations, params=params, datasets=datasets),
            class=c('mudata', 'list'))
  if(validate) {
    .validate(md)
  }
  return(md)
}

.validate <- function(md) {
  # ensure data is summarised
  if(!is.summarised(md$data)) stop('Duplicate data detected')
  locs <- unique(as.character(md$data$location))
  params <- c(unique(as.character(md$data$param)), 'x')
  datasets <- unique(as.character(md$data$dataset))
  # ensure locs/params/datasets are in the tables
  noinflocs <- locs[!(locs %in% md$locations$location)]
  if(length(noinflocs) > 0) stop("Locations not included in location table: ", paste(noinflocs))
  noinfparams <- params[!(params %in% md$params$param)]
  if(length(noinfparams) > 0) stop("Params not included in param table: ", paste(noinfparams))
  noinfds <- datasets[!(datasets %in% md$datasets$dataset)]
  if(length(noinfds) > 0) stop("Datasets not included in dataset table: ", paste(noinfds))
  
  # ensure there are no extraneous information in information tables
  noinflocs <- md$locations$location[!(md$locations$location %in% locs)]
  if(length(noinflocs) > 0) stop("Locations ", paste(noinflocs), " not included in data")
  noinfparams <- md$params$param[!(md$params$param %in% params)]
  if(length(noinfparams) > 0) stop("Parameters ", paste(noinfparams), " not included in data")
  noinfds <- md$datasets$dataset[!(md$datasets$dataset %in% datasets)]
  if(length(noinfds) > 0) stop("Datasets ", noinfds, " not included in data")
  
  # ensure no duplicates in locations, datasets, params
  if(length(unique(md$datasets$dataset)) != 1) stop("Duplicate datasets in dataset table")
  dplyr::do(dplyr::group_by(md$locations, dataset, location), {
    if(nrow(.) > 1) stop("Duplicate location in location table: ", unique(.$location))
    .
  })
  dplyr::do(dplyr::group_by(md$params, dataset, param), {
    if(nrow(.) > 1) stop("Duplicate parameter in parameters table: ", unique(.$param))
    .
  })
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
    data = do.call(rbind, lapply(mudatas, function(m) m$data)),
    locations = do.call(rbind, lapply(mudatas, function(m) m$locations)),
    params = do.call(rbind, lapply(mudatas, function(m) m$params)),
    datasets = do.call(rbind, lapply(mudatas, function(m) m$datasets)),
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
#' @param ... Aguments to/from methods
#'
#' @return A subsetted MuData object
#' @export
#'
subset.mudata <- function(x, datasets=NULL, params=NULL, locations=NULL, validate=TRUE, ...) {
  if(is.null(datasets)) {
    datasets <- unique(x$datasets$dataset)
  }
  if(is.null(locations)) {
    locations <- unique(x$locations$location)
  }
  if(is.null(params)) {
    params <- unique(x$params$param)
  }
  ds <- x$datasets[x$datasets$dataset %in% datasets,]
  pm <- x$params[x$params$param %in% c(params, 'x'),]
  lc <- x$locations[x$locations$location %in% locations,]
  dta <- x$data[(x$data$dataset %in% datasets) & 
                  (x$data$location %in% locations) & 
                  (x$data$param %in% params),]
  dta$dataset <- factor(dta$dataset, levels=datasets)
  dta$param <- factor(dta$param, levels=params)
  dta$location <- factor(dta$location, levels=locations)
  
  mudata(data=dta, locations=lc, params=pm, datasets=ds, validate=validate, defactorize = FALSE)
}

.tagify <- function(df, exnames) {
  dfnames <- names(df)
  if('tags' %in% dfnames) {
    df$tags <- as.character(df$tags)
    df$tags[is.na(df$tags)] <- '{}'
  } else {
    tagnames <- dnames[!(dfnames %in% exnames)]
    if(length(tagnames) > 0) {
      df$tags <- sapply(1:nrow(dxmelt), function(i) {
        vals <- sapply(tagnames, function(name) {
          df[[name]][i]
        })
        vals <- vals[!is.na(vals)]
        paste0('{', paste0('"', names(vals), '": ', vals, collapse=", "), '}')
      })
    } else {
      df$tags <- '{}'
    }
  }
  return(df[c(exnames, 'tags')])
}

