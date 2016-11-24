
#' Create a MuData object
#'
#' @param data The data table
#' @param locations The locations table (can be omitted)
#' @param params The params table (can be omitted)
#' @param datasets The datasets table (can be omitted)
#' @param dataset.id The dataset id (if datasets is unspecified)
#' @param location.id The location id (if locations is unspecified)
#' @param validate Flag to validate input
#' @param expand.tags Flag to expand JSON tags to columns
#'
#' @return A \code{mudata} object
#' @export
#' 
mudata <- function(data, locations=NULL, params=NULL, datasets=NULL, 
                   dataset.id='default', location.id='default', 
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
  
  data <- .tagify(data, exnames = c('dataset', 'location', 'x', 'param', 'value'), expand=expand.tags)
  tagnames <- names(data)[!(names(data) %in% c('dataset', 'location', 'x', 'param', 'value'))]
  data <- as.qtag(data,
                  .qualifiers = c('dataset', 'location', 'x', 'param'),
                  .values='value',
                  .tags=tagnames)
  if(is.null(datasets)) {
    datasets <- data.frame(dataset=unique(as.character(data$dataset)), stringsAsFactors = FALSE)
    if(!expand.tags) {
      datasets$tags <- '{}'
    }
  } else {
    datasets <- .tagify(datasets, exnames = 'dataset', expand=expand.tags)
    datasets$dataset <- as.character(datasets$dataset)
  }
  
  if(is.null(locations)) {
    locations <- expand.grid(dataset=datasets$dataset, location=unique(as.character(data$location)),
                             stringsAsFactors = FALSE)
    if(!expand.tags) {
      locations$tags <- '{}'
    }
  } else {
    locations <- .tagify(locations, exnames = c('dataset', 'location'), expand=expand.tags)
    locations$dataset <- as.character(locations$dataset)
    locations$location <- as.character(locations$location)
  }
  
  if(is.null(params)) {
    params <- expand.grid(dataset=datasets$dataset, param=c('x', unique(as.character(data$param))), 
                          stringsAsFactors = FALSE)
    if(!expand.tags) {
      params$tags <- '{}'
    }
  } else {
    params <- .tagify(params, exnames = c('dataset', 'param'), expand=expand.tags)
    params$param <- as.character(params$param)
    params$dataset <- as.character(params$dataset)
  }
  md <- NULL
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
  if(length(unique(md$datasets$dataset)) != length(md$datasets$dataset)) stop("Duplicate datasets in dataset table")
  dplyr::do(dplyr::group_by(md$locations, dataset, location), {
    if(nrow(.) > 1) stop("Duplicate location in location table: ", unique(.$dataset), '->', unique(.$location))
    .
  })
  dplyr::do(dplyr::group_by(md$params, dataset, param), {
    if(nrow(.) > 1) stop("Duplicate parameter in parameters table: ", unique(.$dataset), '->', unique(.$param))
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
    data = do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$data)),
    locations = unique(do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$locations))),
    params = unique(do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$params))),
    datasets = unique(do.call(plyr::rbind.fill, lapply(mudatas, function(m) m$datasets))),
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

#' Autoplot a mudata object
#'
#' If you get a \code{seq...finite values} error, you may have to check for params
#' that have all non-detect values. This can be done with the dplyr summarise function
#' (\code{group_by(dataset, param) / summarise(allnd=all(is.na(value))) / data.frame()}).
#' The \code{subset} argument is quite powerful for filtering, but does not affect the order
#' of appearance. For this, use \link{subset.mudata}.
#'
#' @param x A \link{mudata} object
#' @param ... Passed on to \code{plotgg.qtag.long}
#'
#' @return A ggplot object
#'
#' @export
#'
plotgg.mudata <- function(x, ...) {
  plotgg.qtag.long(x$data, ...)
}

#' Read/Write a MuData zip file
#'
#' @param md a mudata object
#' @param zipfile file to read/write (can also be a directory)
#' @param validate flag to validate mudata object upon read
#' @param expand.tags flag to expand tags to columns
#' @param ... passed to read/write.csv
#'
#' @export
#'
write.mudata <- function(md, zipfile, overwrite=FALSE, expand.tags=TRUE, ...) {
  if(missing(zipfile)) stop("Parameter zipfile is required")
  if(file.exists(zipfile)) {
    if(overwrite) {
      unlink(zipfile)
    } else {
      stop("File ", zipfile, ' exists...pass overwrite=TRUE to continue')
    }
  }
  zipfolder <- tempfile()
  dir.create(zipfolder)
  if(expand.tags) {
    md <- expand.tags(md)
  } else {
    md <- condense.tags(md)
  }
  write.csv(md$data, file.path(zipfolder, "data.csv"), row.names = FALSE, ...)
  write.csv(md$locations, file.path(zipfolder, "locations.csv"), row.names = FALSE, ...)
  write.csv(md$params, file.path(zipfolder, "params.csv"), row.names = FALSE, ...)
  write.csv(md$datasets, file.path(zipfolder, "datasets.csv"), row.names = FALSE, ...)
  cwd <- getwd()
  setwd(zipfolder)
  tryCatch(zip("zipfile.zip", c("data.csv", "locations.csv", "params.csv", "datasets.csv")),
           error=function(err) {
             setwd(cwd)
             unlink(zipfolder, recursive = TRUE)
             stop(err)
           })
  setwd(cwd)
  tmpzip <- file.path(zipfolder, "zipfile.zip")
  file.copy(tmpzip, zipfile)
  unlink(tmpzip)
  unlink(zipfolder, recursive = TRUE)
}

#' @rdname write.mudata
#' @export
read.mudata <- function(zipfile, validate=TRUE, expand.tags=TRUE, ...) {
  tmpfold <- tempfile()
  deleteOnExit <- TRUE
  if(dir.exists(zipfile)) {
    tmpfold <- zipfile
    deleteOnExit <- FALSE
  } else {
    unzip(zipfile, exdir = tmpfold)
  }
  datafile <- list.files(tmpfold, pattern='data.csv', full.names = TRUE, recursive = TRUE)
  locationsfile <- list.files(tmpfold, pattern='locations.csv', full.names = TRUE, recursive = TRUE)
  paramsfile <- list.files(tmpfold, pattern='params.csv', full.names = TRUE, recursive = TRUE)
  datasetsfile <- list.files(tmpfold, pattern='datasets.csv', full.names = TRUE, recursive = TRUE)
  
  md <- tryCatch({
    if(length(datafile) == 0) stop('data.csv not found')
    data <- read.csv(path.expand(datafile[1]), stringsAsFactors = FALSE, ...)
    if(length(locationsfile) == 0) {
      locations <- NULL
    } else {
      locations <- read.csv(path.expand(locationsfile[1]), stringsAsFactors = FALSE, ...)
    }
    if(length(paramsfile) == 0) {
      params <- NULL
    } else {
      params <- read.csv(path.expand(paramsfile[1]), stringsAsFactors = FALSE, ...)
    }
    if(length(datasetsfile) == 0) {
      datasets <- NULL
    } else {
      datasets <- read.csv(path.expand(datasetsfile[1]), stringsAsFactors = FALSE, ...)
    }
    
    mudata(data=data, locations=locations, params=params, datasets = datasets,
           expand.tags=expand.tags, validate=validate)
  }, error=function(err) {
    if(deleteOnExit) {
      unlink(tmpfold, recursive = TRUE)
    }
    stop(err)
  })
  if(deleteOnExit) {
    unlink(tmpfold, recursive = TRUE)
  }
  return(md)
}

