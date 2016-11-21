
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
                   dataset.id='default', location.id='default', validate=TRUE) {
  if(!('dataset' %in% names(data))) {
    data$dataset <- dataset_id
  } else {
    data$dataset <- as.character(data$dataset)
  }
  if(!('location' %in% names(data))) {
    data$location <- location_id
  } else {
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
  if(!is.summarised(md$data)) stop('Data must be summarised')
  locs <- unique(md$data$location)
  params <- c(unique(md$data$param), 'x')
  datasets <- unique(md$data$dataset)
  # ensure locs/params/datasets are in the tables
  noinflocs <- locs[!(locs %in% md$locations$location)]
  if(length(noinflocs) > 0) stop("Locations not included in location table: ", paste(noinflocs))
  noinfparams <- params[!(params %in% md$params$param)]
  if(length(noinfparams) > 0) stop("Locations not included in location table: ", paste(noinfparams))
  noinfds <- datasets[!(datasets %in% md$datasets$dataset)]
  if(length(noinfds) > 0) stop("Locations not included in location table: ", paste(noinfds))
  
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

