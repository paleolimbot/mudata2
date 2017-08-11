#' Rename a column in an object
#'
#' Essentially a thin convenience wrapper around \code{plyr::rename(x, list(...))}.
#' 
#' @param .data An object that has columns that can be renamed
#' @param ... Key/value pairs to replace in the form \code{oldval="newval"}
#' @param warn_missing Print a message if any old names are not actually present in x
#' @param warn_duplicated Print a message if any name appears more than once in x 
#'   after the operation.
#'
#' @return A copy of the modified object
#' @export
#'
#' @examples
#' data(pocmaj)
#' rename_cols(pocmaj, Ca="Calcium")
#'
rename_cols <- function(.data, ..., warn_missing=TRUE, warn_duplicated=TRUE) UseMethod("rename_cols")

#' @export
#' @rdname rename_cols
rename_cols.default <- function(.data, ..., warn_missing=TRUE, warn_duplicated=TRUE) {
  plyr::rename(.data, list(...), warn_missing = warn_missing, warn_duplicated = warn_duplicated)
}


#' Replace/rename values in a vector
#' 
#' This function replaces character values with new character values, which
#' is useful when performing rename operations when values are held in character vectors.
#'
#' @param x Vector of values to replace
#' @param ... Key/value pairs in the form \code{oldvalue="newvalue"}
#' @param defaultValue A vector of values to use as the default should the value not
#'   be found in \code{...}
#' @param warn_missing Print a message if any old names are not actually present in x
#'
#' @return A vector with values replaced
#' @export
#'
#' @examples
#' x <- c("fish", "fish", "fish", "whistle")
#' rename_values(x, fish="newfish")
#' rename_values(x, whistle="newwhistle")
#' rename_values(x, fish="newfish", defaultValue="not a fish")
#'
rename_values <- function(x, ..., defaultValue=x, warn_missing=TRUE) {
  replacer <- list(...)
  if(length(replacer) == 0) return(x)
  
  replacernames <- names(replacer)
  if(is.null(replacernames) && (length(replacer) == 1)) {
    replacer <- replacer[[1]]
    replacernames <- names(replacer)
  }
  notinvector <- replacernames[!(replacernames %in% x)]
  if(warn_missing && (length(notinvector) > 0)) 
    message("Not all values were found: ", paste(notinvector, collapse=", "))
  if(length(defaultValue) != length(x)) {
    defaultValue <- rep(defaultValue, length.out=length(x))
  }
  sapply(1:length(x), function(i) {
    v <- x[i]
    ifelse(v %in% replacernames, replacer[[v]], defaultValue[i])
  })
}

#' Rename datasets, params, locations, and columns
#' 
#' Provides a convenient way to rename datasets, params, locations, and columns
#' such that their usage with a mudata object remains consistent.
#'
#' @param md A \link{mudata} object
#' @param .data A \link{mudata} object
#' @param ... Key/value pairs in the form \code{"oldvalue"="newvalue"}
#' @param apply_to The tables which the rename operation should consider
#' @param warn_missing Print a message if any old names are not actually present
#' @param warn_duplicated Print a message if any name appears more than once in x 
#'   after the operation.
#'
#' @return A modified \link{mudata} object.
#' @export
#'
#' @examples
#' data(kentvillegreenwood)
#' md2 <- rename_datasets(kentvillegreenwood, ecclimate="avalley")
#' validate_mudata(md2)
#' md2 <- rename_locations(kentvillegreenwood, "GREENWOOD A"="Greenwood")
#' validate_mudata(md2)
#' md2 <- rename_params(kentvillegreenwood, maxtemp="Maximum Temperature")
#' validate_mudata(md2)
#' md2 <- rename_cols(kentvillegreenwood, latitude="lat", longitude="lon")
#' validate_mudata(md2)
#' 
rename_datasets <- function(md, ..., apply_to=c("data", "locations", "params", "datasets", "columns"),
                            warn_missing=TRUE) {
  for(dfname in apply_to) {
    md[[dfname]]$dataset <- rename_values(md[[dfname]]$dataset, ..., warn_missing=warn_missing)
  }
  return(md)
}

#' @rdname rename_datasets
#' @export
rename_params <- function(md, ..., apply_to=c("data", "params"), warn_missing=TRUE) {
  for(dfname in apply_to) {
    md[[dfname]]$param <- rename_values(md[[dfname]]$param, ..., warn_missing=warn_missing)
  }
  return(md)
}

#' @rdname rename_datasets
#' @export
rename_locations <- function(md, ..., apply_to=c("data", "locations"), warn_missing=TRUE) {
  for(dfname in apply_to) {
    md[[dfname]]$location <- rename_values(md[[dfname]]$location, ..., warn_missing=warn_missing)
  }
  return(md)
}

#' @rdname rename_datasets
#' @export
rename_cols.mudata <- function(.data, ..., apply_to=c("datasets", "locations", "params", "data", "columns"),
                               warn_missing=FALSE, warn_duplicated=TRUE) {
  for(dfname in apply_to) {
    .data[[dfname]] <- rename_cols(.data[[dfname]], ..., warn_missing=warn_missing, 
                               warn_duplicated=TRUE)
  }
  .data$columns$column <- rename_values(.data$columns$column, ..., warn_missing=warn_missing)
  return(.data)
}
