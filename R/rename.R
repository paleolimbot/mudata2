#' Replace/rename a column in an object
#'
#' Essentially a thin convenience wrapper around \code{plyr::rename(x, list(...))},
#' except \link{qtag} objects have their qualifiers/tags/values attributes properly modified
#' 
#' @param x An object that has columns that can be renamed
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
#' rename.cols(pocmaj, Ca="Calcium")
#' pocmaj2 <- as.qtag(pocmaj, .qualifiers=c("core", "depth"))
#' pocmaj2 <- rename.cols(pocmaj2, Ca="Calcium")
#' attr(pocmaj2, "values")
#'
rename.cols <- function(x, ..., warn_missing=TRUE, warn_duplicated=TRUE) UseMethod("rename.cols")

#' @export
#' @rdname rename.cols
rename.cols.default <- function(x, ..., warn_missing=TRUE, warn_duplicated=TRUE) {
  plyr::rename(x, list(...), warn_missing = warn_missing, warn_duplicated = warn_duplicated)
}

#' @export
#' @rdname rename.cols
rename.cols.qtag <- function(x, ..., warn_missing=TRUE, warn_duplicated=TRUE) {
  replace <- list(...)
  out <- plyr::rename(x, replace)
  attr(out, "qualifiers") <- rename.values(qualifiers(x), replace)
  attr(out, "values") <- rename.values(values(x), replace)
  attr(out, "tags") <- rename.values(tags(x), replace)
  attr(out, "summarised") <- is.summarised(x)
  class(out) <- class(x)
  return(out)
}


#' Replace/rename values in a vector
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
#' rename.values(x, fish="newfish")
#' rename.values(x, whistle="newwhistle")
#' rename.values(x, fish="newfish", defaultValue="not a fish")
#'
rename.values <- function(x, ..., defaultValue=x, warn_missing=TRUE) {
  replacer <- list(...)
  replacernames <- names(replacer)
  notinvector <- replacernames[!(replacernames %in% x)]
  if(length(notinvector) > 0) message("Not all values were found: ", paste(notinvector, collapse=", "))
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
#' such that their usage remains consistent.
#'
#' @param md A \link{mudata} object
#' @param x A \link{mudata} object
#' @param ... Key/value pairs in the form \code{"oldvalue"="newvalue"}
#' @param apply_to The sub-elements which the rename operation should consider
#' @param warn_missing Print a message if any old names are not actually present
#' @param warn_duplicated Print a message if any name appears more than once in x 
#'   after the operation.
#'
#' @return A modified \link{mudata} object.
#' @export
#'
#' @examples
#' data(longlake2016)
#' md2 <- rename.datasets(longlake2016, Dunnington_longlake="dll")
#' validate.mudata(md2)
#' md2 <- rename.locations(longlake2016, "LL GC10"="LLGC10")
#' validate.mudata(md2)
#' md2 <- rename.params(longlake2016, LOI="Loss On Ignition")
#' validate.mudata(md2)
#' md2 <- rename.cols(longlake2016, err="stderr")
#' validate.mudata(md2)
#' 
rename.datasets <- function(md, ..., apply_to=c("data", "locations", "params", "datasets", "columns"),
                            warn_missing=TRUE) {
  for(dfname in apply_to) {
    md[[dfname]]$dataset <- rename.values(md[[dfname]]$dataset, ..., warn_missing=warn_missing)
  }
  return(md)
}

#' @rdname rename.datasets
#' @export
rename.params <- function(md, ..., apply_to=c("data", "params"), warn_missing=TRUE) {
  for(dfname in apply_to) {
    md[[dfname]]$param <- rename.values(md[[dfname]]$param, ..., warn_missing=warn_missing)
  }
  return(md)
}

#' @rdname rename.datasets
#' @export
rename.locations <- function(md, ..., apply_to=c("data", "locations"), warn_missing=TRUE) {
  for(dfname in apply_to) {
    md[[dfname]]$location <- rename.values(md[[dfname]]$location, ..., warn_missing=warn_missing)
  }
  return(md)
}

#' @rdname rename.datasets
#' @export
rename.cols.mudata <- function(x, ..., apply_to=c("datasets", "locations", "params", "data", "columns"),
                               warn_missing=FALSE, warn_duplicated=TRUE) {
  for(dfname in apply_to) {
    x[[dfname]] <- rename.cols(x[[dfname]], ..., warn_missing=warn_missing, 
                               warn_duplicated=TRUE)
  }
  x$columns$column <- rename.values(x$columns$column, ...)
  return(x)
}
