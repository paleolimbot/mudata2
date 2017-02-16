#' Replace/rename a column in an object
#'
#' Essentially a thin convenience wrapper around \code{plyr::rename(x, list(...))},
#' except \link{qtag} objects have their qualifiers/tags/values attributes properly modified
#' 
#' @param x An object that has columns that can be renamed
#' @param ... Key/value pairs to replace in the form \code{oldval="newval"}
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
rename.cols <- function(x, ...) UseMethod("rename.cols")

#' @export
#' @rdname rename.cols
rename.cols.default <- function(x, ...) plyr::rename(x, list(...))
#' @export
#' @rdname rename.cols
rename.cols.qtag <- function(x, ...) {
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
rename.values <- function(x, ..., defaultValue=x) {
  replacer <- list(...)
  replacernames <- names(replacer)
  if(length(defaultValue) != length(x)) {
    defaultValue <- rep(defaultValue, length.out=length(x))
  }
  sapply(1:length(x), function(i) {
    v <- x[i]
    ifelse(v %in% replacernames, replacer[[v]], defaultValue[i])
  })
}


