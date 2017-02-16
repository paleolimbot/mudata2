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
#' replacecol(pocmaj, Ca="Calcium")
#' pocmaj2 <- as.qtag(pocmaj, .qualifiers=c("core", "depth"))
#' pocmaj2 <- replacecol(pocmaj2, Ca="Calcium")
#' attr(pocmaj2, "values")
#'
replacecol <- function(x, ...) UseMethod("replacecol")

#' @export
#' @rdname replacecol
replacecol.default <- function(x, ...) plyr::rename(x, list(...))
#' @export
#' @rdname replacecol
replacecol.qtag <- function(x, ...) {
  replace <- list(...)
  out <- plyr::rename(x, replace)
  attr(out, "qualifiers") <- replaceval(qualifiers(x), replace)
  attr(out, "values") <- replaceval(values(x), replace)
  attr(out, "tags") <- replaceval(tags(x), replace)
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
#' replaceval(x, fish="newfish")
#' replaceval(x, whistle="newwhistle")
#' replaceval(x, fish="newfish", defaultValue="not a fish")
#'
replaceval <- function(x, ..., defaultValue=x) {
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