

#' Melt multiple sets of columns in parallel
#'
#' Essentially this is a wrapper around \code{reshape2::melt.data.frame} that
#' is able to \code{cbind} several melt operations. This is useful when a wide
#' data frame contains uncertainty or flag information in paired columns.
#'
#' @param x A data.frame
#' @param id.vars vector of ID variable names
#' @param variable.name Column name to use to store variables
#' @param ... Named arguments specifying the \code{measure.vars} to be stored to the
#'   column name specified.
#' @param factorsAsStrings Control whether factors are converted to character when melted as
#'   measure variables.
#'
#' @return A \code{qtag.long} object
#' @export
#'
#' @examples
#' data(pocmajsum)
#' parallel.melt(pocmajsum,
#'               id.vars=c("core", "depth"),
#'               values=c("Ca", "Ti", "V"),
#'               err=c("Ca_sd", "Ti_sd", "V_sd"))
#'
parallel.melt <- function(x, id.vars, ..., variable.name="param", factorsAsStrings=TRUE) {
  combos <- list(...)
  combonames <- names(combos)
  if(length(combonames) != length(combos)) stop("All arguments must be named")
  lengths <- unique(sapply(combos, length))
  if(length(lengths) > 1) stop("All melted columns must have the same number of source columns")
  melted <- lapply(combonames, function(varname) {
    reshape2::melt(x, id.vars=id.vars, measure.vars=combos[[varname]], value.name=varname,
                   variable.name=variable.name, factorsAsStrings=factorsAsStrings)
  })
  iddata <- melted[[1]][c(id.vars, variable.name)]
  melted <- lapply(melted, function(df) df[names(df) %in% names(combos)])
  df <- do.call(cbind, c(list(iddata), melted))
  .reclass(df, id.vars=c(id.vars, variable.name), measure.vars=combonames[1],
           tag.vars=combonames[combonames != combonames[1]], summarised=FALSE)
}

# internal function to define 'numeric' (ish) classes
# these functions can have numeric functions applied to them, specifically,
# such as min(), max(), and mean(). Is essentially the opposite of ggplot2's
# is.discrete(), which may be an alternative.
is.numericish <- function(x) {
  return(any(class(x) %in% c("numeric", "integer", "Date", "POSIXct", "POSIXt")))
}
