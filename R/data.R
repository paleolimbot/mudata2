
#' Alta Lake Gravity Core Data
#' 
#' Bulk geochemistry of a gravity core from Alta Lake, Whistler, British Columbia,
#' Canada.
#' 
#' @references
#' Dunnington DW, Spooner IS, White CE, et al (2016) A geochemical perspective on the impact of 
#' development at Alta Lake, British Columbia, Canada. J Paleolimnol 56:315-330. 
#' doi: 10.1007/s10933-016-9919-x
#' 
#' @examples
#' library(ggplot2)
#' autoplot(alta_lake, y = "depth") + scale_y_reverse()
#' autoplot(alta_lake, y = "age")
#' 
"alta_lake"

#' Pockwock Lake/Lake Major Elemental Sample Data
#'
#' A small example data.frame used to test structure methods.
#'
#' @format A \code{data.frame} containing multi-qualifier concentration data
"pocmaj"

#'  Pre-summarised Sample Data
#'
#' A small example data.frame of pre-summarised data; a summarised
#' version of the \link{pocmaj} dataset.
#'
#' @format A \code{data.frame} containing multi-qualifier data
"pocmajsum"

#' Kentville/Greenwood Climate Data
#'
#' Climate data for Kentville and Greenwood (Nova Scotia) for July and August of 1999.
#'
#' @format A \link{mudata} object (list with elements data, locations, params, datasets, columns)
#' 
#' @source Environment Canada via the 'rclimateca' package. \url{http://climate.weather.gc.ca/}
#' 
#' @examples
#' autoplot(kentvillegreenwood)
#' 
"kentvillegreenwood"
