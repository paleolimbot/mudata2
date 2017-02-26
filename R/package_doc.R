


#' A (Mostly) Universal Data Format for Multi-Parameter, Spatiotemporal Data
#' 
#' The 'mudata' package for R is a set of tools to create, manipulate, and 
#' visualize multi-parameter, spatiotemporal data. Data of this type includes 
#' all data where multiple parameters (e.g. wind speed, precipitation, 
#' temperature) are measured along a common axis (e.g. time, depth) at discrete 
#' locations (e.g. climate stations). These data include long-term climate data 
#' collected from climate stations, paleolimnological data, ice core data, and 
#' ocean core data among many others. Data of this type is often voluminous and 
#' difficult to organize given its multi-dimensional nature. The (mostly) 
#' universal data (mudata) format is an attempt to organize these data in a 
#' common way to facilitate their documentation and comparison.
#' 
#' The (mostly) universal data format is a collection of five (or more) tables, 
#' one of which contains the data in a molten form (see \code{reshape2::melt}).
#' The easiest way to visualize a mudata object is to inspect the
#' \code{kentvillegreenwood} dataset within the package. This object is a collection
#' of daily observations from Kentville, Nova Scotia, and Greenwood, Nova Scotia
#' from July and August, 1999.
#' 
#' @examples
#' # inspect the example dataset
#' data(kentvillegreenwood)
#' plot(kentvillegreenwood)
#' 
#' # create a mudata object from a wide data.frame
#' library(reshape2)
#' data("pocmajsum")
#' pocmajwide <- pocmajsum[c("core", "depth", "Ca", "V", "Ti")]
#' pocmajwide <- rename.cols(pocmajwide, "core"="location", "depth"="x")
#' pocmajlong <- melt(pocmajwide, id.vars=c("location", "x"), variable.name = "param",
#'                    value.name="value")
#' md <- mudata(pocmajlong)
#' plot(md, yvar="x")
#' 
#' @seealso \link{mudata}
#' 
#' @docType package
#' @aliases NULL
"_PACKAGE"