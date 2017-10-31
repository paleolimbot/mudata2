
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
#' print(kentvillegreenwood)
#' plot(kentvillegreenwood)
#' 
#' # create a mudata object from a wide data.frame
#' library(tidyr)
#' library(dplyr)
#' # gather columns and summarise replicates
#' datatable <- pocmaj %>%
#'   gather(Ca, Ti, V, key = "param", value = "param_value") %>%
#'   group_by(core, param, depth) %>%
#'   summarise(value=mean(param_value), sd=mean(param_value)) %>%
#'   rename(location = core)
#'
#' # create mudata object
#' md <- mudata(datatable)
#' 
#' # plot mudata contents
#' plot(md, y="depth")
#' library(ggplot2)
#' autoplot(md, y = "depth")
#' 
#' @seealso \link{mudata}
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @docType package
#' @aliases NULL
"_PACKAGE"

# ---- functions from other packages that are exported ----

#' @export
#' @importFrom tidyselect everything matches starts_with ends_with num_range
#' @importFrom tidyselect contains one_of last_col
tidyselect::everything

#' @export
tidyselect::matches

#' @export
tidyselect::starts_with

#' @export
tidyselect::ends_with

#' @export
tidyselect::ends_with

#' @export
tidyselect::contains

#' @export
tidyselect::num_range

#' @export
tidyselect::one_of

#' @export
tidyselect::last_col

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
