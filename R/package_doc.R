
#' A (Mostly) Universal Data Format for Multi-Parameter, Spatiotemporal Data
#' 
#' The 'mudata' package for R is a set of tools to create, manipulate, and 
#' visualize multi-parameter, spatiotemporal data. Data of this type includes 
#' all data where multiple parameters (e.g. wind speed, precipitation, 
#' temperature) are measured along common axes (e.g. time, depth) at discrete 
#' locations (e.g. climate stations). These data include long-term climate data 
#' collected from climate stations, paleolimnological data, ice core data, 
#' long-term water quality monitoring data, and ocean core data among many 
#' others. Data of this type is often voluminous and difficult to
#' organize/document given its multidimensional nature. The (mostly) universal
#' data (mudata) format is an attempt to organize these data in a common way to
#' facilitate their documentation and comparison.
#' 
#' The (mostly) universal data format is a collection of five (or more) tables, 
#' one of which contains the data in a parameter-long form (see 
#' \link[tidyr]{gather}). The easiest way to visualize a mudata object is to 
#' inspect the sample datasets within the package (\link{ns_climate}, 
#' \link{kentvillegreenwood}, \link{alta_lake}, \link{long_lake}, and 
#' \link{second_lake_temp}).
#' 
#' @seealso \link{mudata}, \link{read_mudata}
#'   
#' @examples 
#' print(kentvillegreenwood)
#' autoplot(kentvillegreenwood)
#' 
#' @importFrom rlang .data
#'   
#' @docType package
"_PACKAGE"

# ---- functions from other packages that are exported ----

#' @export
#' @importFrom dplyr everything matches starts_with ends_with num_range
#' @importFrom dplyr contains one_of
dplyr::everything

#' @export
dplyr::matches

#' @export
dplyr::starts_with

#' @export
dplyr::ends_with

#' @export
dplyr::ends_with

#' @export
dplyr::contains

#' @export
dplyr::num_range

#' @export
dplyr::one_of

#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @importFrom ggplot2 autoplot
#' @export
dplyr::src_tbls

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
