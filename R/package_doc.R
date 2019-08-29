
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
#' [gather][tidyr::gather]). The easiest way to visualize a mudata object is to
#' inspect the sample datasets within the package ([ns_climate],
#' [kentvillegreenwood], [alta_lake], [long_lake], and
#' [second_lake_temp]).
#'
#' @seealso [mudata], [read_mudata]
#'
#' @references Dunnington DW and Spooner IS (2018). "Using a linked table-based
#' structure to encode self-describing multiparameter spatiotemporal data".
#' FACETS. doi:10.1139/facets-2017-0026 
#' <http://www.facetsjournal.com/doi/10.1139/facets-2017-0026>
#'
#' @examples
#' print(kentvillegreenwood)
#' autoplot(kentvillegreenwood)
#'
#' @importFrom rlang .data
#' @importFrom rlang !!!
#' @importFrom rlang !!
#' @importFrom rlang enquo
#' @importFrom rlang quos
#' @importFrom dplyr n
#'
#' @docType package
"_PACKAGE"

# ---- functions from other packages that are exported ----

#' @export
#' @importFrom dplyr everything
dplyr::everything

#' @export
#' @importFrom dplyr matches
dplyr::matches

#' @export
#' @importFrom dplyr starts_with
dplyr::starts_with

#' @export
#' @importFrom dplyr ends_with
dplyr::ends_with

#' @export
#' @importFrom dplyr contains
dplyr::contains

#' @export
#' @importFrom dplyr num_range
dplyr::num_range

#' @export
#' @importFrom dplyr one_of
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
