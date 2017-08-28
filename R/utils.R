
#' Melt multiple sets of columns in parallel
#'
#' Essentially this is a wrapper around \link[reshape2]{melt.data.frame} that
#' is able to \link{cbind} several melt operations. This is useful when a wide
#' data frame contains uncertainty or flag information in paired columns.
#'
#' @param x A data.frame
#' @param id.vars vector of ID variable names
#' @param variable.name,key Column name to use to store variables
#' @param ... Named arguments specifying the \code{measure.vars} to be stored to the
#'   column name specified.
#' @param convert Convert types (see \link[tidyr]{gather})
#' @param factorsAsStrings,factor_key Control whether factors are converted to character when melted as
#'   measure variables.
#'
#' @return A molten data.frame
#' @export
#'
#' @examples
#' data(pocmajsum)
#' parallel_melt(pocmajsum,
#'               id.vars=c("core", "depth"),
#'               values=c("Ca", "Ti", "V"),
#'               err=c("Ca_sd", "Ti_sd", "V_sd"))
#'
parallel_melt <- function(x, id.vars, ..., variable.name="param", factorsAsStrings=TRUE) {
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
  
  tibble::as_tibble(do.call(cbind, c(list(iddata), melted)))
}

#' @export
#' @rdname parallel_melt
parallel_gather <- function(x, key, ..., convert = FALSE, factor_key = FALSE) {
  # enquos arguments
  lst <- rlang::quos(...)
  
  # check arguments
  if(length(lst) == 0) stop("Must pass at least one value = columns in parallel_gather()")
  if(is.null(names(lst)) || any(names(lst) == "")) {
    stop("All arguments to parallel_gather() must be named")
  }
  
  # use a hack to get column names as character using tidyeval and dplyr
  col_names <- tibble::as_tibble(stats::setNames(as.list(colnames(x)), colnames(x)))
  lst_as_colnames <- lapply(lst, function(name_quo) {
    dplyr::select(col_names, !!name_quo) %>% colnames()
  })
  
  # pass to parallel gather base
  parallel_gather_base(x, key, lst_as_colnames, convert = convert, factor_key = factor_key)
}

#' @export
#' @rdname parallel_melt
parallel_gather_ <- function(x, key, ..., convert = FALSE, factor_key = FALSE) {
  parallel_gather_base(x, key, list(...), convert = convert, factor_key = factor_key)
}

parallel_gather_base <- function(x, key, lst_as_colnames, convert = FALSE, factor_key = FALSE) {
  # check arguments
  if(length(lst_as_colnames) == 0) stop("Must pass at least one value = columns in parallel_gather()")
  if(is.null(names(lst_as_colnames)) || any(names(lst_as_colnames) == "")) {
    stop("All arguments to parallel_gather() must be named")
  }
  
  # check length (each argument should refer to the same number of columns)
  arg_col_count <- vapply(lst_as_colnames, length, integer(1))
  if(!length(unique(arg_col_count)) == 1) {
    stop("All named arguments must refer to the same number of columns")
  }
  
  # id variables are those not mentioned in ...
  id_vars <- setdiff(colnames(x), unlist(lst_as_colnames))
  
  # do gather for each item in ..., using id_vars and cols mentioned in 
  # each argument
  gathered <- lapply(seq_along(lst_as_colnames), function(i) {
    tidyr::gather_(x[c(id_vars, lst_as_colnames[[i]])], 
                   key = key, value = names(lst_as_colnames)[i],
                   gather_cols = lst_as_colnames[[i]],
                   na.rm = FALSE, convert = convert, factor_key = factor_key)
  })
  
  # get id data
  id_data <- gathered[[1]][c(id_vars, key)]
  
  # select non-id vars for each melt operation
  gathered <- lapply(gathered, function(df) df[setdiff(colnames(df), c(id_vars, key))])
  
  # return cbind operation
  dplyr::bind_cols(id_data, gathered)
}
