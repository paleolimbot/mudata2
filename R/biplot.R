
#' Biplot a molten data frame using facets
#' 
#' Uses the ggplot framework and \code{facet_grid} to produce biplots of a molten
#' data frame.
#' 
#' @param x the object to biplot
#' @param id.vars the columns that identify a single value
#' @param namecolumn The column where namesx and namesy are to be found
#' @param namesx The names to be included in the x axes, or all the names to be included
#' @param namesy The names to be included on the y axes, or NULL for all possible combinations
#'   of \code{namesx}.
#' @param measure.var The column containing the values to plot
#' @param errors The column containing the errors. Use \code{NULL} for default ("err" if column
#'   exists or none otherwise), or \code{NA} to suppress.
#' @param labeller The labeller to use to label facets (may want to use \code{label_parsed}
#'   to use plotmath-style labels)
#' @param validate Ensure id.vars identify unique values
#' @param ... passed to \code{aes_string()}
#' 
#' @importFrom stats biplot
#' @export
#' @examples 
#' data(pocmaj)
#' qt <- as.qtag(pocmaj)
#' biplot(qt, color="core")
#' 
longbiplot <- function(x, id.vars, measure.var, namesx=NULL, namesy=NULL, namecolumn=NULL,
                             errors=NULL, labeller=ggplot2::label_value, validate=TRUE, ...) {
  if(!inherits(x, "data.frame")) stop("x must be a data.frame")
  if(!all(c(id.vars, measure.var) %in% names(x))) stop("Some of id.vars/measure.var are not in names(x)")
  if(length(measure.var) != 1) stop("Only one column can be used for measure.var in longbiplot")
  
  # CMD hack
  . <- NULL; rm(.)
  # check that data are summarised
  if(validate) {
    dplyr::do(do.call(dplyr::group_by_, c(list(x), id.vars)), {
      if(nrow(.) > 1) {
        pasteargs <- c(as.list(.[1, id.vars]), list(sep="->"))
        stop("Data are not summarised for id.vars: ", do.call(paste, pasteargs))
      }
      data.frame()
    })
  }
  
  els <- NULL
  quals <- id.vars
  if(!(measure.var %in% names(x))) stop("Column ", measure.var, " was not found in x")
  
  if(is.null(errors)) {
    if("err" %in% names(x)) {
      errors <- "err"
    }
  } else if(is.na(errors)) {
    errors <- NULL
  } else {
    if(!(errors %in% names(x))) stop("Column ", errors, " was not found in x")
  }
  if(is.null(namecolumn)) {
    namecolumn <- quals[length(quals)]
  } else {
    if(!(namecolumn %in% names(x))) stop("Column ", namecolumn, " was not found in x")
  }
  x[[namecolumn]] <- as.character(x[[namecolumn]])
  
  if(is.null(namesx)) {
    namesx <- unique(x[[namecolumn]])
  }
  if(any(!((c(namesx, namesy) %in% x[[namecolumn]])))) stop("Not all names were found in ", namecolumn)
  
  if(is.null(namesy)) {
    els <- dplyr::do(
      dplyr::group_by_(data.frame(i=1:(length(namesx)-1), stringsAsFactors = FALSE), "i"), {
        data.frame(vary=namesx[.$i], varx=namesx[(.$i+1):length(namesx)], stringsAsFactors=F)
      })
    els$i <- NULL
    namesy <- namesx[1:(length(namesx)-1)]
    namesx <- namesx[2:length(namesx)]
  } else {
    els <- expand.grid(varx=namesx, vary=namesy, stringsAsFactors = FALSE)
  }
  joincols <- quals[quals != namecolumn]
  tmp <- dplyr::do(dplyr::group_by_(els, "varx", "vary"), {
    if(.$varx == .$vary) {
      data.frame()
    } else {
      xs <- x[x[[namecolumn]]==.$varx, c(quals, measure.var, errors)]
      ys <- x[x[[namecolumn]]==.$vary, c(quals, measure.var, errors)]
      both <- dplyr::inner_join(xs, ys, by=joincols, suffix=c('.x', '.y'))
      data.frame(both, stringsAsFactors = FALSE)
    }
  })
  
  tmp$varx <- factor(tmp$varx, levels=namesx)
  tmp$vary <- factor(tmp$vary, levels=namesy)
  ggerror <- NULL
  valx <- paste0(measure.var, '.x')
  valy <- paste0(measure.var, '.y')
  if(!is.null(errors)) {
    errx <- paste0(errors, '.x')
    erry <- paste0(errors, '.y')
    ggerror <- list(
      ggplot2::geom_errorbar(
        ggplot2::aes_string(ymax=paste(valy, erry, sep='+'), ymin=paste(valy, erry, sep='-'))),
      ggplot2::geom_errorbarh(
        ggplot2::aes_string(xmax=paste(valx, errx, sep='+'), xmin=paste(valx, errx, sep='-')))
    )
  }
  ggplot2::ggplot(tmp, ggplot2::aes_string(x=valx, y=valy, ...)) + 
    ggplot2::geom_point() + 
    ggerror +
    ggplot2::facet_grid(vary~varx, scales="free", labeller = labeller) +
    ggplot2::labs(x=NULL, y=NULL)
}

#' @rdname longbiplot
#' @export
biplot.qtag.long <- function(x, ...) {
  # CMD hack
  . <- NULL; rm(.)
  # essential to have things be aggregated
  x <- aggregate(x, mean, err=stats::sd(., na.rm = TRUE)/sum(!is.na(.)), force=FALSE)
  longbiplot(x, id.vars=id.vars(x), measure.var=measure.vars(x), ..., validate=FALSE)
}

#' @rdname longbiplot
#' @export
biplot.qtag.wide <- function(x, ...) {
  biplot(long(x), ...)
}

#' Biplot a mudata object
#' 
#' Uses the ggplot framework and \code{facet_grid} to produce a biplot of a mudata object.
#'
#' @param x A mudata object
#' @param namecolumn The column that contains the names for biplotting
#' @param ... passed to \link{longbiplot}
#'
#' @return a ggplot object
#' @export
#' 
#' @examples 
#' data(kentvillegreenwood)
#' biplot(kentvillegreenwood, c("meantemp", "maxtemp", "mintemp"), col="location")
#'
biplot.mudata <- function(x, ..., namecolumn = "param") {
  longbiplot(x$data, id.vars=c("dataset", "location", "param", "x"),
             measure.var="value", ..., namecolumn = namecolumn, validate=FALSE)
}
