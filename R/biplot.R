
#' Biplot an object using ggplot
#' 
#' @param x the object to biplot
#' @param namecolumn The column where namesx and namesy are to be found
#' @param values The column containing the values to plot
#' @param errors The column containing the errors. Use \code{NULL} for default ("err" if column
#'   exists or none otherwise), or \code{NA} to suppress.
#' @param labeller The labeller to use to label facets (may want to use \code{label_parsed}
#'   to use plotmath-style labels)
#' @param ... passed to \code{aes_string()}
#' 
#' @export
#' @examples 
#' data(pocmaj)
#' qt <- as.qtag(pocmaj)
#' biplotgg(qt, color="core")
#' 
biplotgg <- function(x, ...) UseMethod("biplotgg")

#' @rdname biplotgg
#' @export
biplotgg.qtag.wide <- function(x, ...) {
  biplotgg(long(x), ...)
}

#' @rdname biplotgg
#' @export
biplotgg.qtag.long <- function(x, namecolumn=NULL, namesx=NULL, namesy=NULL, values=NULL, 
                               errors=NULL, labeller=ggplot2::label_value, ...) {
  # essential to have things be aggregated
  x <- aggregate(x, mean, err=sd(., na.rm = TRUE)/sum(!is.na(.)))
  
  els <- NULL
  quals <- qualifiers(x)
  if(is.null(values)) {
    values <- values(x)
  } else {
    if(!(values %in% names(x))) stop("Column ", values, "was not found in x")
  }
  if(is.null(errors)) {
    if("err" %in% names(x)) {
      errors <- "err"
    }
  } else if(is.na(errors)) {
    errors <- NULL
  } else {
    if(!(errors %in% names(x))) stop("Column ", errors, "was not found in x")
  }
  if(is.null(namecolumn)) {
    namecolumn <- quals[length(quals)]
  } else {
    if(!(namecolumn %in% names(x))) stop("Column ", namecolumn, "was not found in x")
  }
  if(is.null(namesx)) {
    namesx <- unique(as.character(x[[namecolumn]]))
  }
  if(any(!((c(namesx, namesy) %in% x[[namecolumn]])))) stop("Not all names were found in ", namecolumn)
  
  if(is.null(namesy)) {
    els <- dplyr::do(
          dplyr::group_by(data.frame(i=1:(length(namesx)-1), stringsAsFactors = FALSE), i), {
        data.frame(vary=namesx[.$i], varx=namesx[(.$i+1):length(namesx)], stringsAsFactors=F)
      })
    els$i <- NULL
    namesy <- namesx[1:(length(namesx)-1)]
    namesx <- namesx[2:length(namesx)]
  } else {
    els <- expand.grid(varx=namesx, vary=namesy, stringsAsFactors = FALSE)
  }
  joincols <- quals[quals != namecolumn]
  tmp <- dplyr::do(dplyr::group_by(els, varx, vary), {
    if(.$varx == .$vary) {
      data.frame()
    } else {
      xs <- x[x[[namecolumn]]==.$varx, c(quals, values, errors)]
      ys <- x[x[[namecolumn]]==.$vary, c(quals, values, errors)]
      dplyr::inner_join(xs, ys, by=joincols, suffix=c('.x', '.y'))
    }
  })
  
  tmp$varx <- factor(tmp$varx, levels=namesx)
  tmp$vary <- factor(tmp$vary, levels=namesy)
  ggerror <- NULL
  valx <- paste0(values, '.x')
  valy <- paste0(values, '.y')
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
