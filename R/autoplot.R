#' Autoplot a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#' @param xvar Column to be used on the x-axis
#' @param yvar Column to be used on the y-axis
#' @param facets Column to be used as facetting variable
#' @param geom GGPlot geometries to be used. Can be any combination of point, path, or line.
#' @param subset Subset to plot
#' @param errors The colum that contains uncertainty information
#' @param ... Passed on to \code{aes_string()}
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#' data(pocmaj)
#'
#' pocmajqt <- as.qtag(pocmaj, .qualifiers=c("core", "depth"))
#' plot(pocmajqt, geom=c("path", "point"))
#' plot(pocmajqt, subset=core=="MAJ-1" & param %in% c("Ca", "Ti"))
#' plot(pocmajqt, shape="core")
#' plot(long(pocmajqt))
#'
#' library(ggplot2)
#' autoplot(pocmajqt, shape="core")
#'
#' @importFrom ggplot2 autoplot
#'
autoplot.qtag.long <- function(x, subset, xvar, yvar, facets, geom="path",
                               errors="err", ...) {
  . <- NULL; rm(.) # CMD hack
  x <- aggregate(x, mean, err=stats::sd(., na.rm = TRUE)/sum(!is.na(.)), force=FALSE)
  if(!missing(subset)) {
    x <- x[eval(substitute(subset), envir=x), ]
  }
  
  # deal with geometries
  geoms <- list(path=ggplot2::geom_path(), line=ggplot2::geom_line(), 
                point=ggplot2::geom_point())
  unrecognizedgeoms <- geom[!(geom %in% names(geoms))]
  if(length(unrecognizedgeoms) > 0) stop("Unrecognized geometries: ", 
                                         paste(unrecognizedgeoms, collapse=" ,"))
  plotgeoms <- geoms[geom]
  
  # do mucho guessing of things
  qualifiers <- qualifiers(x)
  values <- values(x)
  mapping <- ggplot2::aes_string(...)
  types <- sapply(qualifiers, function(qual) class(x[[qual]])[1])
  numqualifiers <- qualifiers[types %in% c("numeric", "integer", "Date", "POSIXct", "POSIXt")]
  nonnumqualifiers <- qualifiers[!(qualifiers %in% numqualifiers) & !(qualifiers %in% mapping)]
  guessed <- guess.xy(x, xvar, yvar)
  xvar <- guessed$xvar
  yvar <- guessed$yvar

  facet_scales <- "fixed"
  if(yvar == values) {
    facet_scales <- "free_y"
  } else if(xvar == values) {
    facet_scales <- "free_x"
  }

  ggfacet <- ggplot2::facet_null()
  nonnumindex <- length(nonnumqualifiers)
  if(missing(facets)) {
    if(length(nonnumqualifiers) > 0) {
      # use last non-numeric qualifier
      ggfacet <- ggplot2::facet_wrap(stats::as.formula(paste0("~", nonnumqualifiers[nonnumindex])), scales = facet_scales)
      nonnumindex <- nonnumindex - 1
    } else {
      ggfacet <- ggplot2::facet_null()
    }
  } else if(is.null(facets)) {
    ggfacet <- ggplot2::facet_null()
  } else if(attr(stats::terms.formula(facets), "response") == 1) {
    # 2- sided formula
    ggfacet <- ggplot2::facet_grid(facets, scales = facet_scales)
    chrfacets <- unlist(lapply(attr(stats::terms.formula(facets), "variables")[-1], deparse))
    nonnumqualifiers <- nonnumqualifiers[!(nonnumqualifiers %in% chrfacets)]
    nonnumindex <- length(nonnumqualifiers)
  } else {
    # 1-sided formula
    chrfacets <- unlist(lapply(attr(stats::terms.formula(facets), "variables")[-1], deparse))
    ggfacet <- ggplot2::facet_wrap(facets, scales = facet_scales)
    nonnumqualifiers <- nonnumqualifiers[!(nonnumqualifiers %in% chrfacets)]
    nonnumindex <- length(nonnumqualifiers)
  }

  if((nonnumindex > 0) && !("colour" %in% names(mapping))) {
    mapping <- c(mapping, ggplot2::aes_(colour=as.name(nonnumqualifiers[nonnumindex])))
    nonnumindex <- nonnumindex - 1
  }
  if((nonnumindex > 0) && !("linetype" %in% names(mapping))) {
    mapping <- c(mapping, ggplot2::aes_(shape=as.name(nonnumqualifiers[nonnumindex])))
    nonnumindex <- nonnumindex - 1
  }

  errorbars <- NULL
  if(errors %in% names(x)) {
    if(values == xvar) {
      nonvalrange <- range(x[yvar])
      errbarheight <- (nonvalrange[2]-nonvalrange[1]) / 50.0
      errorbars <- ggplot2::geom_errorbarh(ggplot2::aes_string(xmin=sprintf("%s-%s", xvar, errors),
                                             xmax=sprintf("%s+%s", xvar, errors)),
                                  height=errbarheight,
                                  linetype="solid")
    } else if(values == yvar) {
      nonvalrange <- range(x[xvar])
      errbarheight <- (nonvalrange[2]-nonvalrange[1]) / 50.0
      errorbars <- ggplot2::geom_errorbar(ggplot2::aes_string(ymin=sprintf("%s-%s", yvar, errors),
                                            ymax=sprintf("%s+%s", yvar, errors)),
                                 width=errbarheight,
                                 linetype="solid")
    }
  }

  yrev <- NULL
  if(yvar == "x") {
    if(!.is_ad(x[[yvar]])) {
      yrev <- ggplot2::scale_y_reverse()
    }
  } else if(xvar == "x") {
    if(!.is_ad(x[[xvar]])) {
      yrev <- ggplot2::scale_x_reverse()
    }
  }

  mapping <- c(mapping, ggplot2::aes_(x=as.name(xvar), y=as.name(yvar)))
  class(mapping) <- "uneval"
  
  
  return(ggplot2::ggplot(x, mapping) + errorbars + plotgeoms + ggfacet + yrev)
}

#' @export
#' @rdname autoplot.qtag.long
autoplot.qtag.wide <- function(x, ...) {
  autoplot(long(x), ...)
}

#' @export
#' @rdname autoplot.qtag.long
plot.qtag.long <- function(x, ...) {
  autoplot.qtag.long(x, ...)
}

#' @export
#' @rdname autoplot.qtag.long
plot.qtag.wide <- function(x, ...) {
  plot(long(x), ...)
}

#' Autoplot a mudata object
#'
#' If you get a \code{seq...finite values} error, you may have to check for params
#' that have all non-detect values. This can be done with the dplyr summarise function
#' (\code{group_by(dataset, param) / summarise(allnd=all(is.na(value))) / data.frame()}).
#' The \code{subset} argument is quite powerful for filtering, but does not affect the order
#' of appearance. For this, use \link{subset.mudata}.
#'
#' @param x A \link{mudata} object
#' @param ... Passed on to \link{autoplot.qtag.long}
#'
#' @return A ggplot object
#'
#' @export
#' 
#' @examples 
#' data(kentvillegreenwood)
#' plot(kentvillegreenwood)
#' library(ggplot2)
#' autoplot(kentvillegreenwood)
#'
autoplot.mudata <- function(x, ...) {
  autoplot.qtag.long(x$data, ...)
}

#' @rdname autoplot.mudata
#' @export
plot.mudata <- function(x, ...) {
  autoplot.qtag.long(x$data, ...)
}

guess.xy <- function(x, xvar, yvar) {
  qualifiers <- qualifiers(x)
  values <- values(x)
  types <- sapply(qualifiers, function(qual) class(x[[qual]])[1])
  numqualifiers <- qualifiers[types %in% c("numeric", "integer", "Date", "POSIXct", "POSIXt")]
  if(length(numqualifiers) > 0) {
    if(missing(xvar) && missing(yvar)) {
      xvar <- numqualifiers[length(numqualifiers)]
      yvar <- values
    } else if(missing(xvar)) {
      if(yvar == values) {
        xvar <- numqualifiers[length(numqualifiers)]
      } else {
        xvar <- values
      }
    } else if(missing(yvar)) {
      if(xvar == values) {
        yvar <- numqualifiers[length(numqualifiers)]
      } else {
        yvar <- values
      }
    }
    return(list(xvar=xvar, yvar=yvar))
  } else {
    stop("Could not guess xvar and yvar")
  }
}

.is_ad <- function(x) {
  if(any(class(x) %in% c("numeric", "integer"))) {
    r <- range(x)
    return((r[2] <= 2200) && (r[1] >= 1000))
  } else {
    return(TRUE)
  }
}

