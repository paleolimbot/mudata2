#' Smart plotting of tidy data frames
#' 
#' This function uses ggplot to plot a 'long' data frame with a few \code{id.vars}, 
#' or variables that identify the values in the \code{value} column. The function
#' is optimised to plot multi-parameter spatiotemporal data either horizontally
#' (time on the x axis) or vertically (time on the y axis). Facets are intended
#' to be by parameter, which is guessed based on the right-most variable named
#' in \code{id.vars}. In the case of a \code{qtag} object, many of these values
#' are guessed. This is intended to produce a quick visual of an object to
#' examine its contents.
#' 
#'
#' @param x A \code{data.frame}
#' @param id.vars Columns that identify unique values
#' @param measure.var Column that contains values to be plotted
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
#' qualifierplot(pocmaj, c("core", "depth"), "Ca")
#'
#' pocmajqt <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' plot(pocmajqt, geom=c("path", "point"))
#' plot(pocmajqt, subset=core=="MAJ-1" & param %in% c("Ca", "Ti"))
#' plot(pocmajqt, shape="core", geom=c("path", "point"))
#' plot(long(pocmajqt))
#'
#' library(ggplot2)
#' autoplot(pocmajqt, col="core")
#'
qualifierplot <- function(x, id.vars, measure.var, subset, xvar, yvar, facets, geom="path",
                               errors="err", ...) {
  if(!inherits(x, "data.frame")) stop("x must be a data.frame")
  if(!missing(subset)) {
    x <- x[eval(substitute(subset), envir=x), ]
  }
  # ensure id.vars and measure.var are in names(x)
  if(!all(c(id.vars, measure.var) %in% names(x))) stop("Some of id.vars/measure.var are not in names(x)")
  if(length(measure.var) != 1) stop("Only one column can be used for measure.var in qualfierplot")
  
  # deal with geometries
  geoms <- list(path=ggplot2::geom_path(), line=ggplot2::geom_line(), 
                point=ggplot2::geom_point())
  unrecognizedgeoms <- geom[!(geom %in% names(geoms))]
  if(length(unrecognizedgeoms) > 0) stop("Unrecognized geometries: ", 
                                         paste(unrecognizedgeoms, collapse=" ,"))
  plotgeoms <- geoms[geom]
  
  # do mucho guessing of things
  id.vars <- id.vars
  mapping <- ggplot2::aes_string(...)
  numid.vars <- intersect(id.vars, names(x)[sapply(x, is.numericish)])
  nonnumid.vars <- id.vars[!(id.vars %in% numid.vars) & !(id.vars %in% mapping)]
  guessed <- guess.xy(x, xvar, yvar, id.vars, measure.var)
  xvar <- guessed$xvar
  yvar <- guessed$yvar

  facet_scales <- "fixed"
  if(yvar == measure.var) {
    facet_scales <- "free_y"
  } else if(xvar == measure.var) {
    facet_scales <- "free_x"
  }

  ggfacet <- ggplot2::facet_null()
  nonnumindex <- length(nonnumid.vars)
  if(missing(facets)) {
    if(length(nonnumid.vars) > 0) {
      # use last non-numeric qualifier
      ggfacet <- ggplot2::facet_wrap(stats::as.formula(paste0("~", nonnumid.vars[nonnumindex])), scales = facet_scales)
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
    nonnumid.vars <- nonnumid.vars[!(nonnumid.vars %in% chrfacets)]
    nonnumindex <- length(nonnumid.vars)
  } else {
    # 1-sided formula
    chrfacets <- unlist(lapply(attr(stats::terms.formula(facets), "variables")[-1], deparse))
    ggfacet <- ggplot2::facet_wrap(facets, scales = facet_scales)
    nonnumid.vars <- nonnumid.vars[!(nonnumid.vars %in% chrfacets)]
    nonnumindex <- length(nonnumid.vars)
  }

  if((nonnumindex > 0) && !("colour" %in% names(mapping))) {
    mapping <- c(mapping, ggplot2::aes_(colour=as.name(nonnumid.vars[nonnumindex])))
    nonnumindex <- nonnumindex - 1
  }
  if((nonnumindex > 0) && !("linetype" %in% names(mapping))) {
    mapping <- c(mapping, ggplot2::aes_(shape=as.name(nonnumid.vars[nonnumindex])))
    nonnumindex <- nonnumindex - 1
  }

  errorbars <- NULL
  if(errors %in% names(x)) {
    if(measure.var == xvar) {
      nonvalrange <- range(x[yvar])
      errbarheight <- (nonvalrange[2]-nonvalrange[1]) / 50.0
      errorbars <- ggplot2::geom_errorbarh(ggplot2::aes_string(xmin=sprintf("%s-%s", xvar, errors),
                                             xmax=sprintf("%s+%s", xvar, errors)),
                                  height=errbarheight,
                                  linetype="solid")
    } else if(measure.var == yvar) {
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
#' @rdname qualifierplot
#' @importFrom ggplot2 autoplot
autoplot.qtag.long <- function(x, ...) {
  . <- NULL; rm(.) # CMD hack
  x <- aggregate(x, mean, err=stats::sd(., na.rm = TRUE)/sum(!is.na(.)), force=FALSE)
  qualifierplot(long(x), id.vars=id.vars(x), measure.var=measure.vars(x), ...)
}

#' @export
#' @rdname qualifierplot
#' @importFrom ggplot2 autoplot
autoplot.qtag.wide <- function(x, ...) {
  autoplot.qtag.long(long(x), ...)
}

#' @export
#' @importFrom graphics plot
#' @rdname qualifierplot
plot.qtag.long <- function(x, ...) {
  autoplot.qtag.long(x, ...)
}

#' @export
#' @importFrom graphics plot
#' @rdname qualifierplot
plot.qtag.wide <- function(x, ...) {
  autoplot.qtag.long(long(x), ...)
}

#' Autoplot a mudata object
#' 
#' Produces a quick graphical summary of a mudata object using the ggplot framework.
#' The \code{subset} argument is quite powerful for filtering, but does not affect the order
#' of appearance. For this, use \link{subset.mudata}, which by default orders the datasets,
#' locations, and parameters passed into the function.
#'
#' @param x A \link{mudata} object
#' @param ... Passed on to \link{qualifierplot}
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
#' @importFrom ggplot2 autoplot
#'
autoplot.mudata <- function(x, ...) {
  qualifierplot(x$data, id.vars=c("dataset", "location", "param", "x"), measure.var="value", ...)
}

#' @rdname autoplot.mudata
#' @importFrom graphics plot
#' @export
plot.mudata <- function(x, ...) {
  autoplot.mudata(x, ...)
}

guess.xy <- function(x, xvar, yvar, id.vars, measure.var) {
  numid.vars <- intersect(id.vars, names(x)[sapply(x, is.numericish)])
  if(length(numid.vars) > 0) {
    if(missing(xvar) && missing(yvar)) {
      xvar <- numid.vars[length(numid.vars)]
      yvar <- measure.var
    } else if(missing(xvar)) {
      if(yvar == measure.var) {
        xvar <- numid.vars[length(numid.vars)]
      } else {
        xvar <- measure.var
      }
    } else if(missing(yvar)) {
      if(xvar == measure.var) {
        yvar <- numid.vars[length(numid.vars)]
      } else {
        yvar <- measure.var
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

