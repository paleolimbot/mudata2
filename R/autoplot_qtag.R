#' Autoplot a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#' @param xvar Column to be used on the x-axis
#' @param yvar Column to be used on the y-axis
#' @param facets Column to be used as facetting variable
#' @param subset Subset to plot
#' @param ... Passed on to \code{aes_string()}
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(pocmaj)
#' plotgg(pocmaj)
#'
#' pocmajqt <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' plotgg(pocmajqt)
#' plotgg(pocmajqt, subset=core=="MAJ-1" & column %in% c("Ca", "Ti"))
#' plotgg(pocmajqt, shape="core")
#'
#'
plotgg.qtag.long <- function(x, subset, xvar, yvar, facets, ...) {
  x <- aggregate(x, mean, err=sd(., na.rm = TRUE)/sum(!is.na(.)))
  if(!missing(subset)) {
    x <- x[eval(substitute(subset), envir=x), ]
  }
  qualifiers <- qualifiers(x)
  values <- values(x)
  mapping <- ggplot2::aes_string(...)
  types <- sapply(qualifiers, function(qual) class(x[[qual]])[1])
  numqualifiers <- qualifiers[types %in% c("numeric", "integer")]
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
      ggfacet <- ggplot2::facet_wrap(as.formula(paste0("~", nonnumqualifiers[nonnumindex])), scales = facet_scales)
      nonnumindex <- nonnumindex - 1
    } else {
      ggfacet <- facet_null()
    }
  } else if(is.null(facets)) {
    ggfacet <- facet_null()
  } else if(attr(terms.formula(facets), "response") == 1) {
    # 2- sided formula
    ggfacet <- ggplot2::facet_grid(facets, scales = facet_scales)
    chrfacets <- unlist(lapply(attr(terms.formula(facets), "variables")[-1], deparse))
    nonnumqualifiers <- nonnumqualifiers[!(nonnumqualifiers %in% chrfacets)]
    nonnumindex <- length(nonnumqualifiers)
  } else {
    # 1-sided formula
    chrfacets <- unlist(lapply(attr(terms.formula(facets), "variables")[-1], deparse))
    ggfacet <- facet_wrap(facets, scales = facet_scales)
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
  if("err" %in% names(x)) {
    if(values == xvar) {
      nonvalrange <- range(x[yvar])
      errbarheight <- (nonvalrange[2]-nonvalrange[1]) / 50.0
      errorbars <- ggplot2::geom_errorbarh(ggplot2::aes_string(xmin=sprintf("%s-%s", xvar, "err"),
                                             xmax=sprintf("%s+%s", xvar, "err")),
                                  height=errbarheight,
                                  linetype="solid")
    } else if(values == yvar) {
      nonvalrange <- range(x[xvar])
      errbarheight <- (nonvalrange[2]-nonvalrange[1]) / 50.0
      errorbars <- ggplot2::geom_errorbar(ggplot2::aes_string(ymin=sprintf("%s-%s", yvar, "err"),
                                            ymax=sprintf("%s+%s", yvar, "err")),
                                 width=errbarheight,
                                 linetype="solid")
    }
  }

  yrev <- ggplot2::scale_y_reverse()
  if(.is_ad(x[yvar])) {
    yrev <- NULL
  }
  mapping <- c(mapping, ggplot2::aes_(x=as.name(xvar), y=as.name(yvar)))
  class(mapping) <- "uneval"
  return(ggplot2::ggplot(x, mapping) + ggplot2::geom_path() + errorbars + ggplot2::geom_point() + ggfacet + yrev)
}

#' @export
#' @rdname plotgg.qtag.long
plotgg.qtag.wide <- function(x, ...) {
  plotgg(long(x), ...)
}

#' @export
#' @rdname plotgg.qtag.long
#' @importFrom ggplot2 autoplot
plotgg <- function(x, ...) UseMethod("plotgg")

guess.xy <- function(x, xvar, yvar) {
  qualifiers <- qualifiers(x)
  values <- values(x)
  types <- sapply(qualifiers, function(qual) class(x[[qual]])[1])
  numqualifiers <- qualifiers[types %in% c("numeric", "integer")]
  if(numqualifiers > 0) {
    if(missing(xvar) && missing(yvar)) {
      yvar <- numqualifiers[length(numqualifiers)]
      xvar <- values
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
  r <- range(x)
  return((r[2] <= 2200) && (r[1] >= 1000))
}

