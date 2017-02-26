

#' Convert an object to a qualifier/tag structure
#' 
#' A qualifer/tag structure is an experimental version of a data frame
#' that stores id.vars and measure.vars with the data structure, so it
#' can be handled in a more automatic way. This functionality is experimental.
#'
#' @param df A \code{data.frame} or similar object
#' @param measure.vars Column names containing the values of interest (NA to guess)
#' @param id.vars Column names of qualifying measure.vars (NA to guess)
#' @param tag.vars Column names of tag values
#' @param quiet Use \code{quiet=TRUE} to suppress error messages
#' @param ... Passed to/from methods
#'
#' @return An object of type \code{qtag}, which is essentially the unchanged
#'   input with id.vars, measure.vars, and tag.vars information attached.
#'
#' @export
#'
#' @examples
#' data("pocmaj")
#' pocmaj <- as.qtag(pocmaj, id.vars = c("core", "depth"))
#' long(pocmaj)
#' aggregate(pocmaj)
#' aggregate(long(pocmaj))
#'
as.qtag <- function(df, id.vars=NA, measure.vars=NA, tag.vars=NA, quiet=FALSE) {
  dfnames <- names(df)
  if(identical(id.vars, NA)) {
    id.vars <- id.vars(df)
    if(length(id.vars) == 0) {
      df$.row_id <- 1:nrow(df)
      id.vars <- ".row_id"
    }
  } else {
    id.vars <- as.character(id.vars)
  }
  if(identical(tag.vars, NA)) {
    tag.vars <- NULL
  } else {
    tag.vars <- as.character(tag.vars)
  }
  if(identical(measure.vars, NA)) {
    valuecol <- dfnames[!(dfnames %in% id.vars) & !(dfnames %in% tag.vars)]
    if(!quiet) message("Assuming measure.vars ", paste0("'", valuecol, "'", collapse = ", "))
  } else {
    valuecol <- as.character(measure.vars)
    if(any(!(valuecol %in% dfnames))) stop("Could not find at least one column in value columns")
  }

  if(!quiet) {
    ignored <- dfnames[!(dfnames %in% id.vars) & !(dfnames %in% tag.vars) & !(dfnames %in% valuecol)]
    if(length(ignored) > 0) message("Ignoring columns ", paste0("'", ignored, "'", collapse=", "))
  }
  return(.reclass(df, id.vars, valuecol, tag.vars))
}

.reclass <- function(df, id.vars, measure.vars, tag.vars, summarised) {
  df <- df[c(id.vars, measure.vars, tag.vars)]
  attr(df, "id.vars") <- id.vars
  attr(df, "measure.vars") <- measure.vars
  attr(df, "tag.vars") <- tag.vars
  if(length(measure.vars) > 1) {
    class(df) <- unique(c("qtag.wide", "qtag", class(df)))
  } else {
    class(df) <- unique(c("qtag.long", "qtag", class(df)))
  }
  if(missing(summarised)) {
    attr(df, "summarised") <- is.summarised(df, quiet=TRUE)
  } else {
    attr(df, "summarised") <- summarised
  }
  return(df)
}

#' @rdname as.qtag
#' @export
qtag <- function(df, ...) as.qtag(df, ...)

#' Extract value column names from a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#' @param quiet Suppress error messages on coersion to a qualifier/tag structure
#'
#' @return A vector of value column names
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' measure.vars(pocmaj)
#'
measure.vars <- function(x, quiet=FALSE) {
  vals <- attr(x, "measure.vars")
  if(!is.null(vals)) {
    return(vals[vals %in% names(x)])
  } else {
    # assume measure.vars are all non id.vars/non tag.vars
    nms <- names(x)
    qlfrs <- id.vars(x)
    tgs <- tag.vars(x)
    vals <- nms[!(nms %in% c(qlfrs, tag.vars))]
    if(length(vals) == 0) stop("Zero value columns found")
    if(!quiet) message("Assuming value columns ", paste0("'", vals, "'", collapse=", "))
    return(vals)
  }
}


#' Extract value qualifier names from a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#' @param quiet Supress error messages on coersion to a qualifier/tag structure
#' @param ... Passed to/from methods
#'
#' @return A vector of qualifier column names
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' id.vars(pocmaj)
#'
id.vars <- function(x, ...) UseMethod("id.vars")

#' @export
#' @rdname id.vars
id.vars.qtag <- function(x, ...) {
  q <- attr(x, "id.vars")
  return(q[q %in% names(x)])
}

#' @export
#' @rdname id.vars
id.vars.grouped_df <- function(x, ...) {
  return(unlist(lapply(attr(x, "vars"), deparse)))
}

#' @export
#' @rdname id.vars
id.vars.data.frame <- function(x, ..., quiet=FALSE) {
  cols <- names(x)
  classes <- sapply(x, class)
  nonnum <- cols[!(classes %in% c("integer", "numeric"))]
  if("age" %in% cols && !("age" %in% nonnum)) {
    nonnum <- c(nonnum, "age")
  }
  if("depth" %in% cols && !("depth" %in% nonnum)) {
    nonnum <- c(nonnum, "depth")
  }
  if(!quiet) message("Assuming id.vars ", paste0("'", nonnum, "'", collapse=", "))
  return(nonnum)
}

#' Extract tag column names from a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#'
#' @return A vector of tag column names
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' tag.vars(pocmaj)
#'
tag.vars <- function(x) {
  tg <- attr(x, "tag.vars")
  return(tg[tg %in% names(x)]) # NULL is ok, since there are often no tag.vars
}


#' Extract if the argument is already summarised
#'
#' Checks the \code{nrow()} of sub-data frames produced by \code{group_by()}.
#' Returns \code{all(lengths==1)}.
#'
#' @param x The object
#' @param quiet Pass \code{TRUE} to supress warnings on coersion to a qualifier/tag structure.
#'
#' @return \code{TRUE} if the argument is summarised, \code{FALSE} otherwise
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, c("core", "depth"))
#' is.summarised(pocmaj)
#' is.summarised(aggregate(pocmaj))
#'
is.summarised <- function(x, quiet=FALSE) {
  . <- NULL; rm(.) # CMD hack
  lengths <- dplyr::do(group(x, quiet=quiet), lengths=nrow(.))$lengths
  return(all(lengths==1))
}

#' Extract value column data from a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#'
#' @return A \link{data.frame} of value column data
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' valuedata(pocmaj)
#'
valuedata <- function(x) {
  return(x[measure.vars(x)])
}

#' Extract qualifier column data from a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#'
#' @return A \link{data.frame} of qualifier column data
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' qualifierdata(pocmaj)
#'
qualifierdata <- function(x) {
  return(x[id.vars(x)])
}

#' Extract tag column data from a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#'
#' @return A \link{data.frame} of tag column data
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' tagdata(pocmaj)
#'
tagdata <- function(x) {
  return(x[tag.vars(x)])
}

#' Convert data to long format
#'
#' @param x A \link{qtag} object
#' @param varname The column name in which column names will be stored.
#' @param quiet Supress warning messages on coersion to a qualifier/tag structure.
#' @param ... Passed to other methods
#'
#' @return A (possibly unchanged) \code{qtag.long} data.frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars = c("core", "depth"))
#' long(pocmaj)
#'
long <- function(x, ...) {
  UseMethod("long")
}


#' Convert data to wide format
#'
#' @param x A \link{qtag} object or one that can be coerced to one.
#' @param colvar The column that contains the names of the to-be columns
#' @param fun.aggregate The aggregation function to be used if id.vars other than colvar
#'   identify more than one row each.
#' @param quiet Supress warning messages on coersion to a qualifier/tag structure.
#' @param ... Passed to other methods
#'
#' @return A (possibly unchanged) \code{qtag.wide} data.frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars = c("core", "depth"))
#' pocmajlong <- long(pocmaj)
#' wide(pocmajlong)
#' wide(pocmaj)
#'
wide <- function(x, ...) {
  UseMethod("wide")
}
# brick <- function(x, ...) UseMethod("brick") not implemented

#' @rdname long
#' @export
long.default <- function(x, ...) {
  long(as.qtag(x), ...)
}

#' @rdname long
#' @export
long.qtag.long <- function(x, ...) {
  return(x)
}

#' @rdname long
#' @export
long.qtag.wide <- function(x, varname="param", quiet=FALSE, ...) {
  valuecol <- measure.vars(x)
  id.vars <- id.vars(x)
  tag.vars <- tag.vars(x)
  dfmelt <- reshape2::melt(x[c(id.vars, valuecol)], id.vars=id.vars, measure.vars=valuecol, value.name="value", variable.name=varname)
  if(length(tag.vars) > 0) {
    dfmelt <- merge(dfmelt, x[c(id.vars, tag.vars)], by=id.vars, all.x=TRUE)
  }
  attr(dfmelt, "measure.vars") <- "value"
  attr(dfmelt, "id.vars") <- c(id.vars, varname)
  attr(dfmelt, "tag.vars") <- tag.vars
  attr(dfmelt, "summarised") <- is.summarised(x)
  class(dfmelt) <- c("qtag.long", "qtag", class(dfmelt))
  if(!quiet) message("Assigning measure.vars column 'value' and qualifier '", varname, "'")
  return(dfmelt)
}

#' @rdname wide
#' @export
wide.default <- function(x, ...) {
  wide(as.qtag(x), ...)
}

#' @rdname wide
#' @export
wide.qtag.wide <- function(x, ...) {
  return(x)
}

#' @rdname wide
#' @export
wide.qtag.long <- function(x, colvar, fun.aggregate, quiet=FALSE, ...) {
  id.vars <- id.vars(x)
  if(missing(colvar)) {
    # assume it is the last qualifier
    colvar <- id.vars[length(id.vars)]
    if(!quiet) message("Assuming column variable '", colvar, "'")
  }
  if(missing(fun.aggregate)) {
    # assume mean
    fun.aggregate <- mean
    if(!is.summarised(x) && !quiet) message("Assuming aggregation function 'mean'")
  }
  castvars <- id.vars[id.vars != colvar]

  dfwide <- reshape2::dcast(x, formula=stats::as.formula(paste0(paste0("`", castvars, "`", collapse="+"), "~`", colvar, "`")),
                  fun.aggregate=fun.aggregate, value.var=measure.vars(x), ...)
  dfnames <- names(dfwide)
  attr(dfwide, "id.vars") <- castvars
  attr(dfwide, "measure.vars") <- dfnames[!(dfnames %in% castvars)]
  attr(dfwide, "summarised") <- TRUE
  class(dfwide) <- c(class(dfwide), "qtag", "qtag.wide")
  return(dfwide)
}


#' Group a qualifier/tag structure
#'
#' Essentially a shortcut for grouping a \link{qtag} object by its id.vars
#'
#' @param qtag A qualifier/tag structure
#' @param quiet Pass \code{TRUE} to supress warnings on coersion to a qualifier tag structure.
#'
#' @return A \code{dplyr} grouped data frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' library(dplyr)
#' pocmajqt <- as.qtag(pocmaj, id.vars = c("core", "depth"))
#' pocmajqt %>% group() %>% summarise(mean(Ca))
#' # equivalent to
#' pocmaj %>% group_by(core, depth) %>% summarise(mean(Ca))
#'
#'
group <- function(qtag, quiet=FALSE) {
  id.vars <- id.vars(qtag, quiet=quiet)
  do.call(dplyr::group_by_, c(list(qtag), as.list(id.vars)))
}


#' Aggregate/Summarise a qualifier/tag structure
#'
#' Summarises a \link{qtag} object such that one value exists for every unique
#' qualifier combination. This is useful for summarising replicates.
#'
#' @param x A \link{qtag} object
#' @param force Use \code{force=FALSE} to only aggregate if the object is not already summarised.
#' @param ... A parameter including at least one unnamed parameter for summarising values.
#'  Additional parameters may be used for aggregating a long format.
#'
#' @return A (possibly unchanged) \code{qtag} object
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmajqt <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' aggregate(pocmajqt)
#' aggregate(pocmajqt, mean)
#' aggregate(long(pocmajqt), mean, sd, length)
#' 
#' @importFrom stats aggregate
#'
aggregate.qtag.long <- function(x, ..., force=TRUE) {
  if(!force && is.summarised(x)) return(x)

  id.vars <- id.vars(x)
  funformats <- generate.call(...)
  argnames <- names(funformats)
  measure.vars <- measure.vars(x)
  sumargs <- list()
  sumargs[[".vals"]] <- gsub(x=funformats[1], pattern="%s", replacement=measure.vars, fixed=TRUE)
  tag.vars <- c()
  if(length(funformats) > 1) {
    for(i in 2:length(funformats)) {
      sumargs[[argnames[i]]] <- gsub(x=funformats[i], pattern="%s", replacement=measure.vars, fixed=TRUE)
    }
    tag.vars <- argnames[2:length(argnames)]
  }
  dfs <- data.frame(do.call(dplyr::summarise_, c(list(group(x)), as.list(sumargs))))
  dfs <- plyr::rename(dfs, c(".vals"=measure.vars))
  attr(dfs, "measure.vars") <- measure.vars
  attr(dfs, "id.vars") <- id.vars
  attr(dfs, "format") <- attr(x, "format")
  attr(dfs, "summarised") <- TRUE
  attr(dfs, "tag.vars") <- tag.vars
  class(dfs) <- c("qtag.long", "qtag", class(dfs))
  return(dfs)
}

#' @rdname aggregate.qtag.long
#' @export
aggregate.qtag.wide <- function(x, ..., force=TRUE) {
  if(!force && is.summarised(x)) return(x)

  id.vars <- id.vars(x)
  funformats <- generate.call(...)
  if(length(funformats) > 1) {
    # would need to return as a brick
    stop("Not implemented")
  }
  measure.vars <- measure.vars(x)

  sumargs <- list()
  for(col in measure.vars) {
    sumargs[[col]] <- gsub(x=funformats, pattern="%s", replacement=col, fixed=TRUE)
  }
  dfs <- data.frame(do.call(dplyr::summarise_, c(list(group(x)), as.list(sumargs))))
  attr(dfs, "measure.vars") <- measure.vars
  attr(dfs, "id.vars") <- id.vars
  attr(dfs, "format") <- attr(x, "format")
  attr(dfs, "summarised") <- TRUE
  class(dfs) <- c("qtag.wide", "qtag", class(dfs))
  return(dfs)
}

#' Combine qualifier/tag structures
#'
#' @param ... Objects to combine
#'
#' @return A \code{qtag.long} object
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, id.vars=c("core", "depth"))
#' newrow <- data.frame(core="POC-2", depth=6, Ca=2100, Ti=4100, V=45)
#' rbind(pocmaj, newrow)
#'
rbind.qtag.long <- function(...) {
  objs <- list(...)
  id.vars <- unique(unlist(lapply(objs, id.vars)))
  tag.vars <- unique(unlist(lapply(objs, tag.vars)))
  measure.vars <- unique(unlist(lapply(objs, measure.vars)))
  summarised <- sapply(objs, is.summarised)
  if(length(measure.vars) != 1) {
    stop("Arguments have multiple measure.vars columns: ", paste0("'", measure.vars, "'", collapse=", "))
  }
  out <- do.call(plyr::rbind.fill, objs)
  class(out) <- c("qtag.long", class(out))
  attr(out, "id.vars") <- id.vars
  attr(out, "measure.vars") <- measure.vars
  attr(out, "tag.vars") <- tag.vars
  attr(out, "summarised") <- all(summarised)
  return(out)
}

#' @export
#' @rdname rbind.qtag.long
rbind.qtag <- function(...) {
  do.call(rbind.qtag.long, lapply(list(...), long))
}

# private for now, generates the lazyeval-like behaviour of the aggregate functions
generate.call <- function(..., .quiet=FALSE) {
  sumargs <- sapply(substitute(list(...))[-1], deparse)
  if(length(sumargs) > 0) {
    argnames <- names(sumargs)
    if(is.null(argnames)) {
      argnames <- rep("", length(sumargs))
    }
    if(argnames[1] != "") stop("Need 1 unnamed argument to apply to measure.vars")
    argsformatted <- sapply(sumargs, function(aggfun) {
      if(grepl(x=aggfun, pattern="(", fixed=TRUE)) {
        gsub(x=aggfun, pattern="([(,])\\s*\\.\\s*([),])", replacement="\\1%s\\2")
      } else {
        paste0(aggfun, "(%s)")
      }
    })
    names(argsformatted) <- sapply(1:length(sumargs), function(i) {
      ifelse(i==1, "", ifelse(argnames[i]=="", sumargs[i], argnames[i]))
    })
    return(argsformatted)
  } else {
    if(!.quiet) message("No aggregation expressions, using function 'mean'")
    out <- "mean(%s)"
    names(out) <- ""
    return(out)
  }
}
