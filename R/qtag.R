

#' Convert an object to a qualifier/tag structure
#'
#' @param df A \code{data.frame} or similar object
#' @param .values Column names containing the values of interest
#' @param .qualifiers Column names of qualifying values
#' @param .tags Column names of tag values
#' @param quiet Use \code{quiet=TRUE} to suppress error messages
#'
#' @return An object of type \code{qtag}, which is essentially the unchanged
#'   input with qualifiers, values, and tags information attached.
#'
#' @export
#'
#' @examples
#' data("pocmaj")
#' pocmaj <- as.qtag(pocmaj, qualifiers = c("core", "depth"))
#' long(pocmaj)
#' aggregate(pocmaj)
#' aggregate(long(pocmaj))
#'
as.qtag <- function(df, .qualifiers, .values, .tags, quiet=FALSE) {
  dfnames <- names(df)
  if(missing(.qualifiers)) {
    .qualifiers <- qualifiers(df)
    if(length(.qualifiers) == 0) {
      df$.row_id <- 1:nrow(df)
      .qualifiers <- ".row_id"
    }
  } else {
    .qualifiers <- as.character(.qualifiers)
  }
  if(missing(.tags)) {
    .tags <- NULL
  } else {
    .tags <- as.character(.tags)
  }
  if(missing(.values)) {
    valuecol <- dfnames[!(dfnames %in% .qualifiers) & !(dfnames %in% .tags)]
    if(!quiet) message("Assuming value colums ", paste0("'", valuecol, "'", collapse = ", "))
  } else {
    valuecol <- as.character(.values)
    if(any(!(valuecol %in% dfnames))) stop("Could not find at least one column in value columns")
  }

  if(!quiet) {
    ignored <- dfnames[!(dfnames %in% .qualifiers) & !(dfnames %in% .tags) & !(dfnames %in% valuecol)]
    if(length(ignored) > 0) message("Ignoring columns ", paste0("'", ignored, "'", collapse=", "))
  }
  return(.reclass(df, .qualifiers, valuecol, .tags))
}

.reclass <- function(df, qualifiers, values, tags, summarised) {
  df <- df[c(qualifiers, values, tags)]
  attr(df, "qualifiers") <- qualifiers
  attr(df, "values") <- values
  attr(df, "tags") <- tags
  if(length(values) > 1) {
    class(df) <- c("qtag.wide", "qtag", class(df))
  } else {
    class(df) <- c("qtag.long", "qtag", class(df))
  }
  if(missing(summarised)) {
    attr(df, "summarised") <- is.summarised(df, quiet=TRUE)
  } else {
    attr(df, "summarised") <- summarised
  }
  return(df)
}

#' Extract value column names from a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#'
#' @return A vector of value column names
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' values(pocmaj)
#'
values <- function(x, quiet=FALSE) {
  vals <- attr(x, "values")
  if(!is.null(vals)) {
    return(vals)
  } else {
    # assume values are all non qualifiers/non tags
    nms <- names(x)
    qlfrs <- qualifiers(x)
    tgs <- tags(x)
    vals <- nms[!(nms %in% c(qlfrs, tags))]
    if(length(vals) == 0) stop("Zero value columns found")
    if(!quiet) message("Assuming value columns ", paste0("'", vals, "'", collapse=", "))
    return(vals)
  }
}


#' Extract value qualifier names from a qualifier/tag structure
#'
#' @param x A \link{qtag} object
#'
#' @return A vector of qualifier column names
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' qualifiers(pocmaj)
#'
qualifiers <- function(x, ...) UseMethod("qualifiers")

#' @export
#' @rdname qualifiers
qualifiers.qtag <- function(x, ...) {
  return(attr(x, "qualifiers"))
}

#' @export
#' @rdname qualifiers
qualifiers.grouped_df <- function(x, ...) {
  return(unlist(lapply(attr(x, "vars"), deparse)))
}

#' @export
#' @rdname qualifiers
qualifiers.data.frame <- function(x, ..., quiet=FALSE) {
  cols <- names(x)
  classes <- sapply(x, class)
  nonnum <- cols[!(classes %in% c("integer", "numeric"))]
  if("age" %in% cols && !("age" %in% nonnum)) {
    nonnum <- c(nonnum, "age")
  }
  if("depth" %in% cols && !("depth" %in% nonnum)) {
    nonnum <- c(nonnum, "depth")
  }
  if(!quiet) message("Assuming qualifiers ", paste0("'", nonnum, "'", collapse=", "))
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
#' pocmaj <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' tags(pocmaj)
#'
tags <- function(x) {
  return(attr(x, "tags")) # NULL is ok, since there are often no tags
}


#' Extract if the argument is already summarised
#'
#' Checks the \code{nrow()} of sub-data frames produced by \code{group_by()}.
#' Returns \code{all(lengths==1)}.
#'
#' @param x The object
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
#' pocmaj <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' valuedata(pocmaj)
#'
valuedata <- function(x) {
  return(x[values(x)])
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
#' pocmaj <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' qualifierdata(pocmaj)
#'
qualifierdata <- function(x) {
  return(x[qualifiers(x)])
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
#' pocmaj <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' tagdata(pocmaj)
#'
tagdata <- function(x) {
  return(x[tags(x)])
}

#' Convert data to long format
#'
#' @param x A \link{qtag} object
#' @param ... Passed to other methods
#'
#' @return A (possibly unchanged) \code{qtag.long} data.frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, qualifiers = c("core", "depth"))
#' long(pocmaj)
#'
long <- function(x, ...) {
  UseMethod("long")
}


#' Convert data to wide format
#'
#' @param x A \link{qtag} object
#' @param ... Passed to other methods
#'
#' @return A (possibly unchanged) \code{qtag.wide} data.frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmaj <- as.qtag(pocmaj, qualifiers = c("core", "depth"))
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
long.qtag.wide <- function(qtag, varname="column", quiet=FALSE) {
  valuecol <- values(qtag)
  qualifiers <- qualifiers(qtag)
  tags <- tags(qtag)
  dfmelt <- reshape2::melt(qtag[c(qualifiers, valuecol)], id.vars=qualifiers, measure.vars=valuecol, value.name="values", variable.name=varname)
  if(length(tags) > 0) {
    dfmelt <- merge(dfmelt, qtag[c(qualifiers, tags)], by=qualifiers, all.x=TRUE)
  }
  attr(dfmelt, "values") <- "values"
  attr(dfmelt, "qualifiers") <- c(qualifiers, varname)
  attr(dfmelt, "tags") <- tags
  attr(dfmelt, "summarised") <- is.summarised(qtag)
  class(dfmelt) <- c("qtag.long", "qtag", class(dfmelt))
  if(!quiet) message("Assigning values column 'values' and qualifier '", varname, "'")
  return(dfmelt)
}

#' @rdname wide
#' @export
wide.default <- function(x, ...) {
  wide(as.qtag(x), ...)
}

#' @rdname wide
#' @export
wide.qtag.wide <- function(x) {
  return(x)
}

#' @rdname wide
#' @export
wide.qtag.long <- function(qtag, colvar, fun.aggregate, ..., quiet=FALSE) {
  qualifiers <- qualifiers(qtag)
  if(missing(colvar)) {
    # assume it is the last qualifier
    colvar <- qualifiers[length(qualifiers)]
    if(!quiet) message("Assuming column variable '", colvar, "'")
  }
  if(missing(fun.aggregate)) {
    # assume mean
    fun.aggregate <- mean
    if(!is.summarised(qtag) && !quiet) message("Assuming aggregation function 'mean'")
  }
  castvars <- qualifiers[qualifiers != colvar]

  dfwide <- reshape2::dcast(qtag, formula=as.formula(paste0(paste0("`", castvars, "`", collapse="+"), "~`", colvar, "`")),
                  fun.aggregate=fun.aggregate, value.var=values(qtag), ...)
  dfnames <- names(dfwide)
  attr(dfwide, "qualifiers") <- castvars
  attr(dfwide, "values") <- dfnames[!(dfnames %in% castvars)]
  attr(dfwide, "summarised") <- TRUE
  class(dfwide) <- c(class(dfwide), "qtag", "qtag.wide")
  return(dfwide)
}


#' Group a qualifier/tag structure
#'
#' Essentially a shortcut for grouping a \link{qtag} object by its qualifiers
#'
#' @param qtag
#'
#' @return A \code{dplyr} grouped data frame
#' @export
#'
#' @examples
#' data(pocmaj)
#' library(dplyr)
#' pocmajqt <- as.qtag(pocmaj, qualifiers = c("core", "depth"))
#' pocmajqt %>% group() %>% summarise(mean(Ca))
#' # equivalent to
#' pocmaj %>% group_by(core, depth) %>% summarise(mean(Ca))
#'
#'
group <- function(qtag, quiet=FALSE) {
  qualifiers <- qualifiers(qtag, quiet=quiet)
  do.call(dplyr::group_by_, c(list(qtag), as.list(qualifiers)))
}


#' Aggregate/Summarise a qualifier/tag structure
#'
#' Summarises a \link{qtag} object such that one value exists for every unique
#' qualifier combination. This is useful for summarising replicates.
#'
#' @param x A \link{qtag} object
#' @param ... A parameter including at least one unnamed parameter for summarising values.
#'  Additional parameters may be used for aggregating a long format.
#'
#' @return A (possibly unchanged) \code{qtag} object
#' @export
#'
#' @examples
#' data(pocmaj)
#' pocmajqt <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' aggregate(pocmajqt)
#' aggregate(pocmajqt, mean)
#' aggregate(long(pocmajqt), mean, sd, length)
#' 
#' @importFrom stats aggregate
#'
aggregate.qtag.long <- function(x, ...) {
  if(is.summarised(x)) return(x)

  qualifiers <- qualifiers(x)
  funformats <- generate.call(...)
  argnames <- names(funformats)
  values <- values(x)
  sumargs <- list()
  sumargs[[".vals"]] <- gsub(x=funformats[1], pattern="%s", replacement=values, fixed=TRUE)
  tags <- c()
  if(length(funformats) > 1) {
    for(i in 2:length(funformats)) {
      sumargs[[argnames[i]]] <- gsub(x=funformats[i], pattern="%s", replacement=values, fixed=TRUE)
    }
    tags <- argnames[2:length(argnames)]
  }
  dfs <- data.frame(do.call(dplyr::summarise_, c(list(group(x)), as.list(sumargs))))
  dfs <- plyr::rename(dfs, c(".vals"=values))
  attr(dfs, "values") <- values
  attr(dfs, "qualifiers") <- qualifiers
  attr(dfs, "format") <- attr(x, "format")
  attr(dfs, "summarised") <- TRUE
  attr(dfs, "tags") <- tags
  class(dfs) <- c("qtag.long", "qtag", class(dfs))
  return(dfs)
}

#' @rdname aggregate.qtag.long
#' @export
aggregate.qtag.wide <- function(x, ...) {
  if(is.summarised(x)) return(x)

  qualifiers <- qualifiers(x)
  funformats <- generate.call(...)
  if(length(funformats) > 1) {
    # need to return as a brick
    stop("Not implemented")
    argnames <- names(funformat)
  }
  values <- values(x)

  sumargs <- list()
  for(col in values) {
    sumargs[[col]] <- gsub(x=funformats, pattern="%s", replacement=col, fixed=TRUE)
  }
  dfs <- data.frame(do.call(dplyr::summarise_, c(list(group(x)), as.list(sumargs))))
  attr(dfs, "values") <- values
  attr(dfs, "qualifiers") <- qualifiers
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
#' pocmaj <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' newrow <- data.frame(core="POC-2", depth=6, Ca=2100, Ti=4100, V=45)
#' rbind(pocmaj, newrow)
#'
rbind.qtag.long <- function(...) {
  objs <- list(...)
  qualifiers <- unique(unlist(lapply(objs, qualifiers)))
  tags <- unique(unlist(lapply(objs, tags)))
  values <- unique(unlist(lapply(objs, values)))
  summarised <- sapply(objs, is.summarised)
  if(length(values) != 1) {
    stop("Arguments have multiple values columns: ", paste0("'", values, "'", collapse=", "))
  }
  out <- do.call(plyr::rbind.fill, objs)
  class(out) <- c("qtag.long", class(out))
  attr(out, "qualifiers") <- qualifiers
  attr(out, "values") <- values
  attr(out, "tags") <- tags
  attr(out, "summarised") <- all(summarised)
  return(out)
}

#' @export
#' @rdname rbind.qtag.long
rbind.qtag <- function(...) {
  do.call(rbind.qtag.long, lapply(list(...), long))
}


#' Replace/rename a column in an object
#'
#' Essentially a thin convenience wrapper around \code{plyr::rename(x, list(...))},
#' except \link{qtag} objects have their qualifiers/tags/values attributes properly modified
#'
#' @param ... Key/value pairs to replace in the form \code{oldval="newval"}
#'
#' @return A copy of the modified object
#' @export
#'
#' @examples
#' data(pocmaj)
#' renamecol(pocmaj, Ca="Calcium")
#' pocmaj2 <- as.qtag(pocmaj, qualifiers=c("core", "depth"))
#' pocmaj2 <- renamecol(pocmaj2, Ca="Calcium")
#' attr(pocmaj2, "values")
#'
replacecol <- function(x, ...) UseMethod("renamecol")

#' @export
#' @rdname replacecol
replacecol.default <- function(x, ...) plyr::rename(x, list(...))
#' @export
#' @rdname replacecol
replacecol.qtag <- function(x, ...) {
  replace <- list(...)
  out <- plyr::rename(x, replace)
  attr(out, "qualifiers") <- replaceval(qualifiers(x), replace)
  attr(out, "values") <- replaceval(values(x), replace)
  attr(out, "tags") <- replaceval(tags(x), replace)
  attr(out, "summarised") <- is.summarised(x)
  class(out) <- class(x)
  return(out)
}


#' Replace/rename values in a vector
#'
#' @param x Vector of values to replace
#' @param ... Key/value pairs in the form \code{oldvalue="newvalue"}
#' @param defaultValue A vector of values to use as the default should the value not
#'   be found in \code{...}
#'
#' @return A vector with values replaced
#' @export
#'
#' @examples
#' x <- c("fish", "fish", "fish", "whistle")
#' replaceval(x, fish="newfish")
#' replaceval(x, whistle="newwhistle")
#' replaceval(x, fish="newfish", defaultValue="not a fish")
#'
replaceval <- function(x, ..., defaultValue=x) {
  replacer <- list(...)
  replacernames <- names(replacer)
  if(length(defaultValue) != length(x)) {
    defaultValue <- rep(defaultValue, length.out=length(x))
  }
  sapply(1:length(x), function(i) {
    v <- x[i]
    ifelse(v %in% replacernames, replacer[[v]], defaultValue[i])
  })
}




# private for now, generates the lazyeval-like behaviour of the aggregate functions
generate.call <- function(..., .quiet=FALSE) {
  sumargs <- sapply(substitute(list(...))[-1], deparse)
  if(length(sumargs) > 0) {
    argnames <- names(sumargs)
    if(is.null(argnames)) {
      argnames <- rep("", length(sumargs))
    }
    if(argnames[1] != "") stop("Need 1 unnamed argument to apply to values")
    argsformatted <- sapply(sumargs, function(aggfun) {
      if(grepl(x=aggfun, pattern="(", fixed=TRUE)) {
        gsub(x=aggfun, pattern="(.", replacement="(%s", fixed=TRUE)
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
