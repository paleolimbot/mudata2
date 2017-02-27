
#' Expand JSON to multiple columns
#' 
#' Expands a character vector of JSON values or a data.frame with a column
#' \code{tagcolumn} to a data.frame with columns for each key in the
#' vector of JSON objects. If x is a mudata object, the operation will
#' be performed on the entire object.
#'
#' @param x a vector of JSON values or a data.frame with a 'tags' column
#' @param tagcolumn the column containing json
#' @param lazy Don't preform expantion if tagcolumn exists in \code{x}
#' @param ... Passed to/from methods
#'
#' @return A data.frame with columns added
#' @export
#' 
#' @examples 
#' data(pocmaj)
#' condensed <- condense.tags(pocmaj, tagcolumns = c("Ca", "Ti", "V"))
#' expand.tags(condensed)
#' 
expand.tags <- function(x, ...) UseMethod('expand.tags')

#' @rdname expand.tags
#' @export 
expand.tags.data.frame <- function(x, tagcolumn='tags', lazy=FALSE, ...) {
  if(lazy && !(tagcolumn %in% names(x))) {
    # assume already expanded
    return(x)
  }
  tags <- expandtagsraw(x[[tagcolumn]])
  if(ncol(tags) == 0) {
    return(x[names(x)!=tagcolumn])
  } else {
    out <- cbind(x, tags)[c(names(x)[names(x) != tagcolumn], names(tags))]
    return(out)
  }
}

#' @rdname expand.tags
#' @export 
expand.tags.mudata <- function(x, ...) {
  x$data <- expand.tags(x$data, lazy=TRUE, ...)
  x$locations <- expand.tags(x$locations, lazy=TRUE, ...)
  x$params <- expand.tags(x$params, lazy=TRUE, ...)
  x$datasets <- expand.tags(x$datasets, lazy=TRUE, ...)
  x$columns <- expand.tags(x$columns, lazy=TRUE, ...)
  return(x)
}

expandtagsraw <- function(x, ...) {
  # CMD hack
  . <- NULL; rm(.)
  dplyr::do(dplyr::group_by_(data.frame(.row=1:length(x), .tags=as.character(x), 
                                       stringsAsFactors = FALSE), ".row"),
            {
              df <- as.data.frame(jsonlite::fromJSON(.$.tags))
              if(nrow(df) > 0) {
                cbind(data.frame(TRUE), df)
              } else {
                cbind(data.frame(TRUE))
              }
            })[c(-1, -2)]
}

#' Condense multiple columns to a single JSON column
#' 
#' Performs the opposite of \link{expand.tags}: collapses the data in
#' \code{tagcolumns} to a single column (called \code{tagcolumn}) that contains
#' a JSON representation of the data that was previously in the tag columns
#' at that row.
#'
#' @param x a data frame with tag columns
#' @param tagcolumns column names to be condensed to JSON
#' @param tagcolumn the column in which to store JSON
#' @param ... passed to/from methods
#'
#' @return A modified data.frame
#' @export
#' 
#' @examples 
#' data(pocmaj)
#' condense.tags(pocmaj, tagcolumns = c("Ca", "Ti", "V"))
#' 
condense.tags <- function(x, ...) UseMethod("condense.tags")

#' @rdname condense.tags
#' @export
condense.tags.data.frame <- function(x, tagcolumns, tagcolumn='tags', ...) {
  if(length(tagcolumns) > 0) {
    x[[tagcolumn]] <- sapply(1:nrow(x), function(i) {
      vals <- sapply(tagcolumns, function(name) {
        v <- x[[name]][i]
        if("numeric" %in% class(v) || "integer" %in% class(v) || is.na(v)) {
          return(v)
        } else {
          return(paste0('"', v, '"'))
        }
      })
      vals <- vals[!is.na(vals) & (vals != '""')]
      if(length(vals) > 0) {
        return(paste0('{', paste0('"', names(vals), '": ', vals, collapse=", "), '}'))
      } else {
        return('{}')
      }
    })
  } else {
    x[[tagcolumn]] <- '{}'
  }
  return(x[c(names(x)[!(names(x) %in% c(tagcolumns, tagcolumn))], tagcolumn)])
}

#' @rdname condense.tags
#' @export
condense.tags.mudata <- function(x, ...) {
  x$data <- .tagify(x$data, exnames=c('dataset', 'location', 'param', 'x', 'value'), expand=FALSE)
  x$locations <- .tagify(x$locations, exnames=c('dataset', 'location'), expand=FALSE)
  x$params <- .tagify(x$params, exnames=c('dataset', 'param'), expand=FALSE)
  x$datasets <- .tagify(x$datasets, exnames=c('dataset'), expand=FALSE)
  x$columns <- .tagify(x$columns, exnames=c('dataset', 'table', 'column'), expand=FALSE)
  return(x)
}

.tagify <- function(df, exnames, expand) {
  dfnames <- names(df)
  if(expand) {
    expanded <- expand.tags(df, tagcolumn='tags', lazy=TRUE)
    dfnames <- names(expanded)
    return(expanded[c(exnames, dfnames[!(dfnames %in% exnames)])])
  } else {
    if('tags' %in% dfnames) {
      df$tags <- as.character(df$tags)
      df$tags[is.na(df$tags)] <- '{}'
      return(df[c(exnames, 'tags')])
    } else {
      tagnames <- dfnames[!(dfnames %in% exnames)]
      return(condense.tags(df, tagnames, 'tags'))
    }
  }
}
