
#' Expand JSON to multiple columns
#'
#' @param x a vector of JSON values or a data.frame with a 'tags' column
#' @param tagcolumn the column comtaining json
#'
#' @return A data.frame with columns added
#' @export
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
    if('qtag' %in% class(x)) {
      return(.reclass(x[names(x)!=tagcolumn], qualifiers(x), values(x), c(), is.summarised(x)))
    } else {
      return(x[names(x)!=tagcolumn])
    }
  } else {
    out <- cbind(x, tags)[c(names(x)[names(x) != tagcolumn], names(tags))]
    if('qtag' %in% class(x)) {
      return(.reclass(out, qualifiers(x), values(x), names(tags), is.summarised(x)))
    } else {
      return(out)
    }
  }
}

#' @rdname expand.tags
#' @export 
expand.tags.mudata <- function(x, ...) {
  x$data <- expand.tags(x$data, lazy=TRUE, ...)
  x$locations <- expand.tags(x$locations, lazy=TRUE, ...)
  x$params <- expand.tags(x$params, lazy=TRUE, ...)
  x$datasets <- expand.tags(x$datasets, lazy=TRUE, ...)
  return(x)
}

expandtagsraw <- function(x, ...) {
  dplyr::do(dplyr::group_by(data.frame(.row=1:length(x), .tags=as.character(x), 
                                       stringsAsFactors = FALSE), .row),
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
#' @param df a data frame with tag columns
#' @param tagcolumns column names to be condensed to JSON
#'
#' @return A modified data.frame
#' @export
condense.tags <- function(x, tagcolumns, tagcolumn) UseMethod("condense.tags")

#' @rdname condense.tags
#' @export
condense.tags.data.frame <- function(df, tagcolumns, tagcolumn='tags') {
  if(length(tagcolumns) > 0) {
    df[[tagcolumn]] <- sapply(1:nrow(df), function(i) {
      vals <- sapply(tagcolumns, function(name) {
        v <- df[[name]][i]
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
    df[[tagcolumn]] <- '{}'
  }
  return(df[c(names(df)[!(names(df) %in% c(tagcolumns, tagcolumn))], tagcolumn)])
}

#' @rdname condense.tags
#' @export
condense.tags.mudata <- function(x, ...) {
  x$data <- .tagify(x$data, exnames=c('dataset', 'location', 'param', 'x', 'value'), expand=FALSE)
  x$locations <- .tagify(x$locations, exnames=c('dataset', 'location'), expand=FALSE)
  x$params <- .tagify(x$params, exnames=c('dataset', 'param'), expand=FALSE)
  x$datasets <- .tagify(x$datasets, exnames=c('dataset'), expand=FALSE)
  return(x)
}

.tagify <- function(df, exnames, expand) {
  dfnames <- names(df)
  if(expand) {
    return(expand.tags(df, tagcolumn='tags', lazy=TRUE)[c(exnames, dfnames[!(dfnames %in% exnames)])])
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
