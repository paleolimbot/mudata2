
#' Expand JSON to multiple columns
#'
#' @param x a vector of JSON values or a data.frame with a 'tags' column
#' @param tagcolumn the column comtaining json
#'
#' @return A data.frame with columns added
#' @export
#'
expand.tags <- function(x, tagcolumn='tags', ...) {
  tags <- expand.tags.raw(x[[tagcolumn]])
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

expand.tags.raw <- function(x, ...) {
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
#'
condense.tags <- function(df, tagcolumns, tagcolumn='tags') {
  if(length(tagcolumns) > 0) {
    df[[tagcolumn]] <- sapply(1:nrow(df), function(i) {
      vals <- sapply(tagcolumns, function(name) {
        df[[name]][i]
      })
      vals <- vals[!is.na(vals)]
      paste0('{', paste0('"', names(vals), '": ', vals, collapse=", "), '}')
    })
  } else {
    df[[tagcolumn]] <- '{}'
  }
  return(df[c(names(df)[!(names(df) %in% c(tagcolumns, tagcolumn))], tagcolumn)])
}