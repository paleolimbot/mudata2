

#' Read/Write mudata objects
#'
#' @param md a mudata object
#' @param filename file to read/write (can also be a directory)
#' @param overwrite Pass \code{TRUE} to overwrite if \code{zipfile} already exists.
#' @param validate flag to validate mudata object upon read or before write
#' @param update_columns Update the columns table "type" column to reflect the internal
#'   R types of columns (reccommended).
#' @param pretty Produce pretty or minified JSON output
#' @param txt JSON text from which to read a mudata object.
#' @param expand.tags flag to expand tags to columns
#' @param retype Pass \code{TRUE} to retype columns based on the 'type' column of the 'columns'
#'   table.
#' @param load a list of csv files (without the .csv extension) to load from the source.
#' @param ... passed to read/write.csv
#'
#' @export
#' 
#' @examples 
#' data(kentvillegreenwood)
#' # read/write to zip
#' outfile <- tempfile(fileext=".zip")
#' #write.mudata(kentvillegreenwood, outfile)
#' #md <- read.mudata(outfile)
#' #md <- read.mudata(outfile, retype=TRUE)
#' unlink(outfile)
#' 
#' # read/write to JSON
#' outfile <- tempfile(fileext=".json")
#' #write.mudata(kentvillegreenwood, outfile)
#' #md <- read.mudata(outfile)
#' #md <- read.mudata(outfile, retype=TRUE)
#' unlink(outfile)
#'
write.mudata <- function(md, filename, ...) {
  if(grepl("[.]zip$", filename)) {
    write.mudata.zip(md, filename, ...)
  } else if(grepl("[.]json$", filename)) {
    write_mudata_json(md, filename, ...)
  } else {
    stop("Don't know which format to write file '", filename, "'")
  }
}

#' @rdname write.mudata
#' @export
read.mudata <- function(filename, ...) {
  if(grepl("[.]zip$", filename) || dir.exists(filename)) {
    read.mudata.zip(filename, ...)
  } else if(grepl("[.]json$", filename)) {
    read.mudata.json(filename, ...)
  } else {
    stop("Don't know which format to read file '", filename, "'")
  }
}

#' @rdname write.mudata
#' @export
write.mudata.zip <- function(md, filename, overwrite=FALSE, expand.tags=TRUE, validate=TRUE, ...) {
  if(missing(md)) stop("Parameter md is required")
  if(missing(filename)) stop("Parameter filename is required")
  if(validate) validate_mudata(md) # will stop() on invalid mudata
  
  if(file.exists(filename)) {
    if(overwrite) {
      unlink(filename)
    } else {
      stop("File ", filename, ' exists...pass overwrite=TRUE to continue')
    }
  }
  zipfolder <- tempfile()
  dir.create(zipfolder)
  if(expand.tags) {
    md <- expand.tags(md)
  } else {
    md <- condense.tags(md)
  }
  
  mdnames <- names(md)
  filesWritten <- sapply(mdnames, function(tname) {
    .writetocsv(md[[tname]], file.path(zipfolder, paste0(tname, ".csv")), ...)
  })
  filenames <- paste0(mdnames[filesWritten], ".csv")
  cwd <- getwd()
  setwd(zipfolder)
  tryCatch(utils::zip("zipfile.zip", filenames),
           error=function(err) {
             setwd(cwd)
             unlink(zipfolder, recursive = TRUE)
             stop(err)
           })
  setwd(cwd)
  tmpzip <- file.path(zipfolder, "zipfile.zip")
  file.copy(tmpzip, filename)
  unlink(tmpzip)
  unlink(zipfolder, recursive = TRUE)
}

#' @rdname write.mudata
#' @export
read.mudata.zip <- function(filename, validate=TRUE, expand.tags=TRUE, retype=TRUE,
                        load=c("data", "locations", "params", "datasets", "columns"), ...) {
  if(!("data" %in% load)) stop("'data' must be in argument load")
  tmpfold <- tempfile()
  deleteOnExit <- TRUE
  if(dir.exists(filename)) {
    tmpfold <- filename
    deleteOnExit <- FALSE
  } else {
    utils::unzip(filename, exdir = tmpfold)
  }
  
  md <- tryCatch({
    obj <- sapply(load, function(name) {
      fname <- list.files(tmpfold, pattern=paste0(name, '.csv'), 
                          full.names = TRUE, recursive = TRUE)
      if(name == "data" && length(fname)==0) stop('data.csv not found')
      .readfiletocsv(fname, validate, ...)
    }, USE.NAMES = TRUE, simplify = FALSE)
    
    mud <- mudata(data=obj$data, locations=obj$locations, params=obj$params, datasets=obj$datasets,
                  columns=obj$columns, validate=validate)
    morenames <- load[!(load %in% c("data", "locations", "params", "datasets", "columns"))]
    for(name in morenames) {
      mud[[name]] <- obj[[name]]
    }
    mud
  }, error=function(err) {
    if(deleteOnExit) {
      unlink(tmpfold, recursive = TRUE)
    }
    stop(err)
  })
  if(deleteOnExit) {
    unlink(tmpfold, recursive = TRUE)
  }
  return(md)
}

.writetocsv <- function(df, fname, ...) {
  if(!is.null(df)) {
    utils::write.csv(df, fname, row.names = FALSE, ...)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

.readfiletocsv <- function(fname, validate, ...) {
  if(length(fname) == 0) {
    return(NULL)
  } else {
    df <- try(utils::read.csv(path.expand(fname[1]), stringsAsFactors = FALSE, ...), silent = TRUE)
    if(validate && !('data.frame' %in% class(df))) {
      return(NULL)
    } else {
      return(df)
    }
  }
}

#' @rdname write.mudata
#' @export
write_mudata_json <- function(md, filename, overwrite = FALSE, validate = TRUE, 
                              update_columns = TRUE, pretty = TRUE, ...) {
  
  # check if output file exists, stop if overwrite = FALSE
  if(file.exists(filename) && !overwrite) stop("File ", filename, 
                                               " exists. Use ovewrite = TRUE to overwrite.")
  
  # call mudate_write_json_common with fun = jsonlite::write_json
  write_mudata_json_common(md, jsonlite::write_json,
                           path = filename, validate = validate, 
                           update_columns = update_columns, pretty = pretty, ...)
}

#' @rdname write.mudata
#' @export
to_mudata_json <- function(md, validate = TRUE, update_columns = TRUE, pretty = FALSE,
                           ...) {
  # call mudate_write_json_common with fun = jsonlite::toJSON
  write_mudata_json_common(md, jsonlite::toJSON, validate = validate, 
                           update_columns = update_columns, pretty = pretty, ...)
}

write_mudata_json_common <- function(md, fun, validate = TRUE, update_columns = TRUE, 
                                     pretty = TRUE, ...) {
  
  # prepare using mudata_write_common
  md_write <- write_mudata_common(md, validate = validate, 
                                  update_columns = update_columns, format = "json")
  
  # add attribute information as an additional list item
  md_write[["_mudata"]] <- list(x_columns = attr(md, "x_columns"))
  
  # writing is simple, it is just a JSON object of the mudata list with some
  # pre-set parameters
  fun(md_write, ..., dataframe = "columns", na = "null",
      digits = NA, POSIXt = "ISO8601", factor = "string",
      Date = "ISO8601", auto_unbox = TRUE, pretty = pretty)
}

#' @rdname write.mudata
#' @export
read_mudata_json <- function(filename, validate = TRUE, ...) {
  read_mudata_json_common(jsonlite::read_json, path = filename, validate = validate, ...)
}

#' @rdname write.mudata
#' @export
from_mudata_json <- function(txt, validate = TRUE, ...) {
  read_mudata_json_common(jsonlite::fromJSON, txt = txt, validate = validate, ...)
}

read_mudata_json_common <- function(fun, validate = TRUE, ...) {
  # read to object using fun
  obj <- fun(..., simplifyDataFrame = FALSE, simplifyMatrix = TRUE, simplifyVector = TRUE)
  
  # check basic names, types
  if(!is.list(obj)) stop("JSON object is not a list")
  if(!("data" %in% names(obj))) stop("JSON object is missing the data table")
  mudata_tbls <- c("data", "locations", "params", "datasets", "columns")
  wrong_type_tbls <- vapply(names(obj), function(tbl_name) {
    (tbl_name %in% mudata_tbls) && !is.list(obj[[tbl_name]])
  }, logical(1))
  if(any(wrong_type_tbls)) {
    stop("JSON objects of incorrect type: ", 
         paste(names(obj)[wrong_type_tbls], collapse = ", "))
  }
  
  # extract and remove metadata, if present
  metadata <- obj[["_mudata"]]
  x_columns <- obj[["_mudata"]]$x_columns
  obj[["_mudata"]] <- NULL
  
  # get column type information
  if(("columns" %in% names(obj)) && 
     all(c("table", "column", "type") %in% names(obj$columns))) {
    # generate small version of column type table and validate it
    type_str_tbl <- tibble::tibble(table = obj$columns$table,
                                column = obj$columns$column, type = obj$columns$type) %>%
      dplyr::distinct()
    # types need to be unique to be read by this function
    .checkunique(type_str_tbl, 'columns', c("table", "column"))
    
    # create list of type_str named lists
    . <- NULL; rm(.) # CMD hack
    type_str_tbl <- type_str_tbl %>%
      tidyr::nest_(key_col = "types", nest_cols = c("column", "type"))
    type_strs <- type_str_tbl %>%
      .$types %>%
      lapply(tibble::deframe) %>%
      stats::setNames(type_str_tbl$table)
  } else {
    type_strs <- list()
  }
  
  # pass mudata tables to mudata_parse_tbl
  md <- lapply(intersect(mudata_tbls, names(obj)), 
               function(tbl_name) {
                 type_str <- type_strs[[tbl_name]]
                 if(is.null(type_str)) {
                   type_str <- NA_character_
                 }
                 mudata_parse_tbl(obj[[tbl_name]], type_str = type_str)
               }) %>%
    stats::setNames(intersect(mudata_tbls, names(obj)))
  
  # pass to mudata
  md <- mudata(data = md$data, locations = md$locations, params = md$params, 
               datasets = md$datasets, columns = md$columns, x_columns = x_columns,
               validate = validate)
  
  # add additional obj list items and return
  new_mudata(c(md, obj[setdiff(names(obj), mudata_tbls)]), x_columns = x_columns)
}



# common preparation for write_mudata objects
write_mudata_common <- function(md, validate = TRUE, update_columns = TRUE, format = NA) {
  # validate object
  if(validate) validate_mudata(md)
  
  # update columns table
  if(update_columns) {
    md <- update_columns_table(md, quiet = FALSE)
  }
  
  # prepare the md to be written
  md_write <- lapply(md, mudata_prepare_tbl, format = format)
  
  # return md_write
  md_write
}

# updates the columns table when needed, with an optional warning
update_columns_table <- function(md, quiet = FALSE) {
  
  # generate columns table from original mudata, join to existing cols table
  generated_cols <- generate_type_tbl(md) %>%
    dplyr::right_join(md$columns, by = c("dataset", "table", "column"),
                      prefix = c(".x", ".y"))
  
  # check that type.x and type.y are the same, if type was already in the columns table
  if("type" %in% colnames(md$columns)) {
    type.x <- NULL; rm(type.x); type.y <- NULL; rm(type.y) # CMD hack
    replaced_types <- generated_cols %>% dplyr::filter(type.x != type.y)
    if(nrow(replaced_types) > 0) {
      
      if(!quiet) {
        message(sprintf("Replacing types %s with %s",
                        with(replaced_types, paste(dataset, table, column, type.x, sep = "/")),
                        with(replaced_types, paste(dataset, table, column, type.y, sep = "/"))))
      }
    # overwrite the type column in md$columns
    md$columns <- md$columns %>%
      dplyr::left_join(generated_cols, by = c("dataset", "table", "column"),
                       prefix = c(".x", ".y")) %>%
      dplyr::mutate(type = type.y) %>%
      dplyr::select(-type.y, -type.x)
    }
  } else {
    # assign type column in md$columns
    md$columns <- md$columns %>%
      dplyr::left_join(generated_cols, by = c("dataset", "table", "column"))
  }
  
  # return md
  md
}

#' @rdname write.mudata
#' @export
read.mudata.json <- function(filename, validate=TRUE, retype=TRUE,
                             load = c("data", "locations", "params", "datasets", "columns"), 
                             ...) {
  obj <- sapply(jsonlite::read_json(filename, simplifyVector = TRUE, ...), 
                function(obj) {
                  if(is.list(obj)) {
                    data.frame(sapply(obj, unlist, USE.NAMES = TRUE, simplify = FALSE), 
                               stringsAsFactors = FALSE)
                  } else {
                    obj
                  }
                }, USE.NAMES = TRUE, simplify = FALSE)
  mud <- mudata(data=obj$data, locations=obj$locations, params=obj$params, datasets=obj$datasets,
                columns=obj$columns, validate=validate)
  morenames <- load[!(load %in% c("data", "locations", "params", "datasets", "columns"))]
  for(name in morenames) {
    mud[[name]] <- obj[[name]]
  }
  return(mud)
}


#' Prepare mudata table columns for writing
#' 
#' This set of generics is similar to \link[readr]{output_column} in that it
#' converts columns to a form suitable to writing.
#' \code{mudata_prepare_column} in combination with
#' \link{generate_type_str}, is opposites with \code{mudata_parse_column}.
#'
#' @param x A an object
#' @param format NA for unknown, csv, or json.
#' @param type_str A type string, generated by \link{generate_type_str}
#' @param ... Passed to methods
#'
#' @return An atomic vector
#' @export
#'
mudata_prepare_column <- function(x, format = NA, ...) {
  UseMethod("mudata_prepare_column")
}

#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_tbl <- function(x, format = NA, ...) {
  UseMethod("mudata_prepare_tbl")
}

# by default just return the object
#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_tbl.default <- function(x, format = NA, ...) {
  x
}

#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_tbl.tbl <- function(x, format = NA, ...) {
  # collect foreign data frames
  x <- dplyr::collect(x)
  
  # apply mudata_prepare_column, make a tbl
  lapply(x, mudata_prepare_column, format = format, ...) %>%
    tibble::as_tibble()
}

#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_tbl.data.frame <- function(x, format = NA, ...) {
  mudata_prepare_tbl.tbl(x, format = format, ...)
}

# default is to just return the object unchanged
#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_column.default <- function(x, format = NA, ...) x

# datetimes are converted to ISO8601 (ish) with timezone
#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_column.POSIXt <- function(x, format = NA, ...) {
  tzone <- attr(x, "tzone")
  if(is.null(tzone)) {
    tzone <- ""
  }
  strftime(x, "%Y-%m-%dT%H:%M:%S", tz = tzone, ...)
}

# sfc columns are converted to WKT using sf::st_as_text()
#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_column.sfc <- function(x, format = NA, ...) {
  sf::st_as_text(x, ...)
}

# time columns are ok using as.character()
#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_column.hms <- function(x, format = NA, ...) {
  as.character(x)
}

# list columns are converted to JSON strings only if format != "json"
#' @rdname mudata_prepare_column
#' @export 
mudata_prepare_column.list <- function(x, format = NA, ...) {
  if(is.na(format) || (format != "json")) {
    vapply(x, jsonlite::toJSON, dataframe = "columns", matrix = "rowmajor", 
           auto_unbox = TRUE,
           Date = "ISO8601", POSIXt = "ISO8601", factor = "string", na = "null",
           digits = NA, FUN.VALUE = character(1), ...)
  } else {
    x
  }
}

#' @rdname mudata_prepare_column
#' @export 
mudata_parse_column <- function(x, type_str = NA_character_, ...) {
  # check output class so that parsing doesn't occur if class is already correct
  if(is.na(type_str) || inherits(x, parse_output_class(type_str))) {
    x
  } else {
    as_parser(type_str)(x, ...)
  }
}

#' @rdname mudata_prepare_column
#' @export 
mudata_parse_tbl <- function(x, type_str = NA_character_, ...) {
  # apply mudata_parse_column to x, using type_str as a named vector
  lapply(names(x), function(col_name) {
    mudata_parse_column(x[[col_name]], type_str = type_str[col_name], ...)
  }) %>% 
    stats::setNames(names(x)) %>%
    tibble::as_tibble()
}
