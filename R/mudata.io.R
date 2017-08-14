

#' Read/Write mudata objects
#'
#' @param md a mudata object
#' @param filename file to read/write (can also be a directory)
#' @param overwrite Pass \code{TRUE} to overwrite if \code{zipfile} already exists.
#' @param validate flag to validate mudata object upon read or before write
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
write_mudata_json <- function(md, filename, overwrite=FALSE, validate=TRUE, ...) {
  if(missing(md)) stop("Parameter md is required")
  if(missing(filename)) stop("Parameter filename is required")
  if(validate) validate_mudata(md) # will stop() on error
  
  # writing is simple, it is just a JSON object of the mudata list with some
  # pre-set parameters
  jsonlite::write_json(md, filename, dataframe = "columns", na = "null",
                       digits = NA, POSIXt = "ISO8601", factor = "character",
                       Date = "ISO8601", ...)
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
#' \code{mudata_prepare_column_write} in combination with
#' \link{generate_type_str}, is opposites with \code{mudata_parse_column}.
#'
#' @param x A vector object
#' @param format NA for unknown, csv, or json.
#' @param type_str A type string, generated by \link{generate_type_str}
#' @param ... Passed to methods
#'
#' @return An atomic vector
#' @export
#'
mudata_prepare_column_write <- function(x, format = NA, ...) {
  UseMethod("mudata_prepare_column_write")
}

# default is to just return the object unchanged
#' @rdname mudata_prepare_column_write
#' @export 
mudata_prepare_column_write.default <- function(x, format = NA, ...) x

# datetimes are converted to ISO8601 (ish) with timezone
#' @rdname mudata_prepare_column_write
#' @export 
mudata_prepare_column_write.POSIXt <- function(x, format = NA, ...) {
  strftime(x, "%Y-%m-%dT%H:%M:%S%z", ...)
}

# sfc columns are converted to WKT using sf::st_as_text()
#' @rdname mudata_prepare_column_write
#' @export 
mudata_prepare_column_write.sfc <- function(x, format = NA, ...) {
  sf::st_as_text(x, ...)
}

# time columns are ok using as.character()
#' @rdname mudata_prepare_column_write
#' @export 
mudata_prepare_column_write.hms <- function(x, format = NA, ...) {
  as.character(x)
}

# list columns are converted to JSON strings only if format != "json"
#' @rdname mudata_prepare_column_write
#' @export 
mudata_prepare_column_write.list <- function(x, format = NA, ...) {
  if(is.na(format) || (format != "json")) {
    vapply(x, jsonlite::toJSON, dataframe = "columns", matrix = "rowmajor", 
           Date = "ISO8601", POSIXt = "ISO8601", factor = "string", na = "null",
           digits = NA, FUN.VALUE = character(1), ...)
  } else {
    x
  }
}

#' @rdname mudata_prepare_column_write
#' @export 
mudata_parse_column <- function(x, type_str = NA_character_, ...) {
  # check output class so that parsing doesn't occur if class is already correct
  if(is.na(type_str) || inherits(x, parse_output_class(type_str))) {
    x
  } else {
    as_parser(type_str)(x)
  }
}
