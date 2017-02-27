

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
#' write.mudata(kentvillegreenwood, outfile)
#' md <- read.mudata(outfile)
#' md <- read.mudata(outfile, retype=TRUE)
#' plot(subset(md, params=c("meantemp", "maxtemp")))
#' unlink(outfile)
#' 
#' # read/write to JSON
#' outfile <- tempfile(fileext=".json")
#' write.mudata(kentvillegreenwood, outfile)
#' md <- read.mudata(outfile)
#' md <- read.mudata(outfile, retype=TRUE)
#' plot(subset(md, params=c("meantemp", "maxtemp")))
#' unlink(outfile)
#'
write.mudata <- function(md, filename, ...) {
  if(grepl("[.]zip$", filename)) {
    write.mudata.zip(md, filename, ...)
  } else if(grepl("[.]json$", filename)) {
    write.mudata.json(md, filename, ...)
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
  if(validate) validate.mudata(md) # will stop() on invalid mudata
  
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
                  columns=obj$columns, retype=retype,
                  expand.tags=expand.tags, validate=validate)
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
write.mudata.json <- function(md, filename, overwrite=FALSE, expand.tags=TRUE, validate=TRUE, ...) {
  if(missing(md)) stop("Parameter md is required")
  if(missing(filename)) stop("Parameter filename is required")
  if(validate) validate.mudata(md) # will stop() on error
  
  # writing is simple, it is just a JSON object of the mudata list.
  jsonlite::write_json(md, filename, dataframe = "columns", na="null",
                       digits=NA, ...)
}

#' @rdname write.mudata
#' @export
read.mudata.json <- function(filename, validate=TRUE, expand.tags=TRUE, retype=TRUE,
                             load=c("data", "locations", "params", "datasets", "columns"), ...) {
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
                columns=obj$columns, retype=retype,
                expand.tags=expand.tags, validate=validate)
  morenames <- load[!(load %in% c("data", "locations", "params", "datasets", "columns"))]
  for(name in morenames) {
    mud[[name]] <- obj[[name]]
  }
  return(mud)
}


