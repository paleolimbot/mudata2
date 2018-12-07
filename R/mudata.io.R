

#' Read/Write mudata objects
#' 
#' These functions will read and write mudata objects to disk using a directory 
#' (which contains one .csv file for each table in the object), a ZIP archive 
#' (which is a zipped version of the directory format), or a JSON file. The
#' base read/write functions attempt to guess which of these types to use
#' based on the file extension: use the specific read/write function to
#' avoid this.
#' 
#' These functions are designed to make sure that the read/write operations are
#' as lossless as possible. Some exceptions to this are if date/time columns are
#' not in UTC (in which case they will be converted to UTC before writing), and
#' if table names have characters that are not filesystem safe (allowed
#' characters are [A-Za-z0-9_.-] and others will be stripped).
#' 
#' @param md A mudata object
#' @param filename File to read/write (can also be a directory)
#' @param overwrite Pass \code{TRUE} to overwrite if the file/directory already
#'   exists.
#' @param validate Flag to validate mudata object after read or before write
#' @param update_columns Update the columns table "type" column to reflect the
#'   internal R types of columns (reccommended).
#' @param pretty Produce pretty or minified JSON output
#' @param txt JSON text from which to read a mudata object.
#' @param ... Passed to read/write functions
#'   
#' @export
#' 
#' @examples 
#' # read/write to directory
#' outfile <- tempfile(fileext=".mudata")
#' write_mudata(kentvillegreenwood, outfile)
#' md <- read_mudata(outfile)
#' unlink(outfile)
#' 
#' # read/write to zip
#' outfile <- tempfile(fileext=".zip")
#' write_mudata(kentvillegreenwood, outfile)
#' md <- read_mudata(outfile)
#' unlink(outfile)
#' 
#' # read/write to JSON
#' outfile <- tempfile(fileext=".json")
#' write_mudata(kentvillegreenwood, outfile)
#' md <- read_mudata(outfile)
#' unlink(outfile)
#' 
write_mudata <- function(md, filename, ...) {
  if(grepl("[.]zip$", filename)) {
    write_mudata_zip(md, filename, ...)
  } else if(grepl("[.]json$", filename)) {
    write_mudata_json(md, filename, ...)
  } else {
    message("Using write_mudata_dir() to write to ", filename)
    write_mudata_dir(md, filename, ...)
  }
}

#' @rdname write_mudata
#' @export
read_mudata <- function(filename, ...) {
  if(grepl("[.]zip$", filename)) {
    read_mudata_zip(filename, ...)
  } else if(grepl("[.]json$", filename)) {
    read_mudata_json(filename, ...)
  } else if(dir.exists(filename)) {
    read_mudata_dir(filename, ...)
  } else {
    stop("Don't know which format to read file '", filename, "'")
  }
}


#' @rdname write_mudata
#' @export
write_mudata_zip <- function(md, filename, overwrite = FALSE, validate = TRUE,
                             update_columns = TRUE, ...) {
  # check if output file exists, stop if overwrite = FALSE
  if(file.exists(filename) && !overwrite) stop("File ", filename, " exists. Use ovewrite = TRUE to overwrite.")
  
  # create a temporary directory, use write_mudata_dir to write to it
  dir_file <- tempfile()[1]
  on.exit(unlink(dir_file, recursive = TRUE))
  write_mudata_dir(md, dir_file, overwrite = TRUE, validate = validate, update_columns = update_columns, ...)
  
  # the zip function is tricky in that it requires a working directory change
  filename <- fs::path_abs(filename)
  status <- withr::with_dir(dir_file, utils::zip(filename, list.files(), flags = "-q"))
  if(status != 0) stop(sprintf("Writing zip file '%s' exited with status %s", filename, status))
  
  # return md, invisibly
  invisible(md)
}

#' @rdname write_mudata
#' @export
read_mudata_zip <- function(filename, validate = TRUE, ...) {
  # check that file exists
  if(!file.exists(filename)) stop("File ", filename, " does not exist")
  if(dir.exists(filename)) stop(filename, " is a directory")
  
  # create temporary directory, make sure is cleaned up on exit
  dir_file <- tempfile()[1]
  dir.create(dir_file)
  on.exit(unlink(dir_file, recursive = TRUE))
  
  # unzip zipfile to temporary directory
  utils::unzip(filename, exdir = dir_file)
  
  # use read_mudata_dir on the temporary directory
  read_mudata_dir(dir_file, validate = validate, ...)
}

#' @rdname write_mudata
#' @export
write_mudata_dir <- function(md, filename, overwrite = FALSE, validate = TRUE,
                             update_columns = TRUE, ...) {
  # check that filename isn't a file
  if(file.exists(filename) && !dir.exists(filename)) {
    stop("Not a directory: ", filename)
  }
  
  # check that dir doesn't already exist
  if(file.exists(filename) && !overwrite) stop("Directory ", filename, 
                                               " exists. Use ovewrite = TRUE to overwrite.")
  
  # prepare using mudata_write_common
  md_write <- write_mudata_common(md, validate = validate, 
                                  update_columns = update_columns, format = "csv")
  
  # create output directory
  dir.create(filename, showWarnings = FALSE)
  # check that output directory was created
  if(!dir.exists(filename)) stop("Failed to create directory: ", filename)
  
  # treat attributes like a tbl
  md_write[["_mudata"]] <- tibble::tibble(
    x_columns = jsonlite::toJSON(attr(md, "x_columns")),
    mudata_version = paste(c("R", unlist(utils::packageVersion("mudata2")[[1]])),
                           collapse = ".")
  )
  
  # safe lapply on md_write to write_csv
  result <- lapply(names(md_write), function(tbl_name) {
    fname <- file.path(filename, paste0(gsub("[^A-Za-z0-9_.-]+", "_", tbl_name), ".csv"))
    try(readr::write_csv(md_write[[tbl_name]], fname, na = ""))
  })
  
  # check that write succeeded
  errors <- vapply(result, inherits, "try-error", FUN.VALUE = logical(1))
  if(any(errors)) {
    error_text <- vapply(result[errors], as.character, character(1))
    stop("Error writing mudata to CSV:\n", 
         paste(sprintf(" %s.csv: %s", names(md_write)[errors], error_text),
               collapse = "\n"))
  } else {
    # return the input object, invisibly
    invisible(md)
  }
}

#' @rdname write_mudata
#' @export
read_mudata_dir <- function(filename, validate = TRUE, ...) {
  # check that filename is a directory
  if(!dir.exists(filename)) stop(filename, " does not exist or is not a directory")
  # look for data.csv within filename
  data_csv <- list.files(filename, pattern = "^data\\.csv$", recursive = TRUE,
                         full.names = TRUE)[1]
  if(is.na(data_csv)) stop("data.csv not found within ", filename)
  # use dirname(data_csv) as base directory
  mudir <- dirname(data_csv)
  # warn user if this is a different directory than filename
  # (usually occurs when somebody manually extracts a .zip file)
  if(gsub("[/\\\\]$", "", mudir) != gsub("[/\\\\]$", "", filename)) {
    message("Reading from ", mudir)
  }
  
  # find columns.csv, read it in
  columns_csv <- file.path(mudir, "columns.csv")
  if(file.exists(columns_csv)) {
    # try to generate type_str_tbl
    columns_tbl <- try(readr::read_csv(columns_csv, 
                                        col_names = TRUE,
                                        col_types = readr::cols(.default = readr::col_character())),
                        silent = TRUE)
    type_strs <- type_strs_from_columns(columns_tbl)
  } else {
    type_strs <- list()
  }
  
  # iterate over csv files in mudir, reading as character
  csv_files <- list.files(mudir, pattern = "\\.csv$", full.names = TRUE)
  table_names <- gsub("\\.csv$", "", basename(csv_files))
  obj <- lapply(stats::setNames(csv_files, table_names), function(csv_name) {
    readr::read_csv(csv_name, col_names = TRUE, col_types = readr::cols(.default = readr::col_character()), ...)
  })
                
  
  # remove metadata, extract x_columns
  meta <- obj[["_mudata"]]
  obj[["_mudata"]] <- NULL
  
  # retreive x_columns
  x_columns <- NULL
  if(!is.null(meta) && ("x_columns" %in% colnames(meta))) {
    x_cols_json <- try(jsonlite::fromJSON(meta$x_columns, simplifyVector = FALSE))
    if(is.vector(x_cols_json)) {
      x_columns <- as.character(x_cols_json)
    }
  }
  
  # recreate metadata list
  metadata <- list(x_columns = x_columns)
  
  # mudata tbls are all objects in obj
  mudata_tbls <- names(obj)
  
  # use read_common to apply retyping and create mudata object
  read_common(obj, mudata_tbls, metadata, type_strs, validate = validate)
}

#' @rdname write_mudata
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

#' @rdname write_mudata
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
  md_write[["_mudata"]] <- list(x_columns = x_columns(md),
                                mudata_version = paste(c("R", unlist(utils::packageVersion("mudata2")[[1]])),
                                                       collapse = "."))
  
  # writing is simple, it is just a JSON object of the mudata list with some
  # pre-set parameters
  fun(md_write, ..., dataframe = "columns", na = "null",
      digits = NA, POSIXt = "ISO8601", factor = "string",
      Date = "ISO8601", auto_unbox = TRUE, pretty = pretty)
}

#' @rdname write_mudata
#' @export
read_mudata_json <- function(filename, validate = TRUE, ...) {
  read_mudata_json_common(jsonlite::read_json, path = filename, validate = validate, ...)
}

#' @rdname write_mudata
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
  
  # get column type information
  if(("columns" %in% names(obj)) && 
     all(c("table", "column", "type") %in% names(obj$columns))) {
    # generate small version of column type table and validate it
    columns_tbl <- tibble::tibble(table = obj$columns$table,
                                  column = obj$columns$column, 
                                  type = obj$columns$type)
    
    # use type_strs from generate_type_strs
    type_strs <- type_strs_from_columns(columns_tbl)
  } else {
    # without the columns table, parsers can't be reliably guessed when
    # reading from JSON (because NAs are 'null's, which read as list()s)
    stop("cannot read JSON to mudata without a columns table")
  }
  
  # extract and remove metadata, if present
  metadata <- obj[["_mudata"]]
  obj[["_mudata"]] <- NULL
  
  # define mudata_tbls to mean all list-like objects in obj
  # these should all be tbls with the same logic from columns applied
  mudata_tbls <- names(obj)[vapply(obj, is.list, logical(1))]
  
  # use read_common to apply retyping and create mudata object
  read_common(obj, mudata_tbls, metadata, type_strs, validate = validate)
}


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

# common function to transform tbls, pass to mudata constructor
read_common <- function(obj, mudata_tbls, meta_list, type_strs, validate) {
  
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
  
  # only using x_columns from meta_tbl (both may be NULL)
  x_columns <- meta_list$x_columns
  
  # pass to mudata
  md_obj <- mudata(data = md$data, locations = md$locations, params = md$params, 
                   datasets = md$datasets, columns = md$columns, x_columns = x_columns,
                   more_tbls = md[setdiff(names(md), c("data", "locations", "params", "datasets", "columns"))],
                   validate = validate)
  md_obj
}

# common function to read a columns table (type_str_tbl)
type_strs_from_columns <- function(columns_tbl) {
  if(is.null(columns_tbl)) return(list())
  if(!all(c("table", "column", "type") %in% colnames(columns_tbl))) {
    # no types specified, quietly return list()
    return(list())
  }
  
  # get distinct table, column, type info
  type_str_tbl <- columns_tbl %>%
    dplyr::select("table", "column", "type") %>%
    dplyr::distinct()
  
  # types need to be unique to be read by this function
  is_unique <- try(.checkunique(type_str_tbl, 'columns', c("table", "column")), silent = TRUE)
  if(inherits(is_unique, "try-error")) {
    # duplicate values = slightly malformed columns table. return list() with a message
    message("Possibly malformed columns table: different data types for at least one column among datasets.")
    return(list())
  }
  
  # create list of type_str named lists
  type_str_tbl <- type_str_tbl %>%
    tidyr::nest(c("column", "type"), .key = "types")
  type_strs <- type_str_tbl$types %>%
    lapply(tibble::deframe) %>%
    stats::setNames(type_str_tbl$table)
  type_strs
}

#' Update the columns table
#'
#' @param md A mudata object
#' @param quiet Suppress changes to existing types
#'
#' @return A mudata object
#' @export
#' 
update_columns_table <- function(md, quiet = FALSE) {
  
  # generate columns table from original mudata, join to existing cols table
  generated_cols <- generate_type_tbl(md)
  
  # join with columns to see if any types need to be updated
  generated_cols_joined <- md$columns %>%
    dplyr::left_join(generated_cols, by = c("dataset", "table", "column"),
                     prefix = c(".x", ".y"))
  
  # check that type.x and type.y are the same, if type was already in the columns table
  if("type" %in% colnames(md$columns)) {
    type.x <- NULL; rm(type.x); type.y <- NULL; rm(type.y) # CMD hack
    replaced_types <- generated_cols_joined %>% dplyr::filter(type.x != type.y)
    if((nrow(replaced_types) > 0) && !quiet) {
      message(sprintf("Replacing types %s with %s",
                      with(replaced_types, paste(dataset, table, column, type.y, sep = "/")),
                      with(replaced_types, paste(dataset, table, column, type.x, sep = "/"))))
      
    }
    
    # overwrite the type column in md$columns, add new column data
    md$columns <- generated_cols %>%
      dplyr::left_join(md$columns, by = c("dataset", "table", "column"),
                       prefix = c(".x", ".y")) %>%
      dplyr::mutate(type = type.x) %>%
      dplyr::select(-type.y, -type.x)
  } else {
    # overwrite the md columns table (in case new columns were added)
    md$columns <- generated_cols %>%
      dplyr::left_join(md$columns, by = c("dataset", "table", "column"))
  }
  
  # remove the columns table metadata, make sure output is a tibble
  md$columns <- md$columns %>%
    dplyr::filter(table != "columns") %>%
    tibble::as_tibble()
  
  # return md
  md
}

#' Prepare mudata table columns for writing
#' 
#' This set of generics is similar to \link[readr]{output_column} in that it
#' converts columns to a form suitable to writing.
#' \code{mudata_prepare_column} in combination with is 
#' intended to be opposites with \code{mudata_parse_column} except for 
#' date/time vectors that are not in UTC (mudata_parse_column assumes UTC,
#' and mudata_prepare_column always converts to UTC with a message).
#' 
#' Type strings are currently internal, and are in the columns table in the
#' "type" column. They are usually one of "character", "date", "datetime", "double",
#' "integer", "json", and "wkt". They can also contain simple arguments, like
#' "wkt(epsg=4326)" (actually, "wkt" is the only type string that should have arguments).
#' You should generally not mess with these (in fact, the "type" column in the coluns 
#' table is overwritten right before read by default, so it is hard to mess this up).
#'
#' @param x A an object
#' @param format csv, json, or NA for unknown, 
#' @param type_str A type string, generated by the internal \code{generate_type_str}
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
  if(!identical(tzone, "UTC")) {
    message("Converting POSIXt column to UTC")
  }
  strftime(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC", ...)
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
  # NA_character means "guess"
  if(inherits(x, parse_output_class(type_str))) {
    x
  } else {
    as_parser(type_str)(as.character(x), ...)
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
