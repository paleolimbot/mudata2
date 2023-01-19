
#' Rename identifiers in a mudata object
#'
#' These functions rename locations, datasets, params, and columns, making sure
#' internal consistency is maintained. These functions use dplyr syntax for renaming
#' (i.e. the [rename][dplyr::rename] function). This syntax can also be used while
#' subsetting using [select_locations] and family.
#'
#' @param .data A mudata object
#' @param ... Variables to rename in the form `new_var = old_var`
#'
#' @seealso [rename][dplyr::rename], [select_locations]
#'
#' @return A modified mudata object
#' @export
#' @rdname renamers
#'
#' @examples
#' rename_datasets(kentvillegreenwood, avalley = ecclimate)
#' rename_locations(kentvillegreenwood, Greenwood = starts_with("GREENWOOD"))
#' rename_params(kentvillegreenwood, max_temp = maxtemp)
#' rename_columns(kentvillegreenwood, lon = longitude, lat = latitude)
#'
rename_locations <- function(.data, ...) {
  UseMethod("rename_locations")
}

#' @rdname renamers
#' @export
rename_locations.default <- function(.data, ...) {
  # quo-ify locations
  locations <- quos(...)
  # use tidyselect to get location names
  locations <- .vars_rename(.tidyselect_vars(.data, "location"), "location", !!!locations)
  new_locations <- names(locations)

  # rename datasets using rename_locations_base
  if (any(new_locations != locations)) {
    renamer <- locations[new_locations != locations]
    rename_locations_base(.data, stats::setNames(names(renamer), renamer))
  } else {
    .data
  }
}

#' @rdname renamers
#' @export
rename_params <- function(.data, ...) {
  UseMethod("rename_params")
}

#' @export
#' @rdname renamers
rename_params.default <- function(.data, ...) {
  # quo-ify params
  params <- quos(...)
  # use tidyselect to get location names
  params <- .vars_rename(.tidyselect_vars(.data, "param"), "param", !!!params)
  new_params <- names(params)

  # rename datasets using rename_params_base
  if (any(new_params != params)) {
    renamer <- params[new_params != params]
    md_out <- rename_params_base(.data, stats::setNames(names(renamer), renamer))
  } else {
    .data
  }
}

#' @rdname renamers
#' @export
rename_datasets <- function(.data, ...) {
  UseMethod("rename_datasets")
}

#' @export
#' @rdname renamers
rename_datasets.default <- function(.data, ...) {
  # quo-ify datasets
  datasets <- quos(...)
  # use tidyselect to get dataset names
  datasets <- .vars_rename(.tidyselect_vars(.data, "dataset"), "dataset", !!!datasets)
  new_datasets <- names(datasets)

  # rename datasets using rename_dataset
  if (any(new_datasets != datasets)) {
    renamer <- datasets[new_datasets != datasets]
    rename_datasets_base(.data, stats::setNames(names(renamer), renamer))
  } else {
    .data
  }
}

#' @rdname renamers
#' @export
rename_columns <- function(.data, ...) {
  UseMethod("rename_columns")
}

#' @export
#' @rdname renamers
rename_columns.default <- function(.data, ...) {
  # quo-ify datasets
  columns <- quos(...)
  # use tidyselect to get dataset names
  columns <- .vars_rename(distinct_columns(.data), "column", !!!columns)
  new_columns <- names(columns)

  # rename datasets using rename_dataset
  if (any(new_columns != columns)) {
    renamer <- columns[new_columns != columns]
    # don't allow renaming of required columns
    if (any(renamer %in% c("dataset", "location", "param", "table", "column", "value"))) {
      abort("Cannot rename required mudata columns")
    }
    md_out <- rename_cols_base(.data, stats::setNames(names(renamer), renamer))
    # rename x_columns as well
    attr(md_out, "x_columns") <- rename_values_base(x_columns(md_out),
      stats::setNames(names(renamer), renamer),
      warn_missing = FALSE, warn_duplicated = TRUE
    )
    md_out
  } else {
    .data
  }
}

#' Rename a column in an object
#'
#' Rename columns in a data frame or list
#'
#' @param .data An object that has columns that can be renamed
#' @param ... Key/value pairs to replace in the form `oldval="newval"`
#' @param warn_missing Print a message if any old names are not actually present in x
#' @param warn_duplicated Print a message if any name appears more than once in x
#'   after the operation.
#'
#' @return A copy of the modified object
#' @keywords internal
#'
rename_cols_base <- function(.data, ..., warn_missing = TRUE, warn_duplicated = TRUE) UseMethod("rename_cols_base")

#' @export
#' @rdname rename_cols_base
rename_cols_base.default <- function(.data, ..., warn_missing = TRUE, warn_duplicated = TRUE) {
  names(.data) <- rename_values_base(names(.data), ...,
    warn_missing = warn_missing,
    warn_duplicated = warn_duplicated
  )
  .data
}


#' Replace/rename values in a vector
#'
#' This function replaces character values with new character values, which
#' is useful when performing rename operations when values are held in character vectors.
#'
#' @param x Vector of values to replace
#' @param ... Key/value pairs in the form `oldvalue="newvalue"`
#' @param default_value A vector of values to use as the default should the value not
#'   be found in `...`
#' @param warn_missing Print a message if any old names are not actually present in x
#' @param warn_duplicated Print a message if any name appears more than once in x
#'   after the operation.
#'
#' @return A vector with values replaced
#' @keywords internal
#'
rename_values_base <- function(x, ..., default_value = x, warn_missing = TRUE,
                               warn_duplicated = TRUE) {
  # if a factor, apply the rename operation to the levels
  if (is.factor(x)) {
    if (identical(default_value, x)) {
      default_value <- levels(x)
    }
    levels(x) <- rename_values_base(levels(x), ...,
      default_value = default_value,
      warn_missing = warn_missing, warn_duplicated = warn_duplicated
    )
    return(x)
  }

  replacer <- list(...)
  if (length(replacer) == 0) {
    return(x) # nocov
  }

  # can also pass a single rename vector like c(old_val = "new_val") as first arg
  replacernames <- names(replacer)
  if (is.null(replacernames) && (length(replacer) == 1)) {
    replacer <- unlist(replacer[[1]])
    replacernames <- names(replacer)
  } else {
    replacer <- unlist(replacer)
  }

  # check for missing values
  notinvector <- replacernames[!(replacernames %in% x)]
  if (warn_missing && (length(notinvector) > 0)) {
    message("Not all values were found: ", paste(notinvector, collapse = ", "))
  }

  # check for duplicated values
  if (any(replacer %in% x)) {
    message("Possible duplicated values in x: ", paste(replacer[replacer %in% x], collapse = ", "))
  }

  # make sure default value is the same length as x
  if (length(default_value) != length(x)) {
    default_value <- rep_len(default_value, length.out = length(x))
  }

  # use names to replace values
  new_x <- unname(replacer[x])
  # NA values were not in the replacer, use default value
  new_x[is.na(new_x)] <- default_value[is.na(new_x)]

  # return new x
  new_x
}

#' Rename datasets, params, locations, and columns
#'
#' Provides a convenient way to rename datasets, params, locations, and columns
#' such that their usage with a mudata object remains consistent.
#'
#' @param md A [mudata] object
#' @param .data A [mudata] object
#' @param ... Key/value pairs in the form `"oldvalue"="newvalue"`
#' @param apply_to The tables which the rename operation should consider
#' @param warn_missing Print a message if any old names are not actually present
#' @param warn_duplicated Print a message if any name appears more than once in x
#'   after the operation.
#'
#' @return A modified [mudata] object.
#' @keywords internal
#'
rename_datasets_base <- function(md, ..., apply_to = c("data", "locations", "params", "datasets", "columns"),
                                 warn_missing = TRUE) {
  for (dfname in apply_to) {
    md[[dfname]]$dataset <- rename_values_base(md[[dfname]]$dataset, ..., warn_missing = warn_missing)
  }
  return(md)
}

#' @rdname rename_datasets_base
rename_params_base <- function(md, ..., apply_to = c("data", "params"), warn_missing = TRUE) {
  for (dfname in apply_to) {
    md[[dfname]]$param <- rename_values_base(md[[dfname]]$param, ..., warn_missing = warn_missing)
  }
  return(md)
}

#' @rdname rename_datasets_base
rename_locations_base <- function(md, ..., apply_to = c("data", "locations"), warn_missing = TRUE) {
  for (dfname in apply_to) {
    md[[dfname]]$location <- rename_values_base(md[[dfname]]$location, ..., warn_missing = warn_missing)
  }
  return(md)
}

#' @rdname rename_datasets_base
rename_cols_base.mudata <- function(.data, ..., apply_to = c("datasets", "locations", "params", "data", "columns"),
                                    warn_missing = FALSE, warn_duplicated = TRUE) {
  for (dfname in apply_to) {
    .data[[dfname]] <- rename_cols_base(.data[[dfname]], ...,
      warn_missing = warn_missing,
      warn_duplicated = TRUE
    )
  }
  .data$columns$column <- rename_values_base(.data$columns$column, ..., warn_missing = warn_missing)
  return(.data)
}
