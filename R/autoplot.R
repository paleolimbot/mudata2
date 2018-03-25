
#' Smart plotting of parameter-long data frames
#' 
#' These functions are intended to quickly visualize plot a parameter-long
#' data frame with a few variables that identify single rows in the value column. The function
#' is optimised to plot data with a time axis data either horizontally
#' (time on the x axis) or vertically (time on the y axis). Facets are intended
#' to be by parameter, which is guessed based on the right-most discrete variable named
#' in id_vars. In the context of a \link{mudata} object, this function almost always
#' guesses the axes correctly, but these choices can be overridden.
#'
#' @param .data A \code{data.frame}
#' @param id_vars Columns that identify unique rows
#' @param measure_var Column that contains values to be plotted
#' @param x Column to be used on the x-axis
#' @param y Column to be used on the y-axis
#' @param facets Column(s) to be used as facetting variable (using \link[ggplot2]{facet_wrap})
#' @param geom Can be any combination of point, path, or line.
#' @param error_var The column to be used for plus/minus error bars
#' @param facet_args Passed on to \link[ggplot2]{facet_wrap}
#' @param max_facets Constrain the maximum number of facets, where available
#' @param scales Customize aesthetic mapping in long_plot()
#' @param ... Additional aesthetic mappings, passed on to 
#'   (or treated similarly to) \link[ggplot2]{aes_string}
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' library(dplyr)
#' pocmaj_long <- pocmajsum %>%
#'   select(core, depth, Ca, Ti, V) %>%
#'   gather(Ca, Ti, V, key = "variable", value = "value")
#' long_plot(pocmaj_long, col="core")
#' long_ggplot(pocmaj_long, col="core")
#'
long_ggplot <- function(.data, ..., max_facets = 9, facet_args = list()) {
  # use long_plot_base to do the guessing
  args <- long_plot_base(.data, ...)
  
  # deal with geometries
  geoms <- list(path=ggplot2::geom_path(), line=ggplot2::geom_line(), 
                point=ggplot2::geom_point())
  unrecognized_geoms <- setdiff(args$geom, names(geoms))
  if(length(unrecognized_geoms) > 0) stop("Unrecognized geometries: ", 
                                         paste(unrecognized_geoms, collapse=" ,"))
  plot_geoms <- geoms[args$geom]
  
  # make a mapping with x, y, and more_args
  plot_mapping <- do.call(ggplot2::aes_string, 
                          c(args$mapping[c("x", "y")], args$mapping$more_args))
  
  # create facets 
  if(is.null(args$mapping$facets)) {
    plot_facet <- do.call(ggplot2::facet_null, facet_args)
  } else {
    # specify facet scales as free in the value direction if unspecified
    if(!("scales" %in% facet_args)) {
      if(args$mapping$y == args$mapping$measure_var) {
        facet_args$scales <- "free_y"
      } else if(args$mapping$x == args$mapping$measure_var) {
        facet_args$scales <- "free_x"
      }
    }
    
    # create the facet argument as a formula if unspecified
    # (this lets the user correct a sub-optimally guessed facet spec)
    if(!("facets" %in% facet_args)) {
      facet_args$facets <- stats::as.formula(sprintf("~%s", paste(args$mapping$facets, 
                                                                  collapse = "+")))
    }
    
    # filter such that there are only max_facets number of facets
    facet_df <- create_facet_df(.data, args$mapping$facets, max_facets)
    # create data by collecting and left_joining facet info
    # CMD hack
    .facet_number <- NULL; rm(.facet_number)
    .data <- .data %>%
      dplyr::ungroup() %>%
      dplyr::collect() %>%
      dplyr::left_join(facet_df, by = args$mapping$facets) %>%
      dplyr::filter(!is.na(.facet_number))
    
    plot_facet <- do.call(ggplot2::facet_wrap, facet_args)
  }
  
  # create error bars if error_var is specified
  if(!is.null(args$mapping$error_var)) {
    # guess axis for error bars based on mapping and measure_var
    # aesthetic for error bars is measure_var+/-error_var
    if(args$mapping$y == args$mapping$measure_var) {
      error_bar_geom <- ggplot2::geom_errorbar
      error_bar_mapping <- ggplot2::aes_string(
        ymin = sprintf("%s-%s", args$mapping$measure_var, args$mapping$error_var),
        ymax = sprintf("%s+%s", args$mapping$measure_var, args$mapping$error_var),
        width = sprintf("diff(range(%s, na.rm = TRUE)) / 30", args$mapping$x)
      )
    } else if(args$mapping$x == args$mapping$measure_var) {
      error_bar_geom <- ggplot2::geom_errorbarh
      error_bar_mapping <- ggplot2::aes_string(
        xmin = sprintf("%s-%s", args$mapping$measure_var, args$mapping$error_var),
        xmax = sprintf("%s+%s", args$mapping$measure_var, args$mapping$error_var),
        height = sprintf("diff(range(%s, na.rm = TRUE)) / 30", args$mapping$y)
      )
    } else {
      stop("Cannot guess which direction to place error bars")
    }
    
    # create layer
    plot_error <- error_bar_geom(mapping = error_bar_mapping)
  } else {
    plot_error <- NULL
  }
  
  # collect data
  .data <- dplyr::collect(.data)
  
  # return plot
  ggplot2::ggplot(data = .data, mapping = plot_mapping, environment = parent.frame()) +
    plot_error + plot_geoms + plot_facet
}

#' @rdname long_ggplot
#' @export
long_plot <- function(.data, id_vars = NULL, measure_var = "value", x = NULL, y = NULL, 
                      facets = NULL, geom = "path", error_var = NULL, ...,
                      max_facets = 9, facet_args = list(), scales = list()) {
  # CMD plot hack
  n <- NULL; rm(n); . <- NULL; rm(.); .facet_number <- NULL; rm(.facet_number)
  
  # extra args passed on to the plot function, unless they are pch, col, or lty
  aesthetic_names <- c("pch", "shape", "col", "colour", "color", "lty", "linetype")
  extra_args <- list(...)
  plot_args <- extra_args[setdiff(names(extra_args), aesthetic_names)]
  long_plot_args <- extra_args[intersect(names(extra_args), aesthetic_names)]
  
  # use long_plot_base to do the guessing (ignore error_var)
  args <- do.call(
    long_plot_base,
    c(list(.data, id_vars = id_vars, measure_var = measure_var,
            x = x, y = y, facets = facets, geom = geom),
            long_plot_args))
  
  # use more_args as the aesthetic mapping
  aesthetic_mapping <- args$mapping$more_args
  # remove NULLs from aestheic mapping
  null_cols <- vapply(aesthetic_mapping, is.null, logical(1))
  aesthetic_mapping <- aesthetic_mapping[!null_cols]
  
  position_mapping <- args$mapping[c("x", "y")]
  
  # convert geoms to plot.default "type" parameter
  geom <- sort(args$geom)
  if(identical(geom, "point")) {
    plot_type <- "p"
  } else if(identical(geom, c("line", "point"))) {
    plot_type <- "b"
  } else if(identical(geom, c("path", "point"))) {
    plot_type <- "b"
  } else if(identical(geom, "line")) {
    plot_type <- "l"
  } else if(identical(geom, "path")) {
    plot_type <- "l"
  } else if(length(geom) > 1 || !is.character(geom)) {
    stop("geom must be a character vector of length 1")
  } else {
    plot_type <- geom
  }
  
  # train default scales, if any
  default_scales <- lapply(aesthetic_mapping[!(names(aesthetic_mapping) %in% names(scales))],
                           function(col_name) {
                             .data %>%
                               dplyr::ungroup() %>%
                               dplyr::select(!!col_name) %>%
                               dplyr::distinct() %>%
                               dplyr::collect() %>%
                               dplyr::mutate(count = 1:n()) %>%
                               tibble::deframe()
                           })
  scales <- c(scales, default_scales)
  
  if(!is.null(args$mapping$facets)) {
    # figure out facet info
    facet_df <- create_facet_df(.data, args$mapping$facets, max_facets)
    
    # assign facet labels by pasting each non-facet_number column together
    facet_df$.facet_label <- vapply(seq_len(nrow(facet_df)), function(i) {
      row <- facet_df[i, ]
      row$.facet_number <- NULL
      paste(unlist(row), collapse = " ")
    }, character(1))
    
    if(nrow(facet_df) == 0) {
      stop("Zero facets were found; no data to plot")
    } else if(nrow(facet_df) > 1) { 
      # if there is more than one facet, use mfrow to "facet" the plot
      # get number of rows and cols for mfrow
      if(is.null(facet_args$ncol)) {
        facet_args$ncol <- 3
      }
      ncol <- facet_args$ncol
      nrow <- ceiling(nrow(facet_df) / ncol)
      
      # set ane ensure defaults are reset on exit
      old_par <- graphics::par(mfrow = c(nrow, ncol),
                               oma = c(5,4,2,2) + 0.1,
                               mar = c(2,2,1,1) + 0.1)
      on.exit(graphics::par(old_par))
    }
    
    # create data by collecting and left_joining facet info
    .data <- .data %>%
      dplyr::ungroup() %>%
      dplyr::collect() %>%
      dplyr::left_join(facet_df, by = args$mapping$facets) %>%
      dplyr::filter(!is.na(.facet_number))
  } else {
    # collect and make dummy facet number/label columns
    facet_df <- .data %>%
      dplyr::ungroup() %>%
      dplyr::collect() %>%
      dplyr::mutate(.facet_number = NA_real_, .facet_label = NA_character_)
  }
  
  # add some defaults to the plot args
  plot_args <- c(
    list(xlab = NA, ylab = NA, type = plot_type),
    plot_args
  )
  
  # group by facet number, label and start plotting
  .data %>%
    dplyr::group_by(.facet_number, .facet_label) %>%
    dplyr::do({
      .facet_label <- unique(.$.facet_label)
      base_mapped_plot(., c(position_mapping, aesthetic_mapping), scales, 
                       c(list(main = .facet_label), plot_args))
      data.frame()
    })
  
  # return debugging information, invisibly
  invisible(list(args = args, facet_df = facet_df, scales = scales))
}

base_mapped_plot <- function(.data, mapping, scales, plot_args) {
  
  # make plot() args based on mapping
  args <- lapply(names(mapping), function(aes_name) {
    col_data <- .data[[mapping[[aes_name]]]]
    if(aes_name %in% names(scales)) {
      col_data <- stats::setNames(scales[[aes_name]][col_data], NULL)
    } else {
      col_data
    }
  })
  
  # set names to args from mapping
  args <- stats::setNames(args, names(mapping))
  
  # add extra plot args, extract plot type
  plot_type <- plot_args$type
  plot_args$type <- NULL
  
  # create plot args for the first call to plot()
  # check for all(is.na()) for x and y
  if(all(is.na(args$x))) {
    range_x <- c(0, 0)
  } else {
    range_x <- range(args$x, na.rm = TRUE)
  }
  
  if(all(is.na(args$y))) {
    range_y <- c(0, 0)
  } else {
    range_y <- range(args$y, na.rm = TRUE)
  }
  
  plot_args <- c(list(
    x = range_x,
    y = range_y, type = "n"
  ), plot_args)
  
  # setup the plot ranges/axes by plotting the x and y ranges without points
  do.call(graphics::plot, plot_args)
  
  # allow plot type = "n"
  if(plot_type == "n") return()
  
  # each non-xy aesthetic, add the "series"
  non_position_args <- setdiff(names(args), c("x", "y"))
  if(length(non_position_args) == 0) {
    splitter <- rep(1, length(args$x))
  } else {
    splitter <- do.call(paste, args[non_position_args])
  }
  lapply(split(as.data.frame(args), splitter),
         function(df) {
           series_args <- c(as.list(df[c("x", "y")]),
                            as.list(dplyr::distinct(df[non_position_args])))
           if(plot_type %in% c("b", "l")) {
             # plot lines
             line_args <- series_args
             line_args$pch <- NULL
             do.call(graphics::lines, line_args)
           }
           if(plot_type != "l") {
             # plot points
             series_args$lty <- NULL
             do.call(graphics::points, series_args)
           }
         })
}

#' @rdname long_ggplot
#' @export
long_plot_base <- function(.data, id_vars = NULL, measure_var = "value", x = NULL, y = NULL, 
                           facets = NULL, geom = "path", error_var = NULL, ...) {
  
  # check for measure_var (and proper .data type)
  .checkcols(.data, '.data', measure_var)
  
  # check for empty .data
  if(.isempty(.data)) stop(".data contains no data")
  
  # guess or check id_vars if needed
  if(is.null(id_vars)) {
    id_vars <- guess_id_vars(colnames(.data), measure_var)
  } else {
    .checkcols(.data, '.data', id_vars)
  }
  
  # use head(.data) instead of full .data object to check id_var types
  .data_head <- .data %>% utils::head() %>% dplyr::collect()
  
  # find which id_vars are numeric
  numeric_id_vars <- intersect(id_vars, colnames(.data)[vapply(.data_head, is.numericish, logical(1))])
  non_numeric_id_vars <- setdiff(id_vars, numeric_id_vars)
  
  # guess or check x and y columns as needed
  xy_guess <- guess_xy(.data, x, y, numeric_id_vars, measure_var)
  x <- xy_guess$x
  y <- xy_guess$y
  
  # make sure x and y are in .data
  .checkcols(.data, '.data', c(x, y))
  
  # check error_var as needed
  if(!is.null(error_var)) {
    .checkcols(.data, '.data', error_var)
  }
  
  # facets are specified as formulas in ggplot2::qplot, but here they need to be
  # strings so that they can be used in either long_ggplot or long_plot
  if(inherits(facets, "formula")) {
    facets <- all.vars(facets)
  }
  
  # check facets as needed
  if(!is.null(facets)) {
    .checkcols(.data, '.data', facets)
  }
  
  # evaluate other aesthetics that should be mapped from id_vars,
  # but not if they are already mapped from ...
  more_args <- list(...)
  
  # check additional mapping columns
  .checkcols(.data, '.data', unlist(more_args))
  
  # make small effort to rename aesthetics to base-r like names
  aes_renamer <- c("colour" = "col", "color" = "col", "shape" = "pch",
                   "linetype" = "lty")
  names(more_args) <- ifelse(names(more_args) %in% names(aes_renamer), 
                             aes_renamer[names(more_args)], names(more_args))
  # treating facets like a mapped value here
  unmapped_aes <- setdiff(c("col", "pch", "lty"), names(more_args))
  if(is.null(facets)) {
    unmapped_aes <- c("_facet", unmapped_aes)
  }
  unmapped_vars <- rev(setdiff(non_numeric_id_vars, 
                               c(x, y, facets, unlist(more_args, use.names = FALSE))))
  
  # here is just an attempt to match up the unmapped_aes to the unmapped_vars to the shortest
  # length of either
  additional_mapping_len <- min(length(unmapped_vars), length(unmapped_aes))
  additional_mapping <- stats::setNames(utils::head(unmapped_vars, additional_mapping_len),
                                 utils::head(unmapped_aes, additional_mapping_len))
  
  # turn additional_mapping into a list and extract _facets from it, since
  # it is not an aesthetic
  additional_mapping <- as.list(additional_mapping)
  if("_facet" %in% names(additional_mapping)) {
    facets <- additional_mapping[["_facet"]]
    additional_mapping[["_facet"]] <- NULL
    message('Using facets = c(',
            paste0('"', facets, '"', collapse = ", "),
            ')')
  }
  
  # warn about additional mappings
  if(length(additional_mapping) > 0) {
    message("Using ", paste(sprintf('%s = "%s"', names(additional_mapping), 
                                    unlist(additional_mapping)), 
                            collapse = ", "))
  }
  
  # add the additional_mapping to more_args
  more_args <- c(more_args, additional_mapping)
  
  # return the data and a string mapping that can be used in long_ggplot or long_plot
  list(.data = .data, geom = geom, mapping = list(
    x = x, y = y, error_var = error_var, facets = facets,
    measure_var = measure_var, more_args = more_args
  ))
}


create_facet_df <- function(.data, facets, max_facets) {
  
  # get all distinct combos of facet vars
  facet_df <- .data %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::one_of(facets)) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    # sort by all facetting variables
    dplyr::group_by_all() %>%
    dplyr::arrange(.by_group = TRUE) %>%
    dplyr::ungroup() %>%
    # create facet number variable
    dplyr::mutate(.facet_number = 1:n())
  
  # check number of facets, only plot first max_facets
  if(!identical(max_facets, FALSE) && (nrow(facet_df) > max_facets)) {
    message(sprintf("Using first %s facets of %s. ", max_facets, nrow(facet_df)),
            "Use max_facets = FALSE to plot all facets")
    facet_df <- utils::head(facet_df, max_facets)
  }
  
  facet_df
}

# guesses x and y based on the numeircness of .data
guess_xy <- function(.data, x, y, numeric_id_vars, measure_var) {
  # if x and y are both non-null, just use x and y with no guessing
  if(!is.null(x) && !is.null(y)) return(list(x = x, y = y))
  
  # guess x and y based on numeric-ness of x and y
  # can't guess x and y if neither are numeric
  if(length(numeric_id_vars) == 0) stop("Could not guess x and y axes from columns: ",
                                        paste0(colnames(.data), collapse = ", "))
  
  if(is.null(x) && is.null(y)) {
    # if neither is specified, y is the measure_var and 
    # x is the last numeric identifier
    x <- numeric_id_vars[length(numeric_id_vars)]
    y <- measure_var
    # message the guess
    message('Using x = "', x, '"', ', y = "', y, '"')
  } else if(is.null(x)) {
    # if only y is specified, use the last numeric identifier for x or
    # the measure_var column (this allows specifying a sideways plot like
    # for downcore plots)
    if(y == measure_var) {
      x <- numeric_id_vars[length(numeric_id_vars)]
    } else {
      x <- measure_var
    }
    # message the guess
    message('Using x = "', x, '"')
  } else if(is.null(y)) {
    # if only x is specified, use the last numeric identifier for x or
    # the measure_var column (this allows specifying a sideways plot like
    # for downcore plots)
    if(x == measure_var) {
      y <- numeric_id_vars[length(numeric_id_vars)]
    } else {
      y <- measure_var
    }
    # message the guess
    message('Using y = "', y, '"')
  }
  
  # return x and y as a list
  list(x = x, y = y)
}

guess_id_vars <- function(vars, measure_var) {
  # guess is all variables to the left of measure_var
  measure_var_loc <- which(vars == measure_var)
  id_vars <- setdiff(vars[1:measure_var_loc], measure_var)
  if(length(id_vars) == 0) stop("id_vars could not be guessed from columns: ",
                                paste0('"', vars, '"', collapse = ", "))
  # warn about guessing
  message("Using id_vars = c(",
          paste0('"', id_vars, '"', collapse = ", "),
          ")")
  
  # return id_vars
  id_vars
}

#' Autoplot a mudata object
#' 
#' Produces a quick graphical summary of a mudata object. The \code{autoplot()}
#' function is based on ggplot2's \link[ggplot2]{qplot}, and is the preferred
#' (and most flexible) plotting method. The \code{plot()} function uses
#' base R graphics and produces quick summary plot, but is unlikely to be
#' useful in any other context. Note that all column names must be quoted
#' (i.e., \code{aesthetic = "col_name"} not \code{aesthetic = col_name}).
#'
#' @param x,object A \link{mudata} object
#' @param facets Column to be used as facet column
#' @param col Column to be used as colour aesthetic
#' @param pch Column to be used as shape aesthetic
#' @param ... Passed on to \link{long_plot} or \link{long_ggplot}
#'
#' @export
#' 
#' @seealso \link{long_ggplot}
#' 
#' @examples 
#' # plot using base plot
#' plot(kentvillegreenwood)
#' 
#' # a more informative plot using ggplot
#' autoplot(kentvillegreenwood)
#' 
#' @importFrom ggplot2 autoplot
#'
autoplot.mudata <- function(object, facets = "param", col = "location", pch = "dataset", ...) {
  long_ggplot(object$data, id_vars=c("dataset", "location", "param", x_columns(object)), 
              facets = facets, col = col, pch = pch,
              measure_var="value", ...)
}

#' @rdname autoplot.mudata
#' @importFrom graphics plot
#' @export
plot.mudata <- function(x, facets = "param", col = "location", pch = "dataset", ...) {
  long_plot(x$data, id_vars=c("dataset", "location", "param", x_columns(x)), 
            facets = facets, col = col, pch = pch,
            measure_var="value", ...)
}

# internal function to define 'numeric' (ish) classes
# these functions can have numeric functions applied to them, specifically,
# such as min(), max(), and mean(). Is essentially the opposite of ggplot2's
# is.discrete(), which may be an alternative.
is.numericish <- function(x) {
  return(any(class(x) %in% c("numeric", "integer", "Date", "POSIXct", "POSIXt", "difftime")))
}

