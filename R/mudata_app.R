
#' Interactive Plot of a MUData object using Shiny
#'
#' @param obj A mudata object
#' @param obj_name The mudata object name to use in code generation
#' @param save_state Should changes to the input for this object be saved?
#' @param load_state Should the previous input values for this boject be loaded?
#'
#' @export
#'
#' @examples
#' if(interactive()) {
#'   mudata_app()
#' }
#' 
mudata_app <- function(obj = NULL, obj_name = NULL, save_state = TRUE, load_state = TRUE) {
  # get the object name
  if(is.null(obj_name)) {
    mudata_shiny_object_name <- deparse(substitute(obj))
  } else {
    mudata_shiny_object_name <- obj_name
  }
  
  # process inputs
  if(!is.null(obj)) {
    if(!inherits(obj, "mudata")) stop("obj is not a mudata object")
    mudata_shiny_object <- obj
  } else {
    mudata_shiny_object <- kentvillegreenwood
    mudata_shiny_object_name <- "kentvillegreenwood"
  }
  
  # make shortcut to mudata object
  md <- mudata_shiny_object
  
  # make list of data columns
  data_columns <- c(list("Auto" = "__NULL__"), 
                    stats::setNames(colnames(md$data), colnames(md$data)))
  
  # make list of datasets, locations, params
  # CMD hack
  dataset <- NULL; rm(dataset); location <- NULL; rm(location); param <- NULL; rm(param)
  datasets <- dplyr::distinct(md$datasets, dataset)$dataset
  locations <- dplyr::distinct(md$locations, location)$location
  params <- dplyr::distinct(md$params, param)$param
  
  # make digest of data columns, datasets, locations, params
  mdigest <- digest::digest(
    list(
      data_columns = data_columns,
      datasets = datasets,
      locations = locations,
      params = params
    )
  )
  
  # make save inputs function
  if(save_state) {
    mudata_shiny_save_inputs <- function(input) {
      message("Saving Inputs")
      mudata_app_options[[mdigest]] <- input
    }
  } else {
    mudata_shiny_save_inputs <- function(input) {
      NULL
    }
  }
  
  # make previous state loader
  if(load_state && mdigest %in% names(mudata_app_options)) {
    mudata_shiny_inputs <- mudata_app_options[[mdigest]]
  } else {
    mudata_shiny_inputs <- list()
  }
  
  # make function to get default values
  default_input_value <- function(id, default) {
    df <- mudata_shiny_inputs[[id]]
    if(!is.null(df)) {
      df
    } else {
      default
    }
  }
  
  # make UI
  ui <- shiny::shinyUI(shiny::fluidPage(
    
    # Application title
    shiny::titlePanel(sprintf("Interactive Plot: %s", mudata_shiny_object_name)),
    
    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::checkboxGroupInput("subset_datasets", "Datasets", 
                           choices = sort(datasets),
                           selected = default_input_value('subset_datasets',
                                                          NULL)),
        shiny::checkboxGroupInput("subset_locations", "Locations", 
                           choices = sort(locations),
                           selected = default_input_value('subset_locations',
                                                          NULL)),
        shiny::checkboxGroupInput("subset_params", "Params", 
                           choices = sort(params),
                           selected = default_input_value('subset_params',
                                                          NULL)),
        shiny::selectInput("plot_y", "Y Variable:", choices = data_columns,
                    selected = default_input_value('plot_y', "__NULL__")),
        shiny::checkboxInput("plot_reversed_y", "Reverse", 
                             value = default_input_value('plot_reversed_y', FALSE)),
        shiny::selectInput("plot_col", "Colour Variable:", choices = data_columns,
                    selected = default_input_value('plot_col', "__NULL__")),
        shiny::selectInput("plot_shape", "Shape Variable:", choices = data_columns,
                    selected = default_input_value('plot_shape', "__NULL__")),
        shiny::selectInput("plot_linetype", "Linetype Variable:", choices = data_columns,
                    selected = default_input_value('plot_linetype', "__NULL__")),
        shiny::selectInput("plot_facets", "Facet Variable:", choices = data_columns,
                    selected = default_input_value('plot_facets', "__NULL__")),
        shiny::selectInput("plot_errors", "Error Variable:", choices = data_columns,
                    selected = default_input_value('plot_errors', "__NULL__")),
        shiny::checkboxGroupInput("plot_geom", "Layers", 
                           choices = c("point", "line", "path"),
                           selected = default_input_value('plot_geom', 'path')),
        shiny::numericInput("plot_facet_cols", "Facet Columns", 
                     value = default_input_value('plot_facet_cols', 2), 
                     min = 1, step = 1)
      ),
      
      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::plotOutput("muplot", height = 600),
        shiny::verbatimTextOutput("mutext"),
        shiny::textOutput("input_saved")
      )
    )
  ))
  
  # server logic below
  
  # function to generate subset args based on input
  generate_subset_args <- function(input, output) {
    lst <- list(
      datasets = input$subset_datasets, 
      locations = input$subset_locations,
      params = input$subset_params
    )
    # remove NULLs
    lst[!vapply(lst, is.null, logical(1))]
  }
  
  # function to generate args based on input
  generate_plot_args <- function(input, output) {
    input <- lapply(shiny::reactiveValuesToList(input), function(x) {
      if(identical(x, "__NULL__")) {
        NULL
      } else {
        x
      }
    })
    
    plot_args <- list(
      y = input$plot_y,
      facets = input$plot_facets,
      error_var = input$plot_errors,
      geom = input$plot_geom,
      facet_args = list(ncol = input$plot_facet_cols)
    )
    
    if(!is.null(input$plot_col)) {
      plot_args$col <- input$plot_col
    }
    
    if(!is.null(input$plot_shape)) {
      plot_args$shape <- input$plot_shape
    }
    
    if(!is.null(input$plot_linetype)) {
      plot_args$linetype <- input$plot_linetype
    }
    
    # remove NULLs for specific args
    plot_args[!vapply(plot_args, is.null, logical(1))]
  }
  
  # generate additional ggplot accessories
  generate_ggplot_accessories <- function(input, output) {
    if(input$plot_reversed_y) {
      ggplot2::scale_y_reverse()
    } else {
      NULL
    }
  }
  
  # create server object
  server <- shiny::shinyServer(function(input, output) {
    
    output$input_saved <- shiny::reactive({
      mudata_shiny_save_inputs(shiny::reactiveValuesToList(input))
      "Input saved."
    })
    
    output$muplot <- shiny::renderPlot({
      # get args
      subset_args <- generate_subset_args(input, output)
      plot_args <- generate_plot_args(input, output)
      
      # subset mudata
      mdplot <- do.call(subset, c(list(md), subset_args))
      # plot mudata
      do.call(ggplot2::autoplot, c(list(mdplot), plot_args)) +
        generate_ggplot_accessories(input, output)
    })
    
    output$mutext <- shiny::renderText({
      # get args
      subset_args <- generate_subset_args(input, output)
      plot_args <- generate_plot_args(input, output)
      
      # textify each argument
      textify <- function(obj) {
        tfile <- tempfile()[1]
        on.exit(unlink(tfile))
        dput(obj, tfile, control = character(0))
        paste0(readLines(tfile), collapse = "\n")
      }
      
      subset_string_args <- vapply(subset_args, textify, character(1))
      plot_args <- vapply(plot_args, textify, character(1))
      
      calls <- c(
        mudata_shiny_object_name,
        sprintf("subset(%s)", 
                paste(names(subset_string_args), subset_string_args, sep = " = ",
                      collapse = ", ")),
        sprintf("autoplot(%s)", 
                paste(names(plot_args), plot_args, sep = " = ",
                      collapse = ", "))
      )
      
      # remove subset if there are no args
      if(length(subset_args) == 0) {
        calls <- calls[-2]
      }
      
      # tidy using formatR
      tidy <- formatR::tidy_source(text = paste(calls, collapse = " %>% "),
                                   output = FALSE, width.cutoff = 80,
                                   indent = 2)$text.tidy
      
      # add ggplot2 additions
      if(input$plot_reversed_y) {
        tidy <- paste0(tidy, " + \n  scale_y_reverse()")
      }
      
      # insert ggplot2, magrittr library call
      tidy <- paste0("library(ggplot2)\nlibrary(magrittr)\n\n", tidy)
      
      # insert newlines after pipes
      gsub("\\s*%>%\\s*", " %>% \n  ", tidy)
    })
    
  })
  
  # return shiny app
  shiny::shinyApp(ui, server)
}

#' @rdname mudata_app
#' @export
mudata_app_clear_cache <- function() {
  rm(list = ls(envir = mudata_app_options), envir = mudata_app_options)
}

# environment to keep track of mudata app input states
mudata_app_options <- new.env(parent = emptyenv())

# this file uses kentvillegreenwood not just in examples
data("kentvillegreenwood", envir = environment())

