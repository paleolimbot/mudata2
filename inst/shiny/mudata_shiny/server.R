
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# make sure mudata is assigned, or use kentvillegreenwood otherwise
if(exists(".mudata_shiny_object")) {
  md <- .mudata_shiny_object
} else {
  md <- mudata::kentvillegreenwood
  .mudata_shiny_object_name <- "kentvillegreenwood"
}

if(!exists(".mudata_shiny_object_name")) {
  .mudata_shiny_object_name <- "md"
}

# check for function to save changes to the input value
if(!exists(".mudata_shiny_save_inputs")) {
  .mudata_shiny_save_inputs <- function(input) NULL
}

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
  input <- lapply(reactiveValuesToList(input), function(x) {
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

shinyServer(function(input, output) {

  output$input_saved <- reactive(.mudata_shiny_save_inputs(input))
  
  output$muplot <- renderPlot({
    # get args
    subset_args <- generate_subset_args(input, output)
    plot_args <- generate_plot_args(input, output)
    
    # subset mudata
    mdplot <- do.call(subset, c(list(md), subset_args))
    # plot mudata
    do.call(ggplot2::autoplot, c(list(mdplot), plot_args))
  })
  
  output$mutext <- renderText({
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
      .mudata_shiny_object_name,
      sprintf("subset(%s)", 
              paste(names(subset_string_args), subset_string_args, sep = " = ",
                    collapse = ", ")),
      sprintf("autoplot(%s)", 
              paste(names(plot_args), plot_args, sep = " = ",
                    collapse = ", "))
    )
    
    # tidy using formatR
    tidy <- formatR::tidy_source(text = paste(calls, collapse = " %>% "),
                         output = FALSE, width.cutoff = 80,
                         indent = 2)$text.tidy
    
    # insert ggplot2 library cal
    tidy <- paste0("library(ggplot2)\n", tidy)
    
    # insert newlines after pipes
    gsub("\\s*%>%\\s*", " %>% \n  ", tidy)
  })

})
