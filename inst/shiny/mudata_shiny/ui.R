
# This is the user-interface definition of a Shiny web application.
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

# check for saved input arg values
if(!exists(".mudata_shiny_inputs")) {
  .mudata_shiny_inputs <- list()
}

# make list of data columns
data_columns <- c(list("Auto" = "__NULL__"), 
                  stats::setNames(colnames(md$data), colnames(md$data)))

# make function to get default values
default_input_value <- function(id, default) {
  df <- .mudata_shiny_inputs[[id]]
  if(!is.null(df)) {
    df
  } else {
    default
  }
}

shinyUI(fluidPage(

  # Application title
  titlePanel(sprintf("Interactive Plot: %s", .mudata_shiny_object_name)),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("subset_datasets", "Datasets", 
                         choices = sort(unique(md$data$dataset)),
                         selected = default_input_value('subset_datasets',
                                                        NULL)),
      checkboxGroupInput("subset_locations", "Locations", 
                         choices = sort(unique(md$data$location)),
                         selected = default_input_value('subset_locations',
                                                        NULL)),
      checkboxGroupInput("subset_params", "Params", 
                         choices = sort(unique(md$data$param)),
                         selected = default_input_value('subset_params',
                                                        NULL)),
      selectInput("plot_y", "Y Variable:", choices = data_columns,
                  selected = default_input_value('plot_y', "__NULL__")),
      selectInput("plot_col", "Colour Variable:", choices = data_columns,
                  selected = default_input_value('plot_col', "__NULL__")),
      selectInput("plot_shape", "Shape Variable:", choices = data_columns,
                  selected = default_input_value('plot_shape', "__NULL__")),
      selectInput("plot_linetype", "Linetype Variable:", choices = data_columns,
                  selected = default_input_value('plot_linetype', "__NULL__")),
      selectInput("plot_facets", "Facet Variable:", choices = data_columns,
                  selected = default_input_value('plot_facets', "__NULL__")),
      selectInput("plot_errors", "Error Variable:", choices = data_columns,
                  selected = default_input_value('plot_errors', "__NULL__")),
      checkboxGroupInput("plot_geom", "Layers", 
                         choices = c("point", "line", "path"),
                         selected = default_input_value('plot_geom', 'path')),
      numericInput("plot_facet_cols", "Facet Columns", 
                   value = default_input_value('plot_facet_cols', 3), 
                   min = 1, step = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("muplot", height = 600),
      verbatimTextOutput("mutext")
    )
  )
))
