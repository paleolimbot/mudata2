

#' Interactive Plot of a MUData object
#'
#' @param obj A mudata object
#'
#' @export
#'
#' @examples
#' if(interactive()) {
#'   mudata_app()
#' }
#' 
mudata_app <- function(obj = NULL) {
  if(!is.null(obj)) {
    if(!inherits(obj, "mudata")) stop("obj is not a mudata object")
    # get the object name
    assign(".mudata_shiny_object", obj, envir = .GlobalEnv)
    assign(".mudata_shiny_object_name", deparse(substitute(obj)), envir = .GlobalEnv)
    on.exit({
      rm(list = c(".mudata_shiny_object", ".mudata_shiny_object_name"))
    })
  }
  
  # run the app from the inst/ directory
  shiny::runApp(system.file("shiny/mudata_shiny", package = "mudata"))
}
