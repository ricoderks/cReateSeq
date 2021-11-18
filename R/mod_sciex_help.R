#' sciex_help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sciex_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Sciex Help"),
      p("UNDER CONSTRUCTION")
    )
  )
}
    
#' sciex_help Server Functions
#'
#' @noRd 
mod_sciex_help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_sciex_help_ui("sciex_help_ui_1")
    
## To be copied in the server
# mod_sciex_help_server("sciex_help_ui_1")
