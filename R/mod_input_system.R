#' input_system UI Function
#'
#' @description Select for which system you want to create a sequence table.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_system_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column = 12,
      h4("System")
    ),
    fluidRow(
      column(
        width = 12,
        radioButtons(
          inputId = ns("select_system"),
          label = "Select system:",
          choices = c("Bruker - HyStar" = "bruker",
                      "AB Sciex - Analyst" = "sciex"),
          selected = "bruker"),
        style = "background-color: #E8E8E8")
    )
  )
}

#' input_system Server Functions
#'
#' @noRd 
mod_input_system_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    observe({
      req(input$select_system)
      
      r$select_system <- input$select_system
    })
  })
}


