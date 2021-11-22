#' input_project_info UI Function
#'
#' @description Module to get the project information..
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_project_info_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column = 12,
      h4("Project info")
    ),
    fluidRow(
      column(
        width = 4,
        textInput(inputId = ns("username"),
                  label = "User name:",
                  # value = "Rico",
                  placeholder = "Rico")),
      column(
        width = 4,
        textInput(inputId = ns("projectname"),
                  # value = "test",
                  label = "Project name:",
                  placeholder = "My_project")),
      column(
        width = 4,
        textInput(inputId = ns("datafolder"),
                  # value = "test_sequence",
                  label = "Data folder:",
                  placeholder = "sequence1")),
      style = "background-color: #E8E8E8"),
  )
}

#' input_project_info Server Functions
#'
#' @noRd 
mod_input_project_info_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # get the username
    observeEvent(input$username, {
      
      r$username <- input$username
      # disable download button when something changes
      r$ready_download <- FALSE
      r$export_seq <- NULL
    })
    
    # get the project name
    observeEvent(input$projectname, {
      
      r$projectname <- input$projectname
      # disable download button when something changes
      r$ready_download <- FALSE
      r$export_seq <- NULL
    })
    
    # get the data folder
    observeEvent(input$datafolder, {
      
      r$datafolder <- input$datafolder
      # disable download button when something changes
      r$ready_download <- FALSE
      r$export_seq <- NULL
    })
    
    
  })
}
