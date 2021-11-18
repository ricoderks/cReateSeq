#' bruker_help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bruker_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Bruker Help"),
      h3("Note:"),
      p("This will create a sequence table for HyStar. There are several assumptions to be able to use this.",
        HTML("<ul>
              <li>It assumes a Thermo Scientific (Dionex) as LC.</li>
              <li>The following project structure:</li>
              <ul>
              <li>User name</li>
              <ul>
              <li>Project name</li>
              <ul>
              <li>Data</li>
              <li>Methods</li>
              <ul>
              <li>DA-Methods</li>
              <li>LC-Methods</li>
              <li>MS-Methods</li>
              </ul>
              <li>SampleTables</li>
              </ul>
              </ul>
              </ul>
              </ul>")
      )
    )
  )
}

#' bruker_help Server Functions
#'
#' @noRd 
mod_bruker_help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_bruker_help_ui("bruker_help_ui_1")

## To be copied in the server
# mod_bruker_help_server("bruker_help_ui_1")
