#' sequence_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput renderDT
#' 
mod_sequence_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column = 12,
      DT::DTOutput(outputId = ns("seq_table"))
    )
  )
}

#' sequence_table Server Functions
#'
#' @noRd 
mod_sequence_table_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ### show the created sequence, this table will be editable
    output$seq_table <- DT::renderDT({
      req(r$export_seq)
      
      r$export_seq
      
    },
    selection = "none",
    editable = TRUE,
    options = list(lengthChange = FALSE,
                   dom = "t",
                   ordering = FALSE,
                   pageLength = -1),
    rownames = FALSE
    )
    
  })
}

