#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
 
  # store some information
  r <- reactiveValues()
  
  # input form
  mod_input_form_server(id = "input_form",
                        r = r)
  
  # the sequence table output
  mod_sequence_table_server(id = "sequence_table", 
                            r = r)
  
  # about section
  mod_about_server(id = "about")
   
}
