#' input_form UI Function
#'
#' @description Module containing the input form..
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput renderDT
#'
mod_input_form_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      # select which MS system 
      mod_input_system_ui(id = ns("input_system")),
      
      # project info
      mod_input_project_info_ui(id = ns("input_project_info")),

      # methods
      mod_input_methods_ui(id = ns("input_methods")),
      
      # vials
      mod_input_vials_ui(id = ns("input_vials")),
      
      # sample info
      mod_input_sample_info_ui(id = ns("input_sample_info"))
      
    )
  )
}

#' input_form Server Functions
#'
#' @noRd 
mod_input_form_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # which system
    mod_input_system_server(id = "input_system",
                            r = r)
    
    # project info
    mod_input_project_info_server(id = "input_project_info",
                                  r = r)
    
    # methods
    mod_input_methods_server(id = "input_methods",
                             r = r)
    
    
    # vials
    mod_input_vials_server(id = "input_vials",
                           r = r)
    
    # sample info
    mod_input_sample_info_server(id = "input_sample_info",
                                 r = r)
    
    
  })
}
