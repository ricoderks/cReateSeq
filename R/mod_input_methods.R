#' input_methods UI Function
#'
#' @description Module to handle the methods part.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_methods_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column = 12,
      h4("Methods")),
    fluidRow(
      column(
        width = 6,
        uiOutput(outputId = ns("methods"))),
      column(
        width = 6,
        checkboxInput(inputId = ns("shutdown"),
                      label = "Use shutdown methods:",
                      value = FALSE),
        uiOutput(outputId = ns("use_shutdown"))),
      style = "background-color: #E8E8E8"
    )
  )
}

#' input_methods Server Functions
#'
#' @noRd 
mod_input_methods_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ### Generate the method part input fields ###
    output$methods <- renderUI({
      req(r$select_system)
      
      switch(r$select_system,
             "bruker" = {
               tagList(
                 textInput(inputId = ns("bruker_lcmethod"),
                           label = "LC method:",
                           placeholder = "RP.m"),
                 textInput(inputId = ns("bruker_msmethod"),
                           label = "MS method:",
                           placeholder = "MS_pos.m"),
                 textInput(inputId = ns("bruker_damethod"),
                           label = "Data analysis method:",
                           placeholder = "DA_pos.m"))
             },
             "sciex" = {
               tagList(
                 textInput(inputId = ns("sciex_msmethod"),
                           label = "Acquistion method:",
                           # value = "MS_pos.dam",
                           placeholder = "MS_pos.dam"))
             })
    })
    
    # store lc method bruker
    observeEvent(input$bruker_lcmethod, {
      
      if(!is.null(input$bruker_lcmethod)) {
        r$lc_method <- input$bruker_lcmethod
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$lc_method <- ""
      }
    })
    
    # store ms method bruker
    observeEvent(input$bruker_msmethod, {
      
      if(!is.null(input$bruker_msmethod)) {
        r$ms_method <- input$bruker_msmethod
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$ms_method <- ""
      }
    })
    
    # store da method bruker
    observeEvent(input$bruker_damethod, {
      
      if(!is.null(input$bruker_damethod)) {
        r$da_method <- input$bruker_damethod
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$da_method <- ""
      }
    })
    
    # store ms method Sciex
    observeEvent(input$sciex_msmethod, {
      
      if(!is.null(input$sciex_msmethod)) {
        r$ms_method <- input$sciex_msmethod
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$ms_method <- ""
      }
    })
    
    
    ### generate the shutdown method parts
    output$use_shutdown <- renderUI({
      req(r$select_system,
          input$shutdown)
      
      if(input$shutdown == TRUE){
        switch(r$select_system,
               "bruker" = {
                 tagList(
                   textInput(inputId = ns("lcshutdown"),
                             label = "LC shutdown method:",
                             placeholder = "RP_shutdown.m"),
                   textInput(inputId = ns("msshutdown"),
                             label = "MS shutdown method:",
                             placeholder = "MS_pos_shutdown.m"))
               },
               "sciex" = {
                 tagList(
                   textInput(inputId = ns("msshutdown"),
                             label = "MS shutdown method:",
                             # value = "shutdown_pos.dam",
                             placeholder = "shutdown_pos.dam"))
               })
      }
    })
    
    
    # store lc shutdown method  
    observeEvent({
      input$lcshutdown
    },
    {
      req(input$lcshutdown)
      
      if(!is.null(input$lcshutdown)) {
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
        r$shutdown_lcshutdown <- input$lcshutdown
      } else {
        r$shutdown_lcshutdown <- NULL
      }
    })
    
    # store ms shutdown method
    observeEvent({
      input$msshutdown
    },
    {
      req(input$msshutdown)
      
      if(!is.null(input$msshutdown)) {
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
        r$shutdown_msshutdown <- input$msshutwon
      } else {
        r$shutdown_msshutdown <- NULL
      }
    })
  })
}
