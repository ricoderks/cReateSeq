#' input_vials UI Function
#'
#' @description Module to get the vial information.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_vials_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column = 12,
      h4("Vial positions")),
    fluidRow(
      column(
        width = 3,
        uiOutput(outputId = ns("vial_pos_blank"))),
      column(
        width = 3,
        uiOutput(outputId = ns("vial_pos_qcpool"))),
      column(
        width = 3,
        uiOutput(outputId = ns("vial_pos_sample"))),
      column(
        width = 3,
        numericInput(inputId = ns("inj_vol"),
                     label = "Injection volumen [uL]:",
                     value = 10,
                     min = 1,
                     max = 100,
                     step = 1)),
      style = "background-color: #E8E8E8"
    )
  )
}

#' input_vials Server Functions
#'
#' @noRd 
mod_input_vials_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #### Blanks
    # blank vials UI
    output$vial_pos_blank <- renderUI({
      req(r$select_system)
      
      switch(r$select_system,
             "bruker" = {
               tagList(
                 verticalLayout(textInput(inputId = ns("bruker_blank_pos"),
                                          label = "Blank vial position:",
                                          placeholder = "RA1",
                                          width = "220px"),
                                numericInput(inputId = ns("num_blank"),
                                             label = "Number of blanks:",
                                             value = 3,
                                             min = 1,
                                             step = 1,
                                             width = "220px")))
             },
             "sciex" = {
               tagList(
                 verticalLayout(textInput(inputId = ns("sciex_blank_pos"),
                                          label = "Blank vial position:",
                                          value = "20001",
                                          placeholder = "20001"),
                                numericInput(inputId = ns("num_blank"),
                                             label = "Number of blanks:",
                                             value = 3,
                                             min = 1,
                                             step = 1,
                                             width = "200px")))
             })
    })
    
    # blank vial position Bruker
    observeEvent(input$bruker_blank_pos, {
      if(!is.null(input$bruker_blank_pos)) {
        r$blank_pos <- input$bruker_blank_pos
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$blank_pos <- NULL
      }
    })
    
    # blank vial position Sciex
    observeEvent(input$sciex_blank_pos, {
      if(!is.null(input$sciex_blank_pos)) {
        r$blank_pos <- input$sciex_blank_pos
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$blank_pos <- NULL
      }
    })
    
    # number of blanks
    observeEvent(input$num_blank, {
      if(!is.null(input$num_blank)) {
        r$blank_num <- input$num_blank
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$blank_num <- NULL
      }
    })
    ####
    
    #### QCpool
    # qcpool vials
    output$vial_pos_qcpool <- renderUI({
      req(r$select_system)
      
      switch(r$select_system,
             "bruker" = {
               tagList(
                 verticalLayout(textInput(inputId = ns("bruker_qc_pos"),
                                          label = "QCpool vial position:",
                                          placeholder = "RA2",
                                          width = "220px"),
                                numericInput(inputId = ns("num_qc"),
                                             label = "Number of QCpool:",
                                             value = 4,
                                             min = 1,
                                             step = 1,
                                             width = "220px")))
             },
             "sciex" = {
               tagList(
                 verticalLayout(textInput(inputId = ns("sciex_qc_pos"),
                                          label = "QCpool vial position:",
                                          value = "20002",
                                          placeholder = "20002"),
                                numericInput(inputId = ns("num_qc"),
                                             label = "Number of QCpool:",
                                             value = 4,
                                             min = 1,
                                             step = 1,
                                             width = "220px")))
             })
    })
    
    # qcpool vial position Bruker
    observeEvent(input$bruker_qc_pos, {
      if(!is.null(input$bruker_qc_pos)) {
        r$qcpool_pos <- input$bruker_qc_pos
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$qcpool_pos <- NULL
      }
    })
    
    # qcpool vial position Sciex
    observeEvent(input$sciex_qc_pos, {
      if(!is.null(input$sciex_qc_pos)) {
        r$qcpool_pos <- input$sciex_qc_pos
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$qcpool_pos <- NULL
      }
    })
    
    # number of qcpools
    observeEvent(input$num_qc, {
      if(!is.null(input$num_qc)) {
        r$qcpool_num <- input$num_qc
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$qcpool_num <- NULL
      }
    })
    ####
    
    #### Samples
    # sample vials
    output$vial_pos_sample <- renderUI({
      req(r$select_system)
      
      switch(r$select_system,
             "bruker" = {
               tagList(
                 textInput(inputId = ns("bruker_start_sample_pos"),
                           label = "Start sample position:",
                           placeholder = "RB1"))
             },
             "sciex" = {
               tagList(
                 textInput(inputId = ns("sciex_start_sample_pos"),
                           label = "Start sample position:",
                           value = "1",
                           placeholder = "1"))
             })
    })
    
    # sample vial position Bruker
    observeEvent(input$bruker_start_sample_pos, {
      if(!is.null(input$bruker_start_sample_pos)) {
        r$sample_start_pos <- input$bruker_start_sample_pos
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$sample_start_pos <- NULL
      }
    })
    
    # sample vial position Sciex
    observeEvent(input$sciex_start_sample_pos, {
      if(!is.null(input$sciex_start_sample_pos)) {
        r$sample_start_pos <- input$sciex_start_sample_pos
        # disable download button when something chamges
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$sample_start_pos <- NULL
      }
    })
    ####
    
    
    
  })
}
