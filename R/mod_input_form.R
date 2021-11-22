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
      
      fluidRow(
        column = 12,
        h4("Vial positions")),
      fluidRow(
        column(width = 3,
               uiOutput(outputId = ns("vial_pos_blank"))),
        column(width = 3,
               uiOutput(outputId = ns("vial_pos_qcpool"))),
        column(width = 3,
               uiOutput(outputId = ns("vial_pos_sample"))),
        column(width = 3,
               numericInput(inputId = ns("inj_vol"),
                            label = "Injection volumen [uL]:",
                            value = 10,
                            min = 1,
                            max = 100,
                            step = 1)),
        style = "background-color: #E8E8E8"),
      fluidRow(
        column = 12,
        h4("Sample info")),
      fluidRow(
        column(width = 3,
               radioButtons(inputId = ns("sample_info"),
                            label = "Generate sample ID's or read from file:",
                            choices = c("generate" = "generate",
                                        "from file" = "file"),
                            selected = "generate")),
        column(width = 3,
               uiOutput(outputId = ns("sample_info_ui"))),
        column(width = 3,
               actionButton(inputId = ns("make_seq"),
                            label = "Create sequence list"),
               uiOutput(outputId = ns("download_button")),
               verbatimTextOutput(outputId = ns("debug_text"))),
        style = "background-color: #E8E8E8"
      ),
      fluidRow(
        column = 12,
        DT::DTOutput(outputId = ns("sample_list_table"))
      )
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
    
    
    
    
    
    
    ### Generate the vial positions input fields ###
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
    
    ### generate the ui for generating sample info or reading sample info
    output$sample_info_ui <- renderUI({
      req(input$sample_info)
      
      switch(input$sample_info,
             "generate" = {
               tagList(
                 p("Sample ID's will be created automatically, i.e. cpm001, cpm002, etc."),
                 numericInput(inputId = ns("num_samples"),
                              label = "Number of samples:",
                              value = 10,
                              min = 1,
                              step = 1,
                              width = "200px"))
             },
             "file" = {
               tagList(
                 p("Read sample ID's from file"),
                 fileInput(inputId = ns("sample_list_file"),
                           label = "Sample file:",
                           multiple = FALSE,
                           accept = c(".xlsx", ".xls"),
                           width = 400),
                 selectInput(inputId = ns("select_sampleid_col"),
                             label = "Select the column as sample ID :",
                             choices = c("none" = "none")))
             })
    })
    
    ### watch the filename
    observe({
      req(input$sample_list_file)
      
      # store the real filename
      r$excel_file <- input$sample_list_file$name
      r$excel_path <- input$sample_list_file$datapath
      
      # read the data from the first sheet
      r$data <- read_excel_file(file = r$excel_path)
      
    })
    
    ### update column select input
    observe({
      req(input$sample_info)
      req(r$data)
      
      if(input$sample_info == "file" & !is.null(r$data)) {
        column_names <- colnames(r$data)
        
        updateSelectInput(session = session,
                          inputId = "select_sampleid_col",
                          label = "Select the column as sample ID :",
                          choices = c("none", column_names),
                          selected = "none")
      }
    })
    
    ### show the first few lines of the sample info file
    output$sample_list_table <- DT::renderDT({
      req(input$sample_info)
      
      if(input$sample_info == "file") {
        head(r$data, 5)
      }
    },
    caption = "First 5 lines of sample file:",
    options = list(lengthChange = FALSE,
                   dom = "t",
                   ordering = FALSE),
    selection = "none"
    )
    
    ### When something changes remove the sequence list
    observeEvent({
      input$select_system
      input$username
      input$projectname
      input$datafolder
      input$msmethod
      input$lcmethod
      input$damethod
      input$shutdown
      input$bruker_blank_pos
      input$bruker_qc_pos
      input$bruker_start_sample_pos
      input$sciex_blank_pos
      input$sciex_qc_pos
      input$sciex_start_sample_pos
      input$num_samples
      input$num_qc
      input$num_blank
      input$inj_vol
      input$sample_info
    }, {
      # disable download button when switching system
      r$ready_download <- FALSE
      r$export_seq <- NULL
    })
    
    
    
    observeEvent({
      input$select_sampleid_col
    },
    {
      req(input$select_sampleid_col)
      
      if(!is.null(input$select_sampleid_col)) {
        # disable download button when switching system
        r$ready_download <- FALSE
        r$export_seq <- NULL
      }
    })
    
    observeEvent({
      input$sample_list_file
    },
    {
      req(input$sample_list_file)
      
      if(!is.null(input$sample_list_file)) {
        # disable download button when switching system
        r$ready_download <- FALSE
        r$export_seq <- NULL
      }
    })
    
    ### after button click, get all information to create a sequence table ###
    observeEvent(input$make_seq, {
      # create the sequence list
      
      switch(input$sample_info,
             "generate" = {
               r$export_seq <- prepare_sequence_table(info = r,
                                                      input = input)},
             "file" = {
               if(!is.null(input$select_sampleid_col)) {
                 if(input$select_sampleid_col != "none") {
                   r$export_seq <- prepare_sequence_table(info = r,
                                                          input = input)
                 }
               }
             }
      )
    })
    
    ### Download part ###
    # create the download button
    output$download_button <- renderUI({
      req(r$ready_download)
      
      if(r$ready_download) {
        downloadButton(outputId = ns("download_sequence_table"),
                       label = "Download sequence table")
      }
    })
    
    # create the file and offer for download
    output$download_sequence_table <- downloadHandler(
      filename = function() {
        switch(input$select_system,
               "bruker" = {
                 "sequence_table.csv"
               },
               "sciex" = {
                 "sequence_table.txt"
               })
      },
      content = function(file) {
        # you can still do some stuff here
        switch(input$select_system,
               "bruker" = {
                 delim <-  ","
               },
               "sciex" = {
                 delim <- "\t"
               })
        # show progress
        withProgress(message = "Downloading....",
                     value = 0,
                     { # first mimic some progress
                       incProgress(1/10)
                       Sys.sleep(1)
                       incProgress(5/10)
                       # switch(input$select_system,
                       #        "bruker" = {
                       #          # export the file for bruker
                       #        },
                       #        "sciex" = {
                       # export the file for Sciex
                       write_seq_file(seq_table = r$export_seq,
                                      file = file,
                                      delim = delim,
                                      col_names = TRUE,
                                      eol = "\r\n")
                       # })
                     })
      }
    )
    
  })
}
