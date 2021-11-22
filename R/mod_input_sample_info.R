#' input_sample_info UI Function
#'
#' @description Module to handle sample information / sample id's.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_sample_info_ui <- function(id){
  ns <- NS(id)
  tagList(
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
}

#' input_sample_info Server Functions
#'
#' @noRd 
mod_input_sample_info_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
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
    
    ### check if sample id's will be generated or retrieved from file
    observeEvent(input$sample_info, {
      
      if(!is.null(input$sample_info)) {
        r$sample_info <- input$sample_info
        # disable download button when something changes
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else{
        r$sample_info <- NULL
      }
    })
    
    ### get the number of samples
    observeEvent(input$num_samples, {
      
      if(!is.null(input$num_samples)) {
        r$num_samples <- input$num_samples
        # disable download button when something changes
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else{
        r$num_samples <- NULL
      }
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
    
    
    observeEvent(input$select_sampleid_col, {
      # req(input$select_sampleid_col)
      
      if(!is.null(input$select_sampleid_col)) {
        r$select_sampleid_col <- input$select_sampleid_col
        # disable download button when something changes
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$select_sampleid_col <- NULL
      }
    })
    
    observeEvent(input$sample_list_file, {
      # req(input$sample_list_file)
      
      if(!is.null(input$sample_list_file)) {
        r$sample_list_file <- input$sample_list_file
        # disable download button when something changes
        r$ready_download <- FALSE
        r$export_seq <- NULL
      } else {
        r$sample_list_file <- NULL
      }
    })
    
    ### after button click, get all information to create a sequence table ###
    observeEvent(input$make_seq, {
      # create the sequence list
      
      switch(input$sample_info,
             "generate" = {
               r$export_seq <- prepare_sequence_table(info = r)
             },
             "file" = {
               if(!is.null(input$select_sampleid_col)) {
                 if(input$select_sampleid_col != "none") {
                   r$export_seq <- prepare_sequence_table(info = r)
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
        switch(r$select_system,
               "bruker" = {
                 "sequence_table.csv"
               },
               "sciex" = {
                 "sequence_table.txt"
               })
      },
      content = function(file) {
        # you can still do some stuff here
        switch(r$select_system,
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
