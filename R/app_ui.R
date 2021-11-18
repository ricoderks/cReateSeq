#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom utils packageVersion
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    navbarPage(
      title = paste0("cReateSeq", utils::packageVersion("cReateSeq")),
      # create sequence table
      tabPanel(
        title = "Create sequence table"
      ), # end sequence table
      
      # start help
      navbarMenu(
        title = "Help",
        tabPanel(
          title = "Bruker",
          mod_bruker_help_ui(id = "bruker_help")
        ),
        tabPanel(
          title = "Scies",
          mod_sciex_help_ui(id = "sciex_help")
        ),
        "----",
        tabPanel(
          title = "About",
          mod_about_ui(id = "about")
        )
      ) # end help
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'cReateSeq'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

