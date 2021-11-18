#' helpers 
#'
#' @title Read Excel file
#' 
#' @description Read Excel file with sample id's.
#'
#' @param file file path of the Excel file to read
#'
#' @description A fct function
#' 
#' @details The first sheet is read. The first row should be column headers.
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom openxlsx read.xlsx
#'
#' @author Rico Derks
#'
#' @noRd
read_excel_file <- function(file = NULL) {
  if(!is.null(file)) {
    if(file.exists(file)) {
      my_df <- read.xlsx(xlsxFile = file,
                         sheet = 1,
                         colNames = TRUE)
    } else {
      message("File does not exist!")
      my_df <- NULL
    }
  } else {
    message("No file specified!")
    my_df <- NULL
  }
  
  return(my_df)
}