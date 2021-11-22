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


#' 
#' @title Write sequence file
#'
#' @description Write the sequence file for Sciex or Bruker MS
#' 
#' @param seq_table the sequence table as data.frame
#' @param file file name
#' @param delim the delimiter to use
#' @param col_names should there be colnams
#' @param eol end of line character
#'
#' @author Rico Derks
#' 
#' @importFrom readr write_delim
#' 
#' @noRd
write_seq_file <- function(seq_table = NULL, file = NULL, delim = c(",", "\t"), col_names = TRUE, eol = "\r\n") {
  if(!is.null(file)) {
    readr::write_delim(x = seq_table,
                       file = file,
                       delim = delim,
                       col_names = col_names,
                       eol = eol)
  } else {
    message("No file specified!")
  }
}