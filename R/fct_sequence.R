#' sequence 
#' 
#' 
#' @title Prepare sequence table
#'
#' @description Prepare the sequence table
#'
#' @param info Sequence object containing all the information.
#'
#' @return Returns a data frame
#'
#' @export
#'
#' @author Rico Derks
#'
prepare_sequence_table <- function(info){

  # if the a file is used for sample id's get the number of samples by counting rows  
  if(info$sample_info == "file") {
    if(!is.null(info$data)) {
      info$num_samples <- nrow(info$data)
    }
  }
  print(info)
  
  ### create the sequence list
  # start with blanks
  info$seq_table <- data.frame(sample_id = rep("Blank", info$blank_num),
                          ms_method = rep(info$ms_method, info$blank_num),
                          lc_method = rep(info$lc_method, info$blank_num),
                          da_method = rep(info$da_method, info$blank_num),
                          vial_pos = rep(info$blank_pos, info$blank_num),
                          inj_vol = rep(info$inj_vol, info$blank_num))
  
  ### combine qcpool with samples
  # determine the block size
  block_size <- ceiling(info$num_samples / (info$qcpool_num - 1))
  
  switch(info$select_system,
         "bruker" = { # Bruker sequence list
           vial_pos <- create_sample_vials(start_vial = info$sample_start_pos,
                                           number_samples = as.integer(info$num_samples),
                                           vial_type = "48")
         },
         "sciex" = { # Sciex sequence list
           start_vial <- as.numeric(info$sample_start_pos)
           end_vial <- start_vial + as.numeric(info$num_samples) - 1
           vial_pos <- start_vial:end_vial
         })
  
  for (a in 1:info$qcpool_num) {
    if (a < info$qcpool_num){
      # add qcpool
      info$seq_table <- rbind(info$seq_table,
                         data.frame(sample_id = "QCpool",
                                    ms_method = info$ms_method,
                                    lc_method = info$lc_method,
                                    da_method = info$da_method,
                                    vial_pos = info$qcpool_pos,
                                    inj_vol = info$inj_vol))
      
      sample_start <- block_size * (a - 1) + 1
      sample_end <- block_size * a
      if(sample_end > info$num_samples) {
        sample_end <- info$num_samples
      }
      # add samples
      switch(info$sample_info,
             "generate" = {
               info$seq_table <- rbind(info$seq_table,
                                  data.frame(sample_id = sprintf("Sample_cpm%03d", sample_start:sample_end),
                                             ms_method = info$ms_method,
                                             lc_method = info$lc_method,
                                             da_method = info$da_method,
                                             vial_pos = vial_pos[sample_start:sample_end],
                                             inj_vol = info$inj_vol))},
             "file" = {
               info$seq_table <- rbind(info$seq_table,
                                  data.frame(sample_id = paste0("Sample_", values$data[sample_start:sample_end, info$select_sampleid_col]),
                                             ms_method = info$ms_method,
                                             lc_method = info$lc_method,
                                             da_method = info$da_method,
                                             vial_pos = vial_pos[sample_start:sample_end],
                                             inj_vol = info$inj_vol))})
    } else {
      # add the final qcpoolmari
      info$seq_table <- rbind(info$seq_table,
                         data.frame(sample_id = "QCpool",
                                    ms_method = info$ms_method,
                                    lc_method = info$lc_method,
                                    da_method = info$da_method,
                                    vial_pos = info$qcpool_pos,
                                    inj_vol = info$inj_vol))
    }
  }
  
  # end with blanks
  info$seq_table <- rbind(info$seq_table,
                     data.frame(sample_id = rep("Blank", info$blank_num),
                                ms_method = rep(info$ms_method, info$blank_num),
                                lc_method = rep(info$lc_method, info$blank_num),
                                da_method = rep(info$da_method, info$blank_num),
                                vial_pos = rep(info$blank_pos, info$blank_num),
                                inj_vol = rep(info$inj_vol, info$blank_num)))
  
  # add shutdown method when needed
  if (info$shutdown == TRUE) {
    info$seq_table <- rbind(info$seq_table,
                       data.frame(sample_id = "Standby",
                                  ms_method = info$shutdown_msmethod,
                                  lc_method = info$shutdown_lcmethod,
                                  da_method = "",
                                  vial_pos = info$blank_pos,
                                  inj_vol = info$inj_vol))
  }
  ### Create sequence table ###
  # create the table for export
  # Bruker
  switch(info$select_system,
         "bruker" = { # Bruker Hystar
           info$export_seq <- create_hystar_seq(object = info)
           info$ready_download <- TRUE
         },
         "sciex" =  { # AB Sciex Analyst
           info$export_seq <- create_analyst_seq(object = info)
           info$ready_download <- TRUE
         })
  
  return(info$export_seq)
}


#' @title Create a sequence table for Bruker HyStar
#'
#' @description Create a sequence table for Bruker Hystar.
#'
#' @param object Sequence object containing all the information.
#'
#' @return Returns a data frame
#'
#' @export
#'
#' @importFrom dplyr %>% mutate rename n relocate
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
create_hystar_seq <- function(object) {
  object$project_path <- paste("D:\\Data\\Projects",
                               object$user_name,
                               object$project_name,
                               sep = "\\")
  
  object$data_path <- paste("D:\\Data\\Projects",
                            object$user_name,
                            object$project_name,
                            object$data_folder,
                            sep = "\\")
  
  # rename the column names
  export_seq <- object$seq_table %>%
    mutate(Line = 1:n(),
           Injections = "1",
           SampleType = "1",
           AutosamplerMethod = "Standard",
           DataPath = object$data_path,
           lc_method = paste(object$project_path, "Methods", "LC-methods", .data$lc_method, sep = "\\"),
           ms_method = paste(object$project_path, "Methods", "MS-methods", .data$ms_method, sep = "\\"),
           da_method = paste(object$project_path, "Methods", "DA-methods", .data$da_method, sep = "\\"),
           RunDAScript = "0") %>%
    rename(SampleID = .data$sample_id,
           Vial = .data$vial_pos,
           DataAnalysis = .data$da_method,
           `LC Method` = .data$lc_method,
           MS_Method = .data$ms_method,
           Volume = .data$inj_vol) %>%
    relocate(.data$Line,
             .data$SampleID,
             .data$Vial,
             .data$Injections,
             .data$Volume,
             .data$SampleType,
             .data$AutosamplerMethod,
             .data$DataAnalysis,
             .data$DataPath,
             .data$`LC Method`,
             .data$MS_Method,
             .data$RunDAScript)
  
  return(export_seq)
}



#' @title Create a sequence table for AB Sciex Analyst
#'
#' @description Create a sequence table for AB Sciex Analyst.
#'
#' @param object Sequence object containing all the information.
#'
#' @return Returns a data frame
#'
#' @export
#'
#' @importFrom dplyr %>% mutate rename n select
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
create_analyst_seq <- function(object) {
  # rename the column names
  export_seq <- object$seq_table %>%
    mutate(sample_name = .data$sample_id,
           Comments = "",
           ProcMethod = "none",
           RackCode = "1.5mL Standard",
           PlateCode = "1.5mL Standard",
           DilutFact = "1",
           WghtToVol = "0",
           Type = "Unknown",
           RackPos = "1",
           PlatePos = "1",
           SetName = "SET1",
           OutputFile = paste(object$data_folder, sprintf(paste0(.data$sample_id, "_%03d"), 1:n()), sep = "\\")) %>%
    rename(`% header=SampleName` = .data$sample_name,
           SampleID = .data$sample_id,
           VialPos = .data$vial_pos,
           AcqMethod = .data$ms_method,
           SmplInjVol = .data$inj_vol) %>%
    select(.data$`% header=SampleName`,
           .data$SampleID,
           .data$Comments,
           .data$AcqMethod,
           .data$ProcMethod,
           .data$RackCode,
           .data$PlateCode,
           .data$VialPos,
           .data$SmplInjVol,
           .data$DilutFact,
           .data$WghtToVol,
           .data$Type,
           .data$RackPos,
           .data$PlatePos,
           .data$SetName,
           .data$OutputFile)
  
  return(export_seq)
}


#' @title Create a sequence with sample vial positions
#'
#' @description Create a sequence with sample vial positions. This is needed for the
#'     LC which is combined with the Bruker MS. It in general has 3 colored positions
#'     (red, green and blue) and in there 48 vial positions.
#'
#' @param start_vial The starting position of the vial.
#' @param number_samples The number of samples.
#' @param vial_type Define the number of vials per plate. 48 is glass vials and
#'     96 is 96-well plate.
#'
#' @return Returns character vector with all possible vial positions.
#'
#' @export
#'
#' @author Rico Derks
#'

create_sample_vials <- function(start_vial = NULL, number_samples = NULL, vial_type = c("48", "96")) {
  ### sanity checks
  if (is.null(start_vial)) {
    stop("No start position given!")
  }
  
  if(is.null(number_samples)) {
    stop("No number of samples given.")
  } else {
    if(number_samples <= 0) {
      stop("Zero of negative number of samples given!")
    }
  }
  
  vial_type <- match.arg(arg = vial_type)
  
  ### here is where it happens
  # define 48 vial
  if(vial_type == "48") {
    num_row <- 5
    num_col <- 8
  }
  
  # define 96 well plate
  if(vial_type == "96") {
    num_row <- 8
    num_col <- 12
  }
  
  # create all available vial positions for one plate
  vial_pos <- unlist(lapply(LETTERS[1:num_row], function(x) paste(x, 1:num_col, sep = "")))
  
  # all plates (R, G, B)
  vial_pos <- paste(c(rep("R", num_row * num_col),
                      rep("G", num_row * num_col),
                      rep("B", num_row * num_col)), vial_pos, sep = "")
  
  # get the vial positions
  idx_start_vial <- which(vial_pos == start_vial)
  vial_pos <- vial_pos[idx_start_vial:(idx_start_vial + number_samples)]
  
  return(vial_pos)
}

