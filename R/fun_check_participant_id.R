#' Check whether the data contains a participant ID column
#'
#' @description
#' Checks whether `data` has at least one column whose name contains
#' `"participant_id"` (e.g. `participant_id`, or a prefixed/suffixed
#' variant such as `participant_id_original`).
#'
#' @param data A data frame containing the measurement data.
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with entries `passed` (logical) and `message` (a
#'   human-readable description of the result).
#' @keywords internal
#' @noRd
fun_check_participant_id <- function(data, data_format){
  
  if (data_format == "wide"){
    
    if (any(grepl("participant_id", names(data))) == TRUE){
      description = "Data has a participant_id variable."
    }
    
    else {
      description = "Data has no participant_id variable."
    }
    
    check <- list(
      passed = (any(grepl("participant_id", names(data)))),
      message = description
    )
  }
  
  else if (data_format == "long"){
    stop(
      "Long format for data not yet implemented. In the meantime,
         please use wide format data."
    )
  }
  
  else {
    stop(
      "Internal error; fun_check_participant_id : data_format"
    )
  }
  
  return(check)
  
}