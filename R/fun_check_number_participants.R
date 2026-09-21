#' Check whether the number of observed participants matches the metadata
#'
#' @description
#' Compares the number of rows in `data` (one row per participant, in wide
#' format) against the `n_subjects` value declared in `metadata`.
#'
#' @param data A data frame containing the measurement data (one row per
#'   participant in wide format).
#' @param metadata A one-row data frame of study metadata; must contain an
#'   `n_subjects` column.
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with entries `passed` (logical), `observed` (the number
#'   of rows found in `data`), `expected` (`metadata$n_subjects`), and
#'   `message` (a human-readable description of the result).
#' @keywords internal
#' @noRd
fun_check_number_participants <- function(data, metadata, data_format){
  
  if (data_format == "wide"){
    
    n_data <- nrow(data)
    n_meta <- metadata$n_subjects
    
    if (n_data == n_meta){
      description = "Number of participants is correct"
    }
    
    else {
      description = paste0("Expected ", n_meta, " participants, found ", n_data, ".")
    }
    
    check <- list(
      passed = (n_data == n_meta),
      observed = n_data,
      expected = n_meta,
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
      "Internal error; fun_check_number_participants : data_format"
    )
  }
  
  return(check)
  
}