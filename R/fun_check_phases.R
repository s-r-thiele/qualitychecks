#' Check whether every study phase has at least one matching column
#'
#' @description
#' For every phase name declared in `study_design`, checks whether `data`
#' contains at least one column whose name matches that phase.
#'
#' @param data A data frame containing the measurement data.
#' @param study_design A data frame with a `name` column listing the study
#'   phases (e.g. `"acq"`, `"ext"`, `"rex"`).
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with entries `passed` (logical, `TRUE` if every phase
#'   was found), `phases` (the phase names checked), `pass_phases` (a
#'   logical vector, one entry per phase), and `message` (a human-readable
#'   description of the result).
#' @keywords internal
#' @noRd
fun_check_phases <- function(data, study_design, data_format){

  if (data_format == "wide"){

    phase_names <- study_design$name

    pass <- c()

    for (n in seq_along(phase_names)){
      pass[n] <- any(grepl(phase_names[n], names(data)))
    }

    if (all(pass) == TRUE){
      description = c("All phases have at least one variable in the data.")
    }

    else if (any(pass) == TRUE){
      description = paste0(sum(pass), " of ", length(pass), " phases have at least one variable in the data.")
    }
    else {
      description <- c("No variables were found for any phase in the data.")
    }

    check <- list(
      passed = all(pass),
      phases = phase_names,
      pass_phases = pass,
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
      "Internal error; fun_check_phases : data_format"
    )
  }

  return(check)

}
