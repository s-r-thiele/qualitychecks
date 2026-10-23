#' Check whether every questionnaire has at least one matching column
#'
#' @description
#' For every questionnaire name in `quest_names`, checks whether `data`
#' contains at least one column whose name matches that questionnaire.
#'
#' @param data A data frame containing the measurement data.
#' @param metadata A data frame of study metadata. Currently unused, kept
#'   for signature consistency with the other `fun_check_*()` functions.
#' @param quest_names Character vector of questionnaire abbreviations to
#'   check for (as returned by `fun_which_questionnaires()`).
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with entries `passed` (logical, `TRUE` if every
#'   questionnaire was found), `questionnaires` (the questionnaire names
#'   checked), `pass_questionnaires` (a logical vector, one entry per
#'   questionnaire), and `message` (a human-readable description of the
#'   result).
#' @keywords internal
#' @noRd
fun_check_questionnaires <- function(data, metadata, quest_names, data_format){

  if (data_format == "wide"){

    pass <- c()

    for (n in seq_along(quest_names)){
      pass[n] <- any(grepl(quest_names[n], names(data)))
    }

    if (all(pass) == TRUE){
      messg = "All questionnaires have at least one variable in the data."
    }

    else if (any(pass) == TRUE){
      messg = paste0(sum(pass), " of ", length(pass), " questionnaires have at
                           least one variable in the data.")
    }
    else {
      messg = "No variables were found for any questionnaire in the data."
    }

    # Create description with dynamic line breaks
    description = paste(strwrap(messg, width = 41), collapse = "\n")

    check <- list(
      passed = all(pass),
      questionnaires = quest_names,
      pass_questionnaires = pass,
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
      "Internal error; fun_check_questionnaires : data_format"
    )
  }

  return(check)

}
