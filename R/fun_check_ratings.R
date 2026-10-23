#' Check whether every rating has at least one matching column
#'
#' @description
#' For every rating name in `rating_names`, checks whether `data` contains
#' at least one column whose name matches that rating.
#'
#' @param data A data frame containing the measurement data.
#' @param metadata A data frame of study metadata. Currently unused, kept
#'   for signature consistency with the other `fun_check_*()` functions.
#' @param rating_names Character vector of rating abbreviations to check
#'   for (as returned by `fun_which_ratings()`).
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with entries `passed` (logical, `TRUE` if every rating
#'   was found), `ratings` (the rating names checked), `pass_ratings` (a
#'   logical vector, one entry per rating), and `message` (a
#'   human-readable description of the result).
#' @keywords internal
#' @noRd
fun_check_ratings <- function(data, metadata, rating_names, data_format){

  # Check

  if (data_format == "wide"){

    pass <- c()

    for (n in seq_along(rating_names)){
      pass[n] <- any(grepl(rating_names[n], names(data)))
    }

    if (all(pass) == TRUE){
      messg = "All ratings have at least one variable in the data."
    }

    else if (any(pass) == TRUE){
      messg = paste0(sum(pass), " of ", length(pass), " ratings have at least one variable in the data.")
    }
    else {
      messg = "No variables were found for any rating in the data."
    }

    # Create description with dynamic line breaks
    description = paste(strwrap(messg, width = 41), collapse = "\n")

    check <- list(
      passed = all(pass),
      ratings = rating_names,
      pass_ratings = pass,
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
      "Internal error; fun_check_ratings : data_format"
    )
  }

  return(check)

}
