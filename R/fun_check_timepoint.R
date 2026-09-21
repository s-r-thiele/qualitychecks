#' Check whether every specified timepoint has a matching column
#'
#' @description
#' For every rating and questionnaire declared in `metadata`, checks
#' whether the timepoint(s) specified in its `<measure>_timepoint` column
#' (comma-separated if more than one) each have at least one matching
#' column in `data`.
#'
#' @note
#' `pmeasure_names` is accepted but currently unused: physiological
#' measures are not yet included in the timepoint check. Known limitation,
#' not addressed here.
#'
#' @param data A data frame containing the measurement data.
#' @param metadata A data frame of study metadata, containing a
#'   `<measure>_timepoint` column for every rating/questionnaire
#'   administered.
#' @param study_design A data frame describing the study's phases.
#'   Currently unused, kept for signature consistency with the other
#'   `fun_check_*()` functions.
#' @param rating_names Character vector of rating abbreviations, as
#'   returned by `fun_which_ratings()` (may still carry a `"us_"` prefix).
#' @param rating_names_fix Character vector of rating abbreviations with
#'   any `"us_"` prefix stripped, as returned by `fix_rating_names()`.
#' @param quest_names Character vector of questionnaire abbreviations, as
#'   returned by `fun_which_questionnaires()`.
#' @param pmeasure_names Character vector of physiological-measure
#'   abbreviations. Currently unused (see Note).
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with a top-level `passed` (logical) and `description`
#'   (character), plus one named entry per rating/questionnaire (named by
#'   its abbreviation), each a list with `value` (the specified
#'   timepoint(s)), `pass` (logical), and `check` (a named list, one entry
#'   per timepoint, `TRUE`/`FALSE`/`NA`).
#' @keywords internal
#' @noRd
fun_check_timepoint <- function(data, metadata, study_design,
                                rating_names, rating_names_fix,
                                quest_names, pmeasure_names,
                                data_format){

  if (data_format == "wide"){

    # Create structure of results object

    check <- list()

    # Substructure timepoint
    check$passed <- logical()

    check$description <- character()

    # Create variable with relevant measures
    measures_clean <- c(rating_names_fix, quest_names)

    if (length(rating_names) == 0){
      ratings <- character(0)
    }
    else{
      ratings <- paste0("rating_", rating_names)
    }

    if (length(quest_names) == 0){
      questionnaires <- character(0)
    }
    else{
      questionnaires <- paste0("quest_", quest_names)
    }

    measures <- c(ratings, questionnaires)

    measures_t <- paste0(measures, "_timepoint")

    # Get timepoint info from metadata
    timepoints <- tibble::tibble(metadata[measures_t])

    check_local_t <- logical()

    # Timepoint structure
    for (m in seq_along(measures_t))
    {
      # Get names
      measure_name <- measures_t[m]
      measure_name_clean <- measures_clean[m]

      # Create structure of result object
      check[[measure_name_clean]] <- list()

      # Get value for selected variable
      tp <- timepoints[[measure_name]]

      # Check for more than one entry
      if (any(grepl(",", tp)))
      {
        tp <- stringr::str_split_1(tp, ",")
      }

      check[[measure_name_clean]]$value <- tp
      check[[measure_name_clean]]$pass <- logical()

      # Loop across all entries of timepoint
      for (v in tp)
      {
        # Check for NAs
        if (is.na(v))
        {
          check[[measure_name_clean]]$check[["NA"]] <- NA
          next
        }

        # Create string to search for in data names
        search_string <- paste0(measure_name_clean, ".*", v, ".*")

        # Check if colums exist with this timepoint
        check[[measure_name_clean]]$check[[v]] <- any(grepl(search_string, names(data)), na.rm = TRUE)
      }

      # Save all checks
      check_local_t[m] <- all(as.logical(check[[measure_name_clean]]$check))
      check[[measure_name_clean]]$pass <- check_local_t[m]

    }

    # Get global pass
    if (any(is.na(check_local_t)))
    {
      check$passed <- FALSE
    }
    else {
      check$passed <- all(check_local_t)
    }


    # Description

    vars <- check[sapply(check, is.list)]
    test_fail <- character()

    if (check$passed == TRUE)
    {
      description <- c("All combinations of specified timepoints and variables have been found in the data")
    }
    else {
      for (m in seq_along(vars))
      {
        measure <- vars[m]
        measure_name <- names(measure)

        if (is.na(vars[[measure_name]]$pass)|vars[[measure_name]]$pass == FALSE)
        {
          failed <- measure_name
          test_fail <- c(test_fail, measure_name)
        }
      }
      test_fail <- paste0("'", test_fail, "'", collapse = ", ")
      description <- paste0("Specification for at least one timepoint of the measure(s) ",
                            test_fail, " does not fit the data")
    }

     check$description <- description
  }

  else if (data_format == "long"){
    stop(
      "Long format for data not yet implemented. In the meantime,
         please use wide format data."
    )
  }

  else {
    stop(
      "Internal error; fun_check_timepoint : data_format"
    )
  }

  return(check)

}
