# General summary ####
#' Summarize a `quality_checks` object
#'
#' @description
#' Prints an overview of all nine quality checks (pass/fail per check).
#' With `full = TRUE`, additionally prints the detailed summary of every
#' individual check by dispatching to each check's own `summary()` method.
#'
#' @param object An object of class `quality_checks`.
#' @param full Logic. Indicator if a short or a full summary should be printed.
#' The default setting is FALSE.
#' @param ... Currently unused
#'
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks <- function(object, full = FALSE, ...){

  cat("-----------------------------------------\n")
  cat("Quality Check Summary\n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n","Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Participant n", object$participant_n$passed))
  cat(sprintf("%0s %-22s %17s\n", "", "Participant ID", object$participant_id$passed))
  cat(sprintf("%0s %-22s %17s\n", "", "Phases", object$phases$passed))
  cat(sprintf("%0s %-22s %17s\n", "", "Ratings", object$ratings$passed))
  cat(sprintf("%0s %-22s %17s\n", "", "Questionnaires", object$questionnaires$passed))
  cat(sprintf("%0s %-22s %17s\n", "", "Aggregation", object$aggregation$passed))
  cat(sprintf("%0s %-22s %17s\n", "", "Transformation", object$transformation$passed))
  cat(sprintf("%0s %-22s %17s\n", "", "Timepoint", object$timepoint$passed))
  cat(sprintf("%0s %-22s %17s\n", "", "Frequency", object$frequency$passed))
  cat("\n")

  if (full == FALSE){
    cat("For more information, try 'summary(object$<check_name>)'\n")
    cat("or set 'full = TRUE'.\n")
    cat("\n")
    cat("If this is the first time using 'quality_check', you may \n")
    cat("want to look at the vignettes 'getting-started' or 'troubleshooting'\n")
    cat("using 'vignette('<vignette_name>', package = 'qualitychecks')'.")
  }
  else {
    summary(object$participant_n)
    cat("\n")
    summary(object$participant_id)
    cat("\n")
    summary(object$phases)
    cat("\n")
    summary(object$ratings)
    cat("\n")
    summary(object$questionnaires)
    cat("\n")
    summary(object$aggregation)
    cat("\n")
    summary(object$transformation)
    cat("\n")
    summary(object$timepoint)
    cat("\n")
    summary(object$frequency)
    cat("\n")
  }
}

# Summary number of participants ####
#' Summarize a `quality_checks_participant_n` object
#'
#' @description
#' Prints the result of the participant-count check: expected vs.
#' observed number of participants.
#'
#' @param object An object of class `quality_checks_participant_n`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_participant_n <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Participant_n \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n","Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Participant n", object$passed))
  cat("\n")

  # Test specifics
  cat(sprintf("%-22s %18s\n","Expected n", object$expected))
  cat(sprintf("%-22s %18s\n","Observed n", object$observed))
  cat("\n")

  # Description
  cat(object$message, "\n")
}

# Summary participant id ####
#' Summarize a `quality_checks_participant_id` object
#'
#' @description
#' Prints the result of the participant-ID check.
#'
#' @param object An object of class `quality_checks_participant_id`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_participant_id <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Participant_id \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n","Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Participant ID", object$passed))
  cat("\n")

  # Description
  cat(object$message, "\n")
}

# Summary phases ####
#' Summarize a `quality_checks_phases` object
#'
#' @description
#' Prints the result of the phase check, listing every study phase and
#' whether a matching column was found in the data.
#'
#' @param object An object of class `quality_checks_phases`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_phases <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Phases \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n","Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Phases", object$passed))
  cat("\n")

  # Test specifics
  cat(sprintf("%-22s %18s\n", "Phases", "Found in data")) # Title

  for (n in seq_along(object$phases))
  {
    cat(sprintf("%0s %-22s %17s\n",
                "",                       # Indent
                object$phases[n],         # Phases given in metadata
                object$pass_phases[n]))   # Results of test if phases found
  }
  cat("\n")

  # Description
  cat(object$message, "\n")
}

# Summary ratings ####
#' Summarize a `quality_checks_ratings` object
#'
#' @description
#' Prints the result of the ratings check, listing every rating and
#' whether a matching column was found in the data.
#'
#' @param object An object of class `quality_checks_ratings`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_ratings <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Ratings \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n","Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Ratings", object$passed))
  cat("\n")

  # Test specifics
  cat(sprintf("%-22s %18s\n", "Ratings", "Found in data")) # Title

  for (n in seq_along(object$ratings))
  {
    cat(sprintf("%0s %-22s %17s\n",
                "",                        # Indent
                object$ratings[n],         # Ratings given in metadata
                object$pass_ratings[n]))   # Results of test if rating found
  }
  cat("\n")

  # Description
  cat(object$message, "\n")
}

# Summary questionnaires ####
#' Summarize a `quality_checks_questionnaires` object
#'
#' @description
#' Prints the result of the questionnaires check, listing every
#' questionnaire and whether a matching column was found in the data.
#'
#' @param object An object of class `quality_checks_questionnaires`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_questionnaires <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Questionnaires \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n","Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Questionnaires", object$passed))
  cat("\n")

  # Test specifics
  cat(sprintf("%-22s %18s\n","Questionnaires", "Found in data")) # Title

  for (n in seq_along(object$questionnaires))
  {
    cat(sprintf("%0s %-22s %17s\n",
                "",                             # Indent
                object$questionnaires[n],       # Questionnaires given in metadata
                object$pass_questionnaires[n])) # Test results
  }
  cat("\n")

  # Descriptions
  cat(object$message, "\n")
}

# Summary aggregation ####
#' Summarize a `quality_checks_aggregation` object
#'
#' @description
#' Prints the result of the aggregation check for every rating and
#' questionnaire: its declared aggregation method, expected range, and
#' observed range.
#'
#' @param object An object of class `quality_checks_aggregation`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_aggregation <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Aggregation \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n","Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Aggregation:", object$passed))
  cat("\n")

  # Test specifics

  ## Ratings
  ratings <- object$ratings[sapply(object$ratings, is.list)] # Across all entries
                                                      # in ratings that are lists
  for (n in seq_along(ratings))
  {
    cat(sprintf("%-40s\n", names(ratings)[n])) # Rating names
    cat(sprintf("%0s %-22s %17s\n",
                "",                            # Indent
                "Passed",                      # Passing
                ratings[[n]]$pass_rat))        # Did the rating pass?
    cat(sprintf("%0s %-22s %17s\n",
                "",
                "Aggregation",                 # Aggregation
                ratings[[n]]$aggregation))     # What aggregation was entered
    cat(sprintf("%0s %-22s %17s\n",
                "",
                "Expected Range",              # Expected range
                paste0(ratings[[n]]$expected_range, collapse = " - "))) # What is the expected range
                                                                        # based on aggregation given
    cat(sprintf("%0s %-22s %17s\n",
                "",
                "Observed Range",              # Observed range
                paste0(ratings[[n]]$observed_range, collapse = " - "))) # What is the observed range
  }

  ## Questionnaires
  quest <- object$questionnaires[sapply(object$questionnaires, is.list)]
  for (n in seq_along(quest))
  {
    cat(sprintf("%-40s\n", names(quest)[n])) # Title
    cat(sprintf("%0s %-22s %17s\n",
                "",                          # Indent
                "Passed",                    # Passing
                quest[[n]]$pass_quest))      # Did the questionnaire pass?
    cat(sprintf("%0s %-22s %17s\n",
                "",
                "Aggregation",               # Aggregation
                quest[[n]]$aggregation))     # What aggregation was entered
    cat(sprintf("%0s %-22s %17s\n",
                "",
                "Expected Range",            # Expected range
                paste0(quest[[n]]$expected_range, collapse = " - "))) # What is the expected range
                                                                      # based on aggegation given
    cat(sprintf("%0s %-22s %17s\n",
                "",
                "Observed Range",            # Observed range
                paste0(quest[[n]]$observed_range, collapse = " - "))) # What is the observed range
  }
  cat("\n")

  # Description
  cat(object$ratings$description_r, "\n")
  cat(object$questionnaires$description_q, "\n")
}

# Summary transformation ####
#' Summarize a `quality_checks_transformation` object
#'
#' @description
#' Prints the result of the transformation check for every rating and
#' physiological measure: each declared transformation's expected vs.
#' observed mean, standard deviation, and range.
#'
#' @param object An object of class `quality_checks_transformation`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_transformation <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Transformation \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n", "Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Transformation:", object$passed))
  cat("\n")

  # Test specifics
  measures <- object[sapply(object, is.list)] # for all lists in the object
  for (n in seq_along(measures))
  {
    mes <- names(measures)[n]
    cat(sprintf("%-32s\n", mes))              # Measure name
    cat(sprintf("%0s %-22s %17s\n",
                "",                                           # Indent
                "Passed",                                     # Passed
                measures[[mes]]$transformations$pass_local))  # Result of test

    # Every transformation
    transformations <- object[[mes]]$transformations[sapply(object[[mes]]$transformations, is.list)]
    for (t in seq_along(transformations))
    {
      exp_obj <- measures[[mes]]$transformations[[t]]  # info on expected values
      obs_obj <- measures[[mes]]$descriptive_measures  # info on observed values
      cat(sprintf("%0s %-22s %17s\n",
                  "",
                  paste0("Transformation ", t),        # Order of transformation
                  exp_obj$name))                       # Which transformation?
      cat(sprintf("%1s %-22s %16s\n",
                  "",
                  "Mean exp.",                         # Expected mean
                  paste0(exp_obj$mean_expected, collapse = " - ")))
      cat(sprintf("%1s %-22s %16s\n",
                  "",
                  "Mean obs.",                         # Observed mean
                  paste0(round(range(obs_obj$mean, na.rm = T), 1), collapse = " - ")))
      cat(sprintf("%1s %-22s %16s\n",
                  "",
                  "SD exp.",                           # Expected standard deviation
                  paste0(exp_obj$sd_expected, collapse = " - ")))
      cat(sprintf("%1s %-22s %16s\n",
                  "",
                  "SD obs.",                           # Observed standard deviation
                  paste0(round(range(obs_obj$sd, na.rm = T), 1), collapse = " - ")))
      cat(sprintf("%1s %-22s %16s\n",
                  "",
                  "Range exp.",                        # Expected range
                  paste0(exp_obj$range_expected, collapse = " - ")))
      cat(sprintf("%1s %-22s %16s\n",
                  "",
                  "Range obs.",                        # Observed range
                  paste0(round(range(obs_obj$range, na.rm = T), 1), collapse = " - ")))
    }
  }
  cat("\n")

  # Description
  cat(object$description, "\n")
}

# Summary timepoint ####
#' Summarize a `quality_checks_timepoint` object
#'
#' @description
#' Prints the result of the timepoint check for every rating and
#' questionnaire: the specified timepoint(s) and whether each was found in
#' the data.
#'
#' @param object An object of class `quality_checks_timepoint`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_timepoint <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Timepoint \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n", "Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Timepoint:", object$passed))
  cat("\n")

  # Test specifics
  tp <- object[sapply(object, is.list)]         # All list entries
  for (n in seq_along(tp))
  {
    measure <- names(tp)[n]
    cat(sprintf("%-40s\n", measure))            # Measure name
    cat(sprintf("%0s %-22s %17s\n",
                "",                             # Indent
                "Passed",                       # Passed
                tp[[measure]]$pass))            # Test result

    for (i in seq_along(tp[[measure]]$value))   # Across all timepoints provided
    {
      tp_name <- tp[[measure]]$value[i]
      cat(sprintf("%0s %-22s %17s\n",
                  "",
                  paste0("Timepoints ", tp[[measure]]$value[i], " found"), # Which timepoint was provided?
                  tp[[measure]]$check[[tp_name]]))                         # Test result
    }
  }
  cat("\n")

  # Description
  cat(object$description, "\n")
}

# Summary frequency ####
#' Summarize a `quality_checks_frequency` object
#'
#' @description
#' Prints the result of the frequency check for every rating and
#' questionnaire: the specified measurement frequency and the expected vs.
#' observed number of columns.
#'
#' @param object An object of class `quality_checks_frequency`.
#' @param ... Currently unused
#' @return Called for its side effect of printing to the console; returns
#'   `NULL` invisibly.
#'
#' @export
summary.quality_checks_frequency <- function(object, ...){

  # Head
  cat("-----------------------------------------\n")
  cat("Quality Check Frequency \n")
  cat("-----------------------------------------\n")
  cat(sprintf("%-22s %18s\n", "Check", "Passed"))
  cat(sprintf("%0s %-22s %17s\n", "", "Frequency:", object$passed))
  cat("\n")

  # Test specifics
  freq <- object[sapply(object, is.list)]  # Across all lists in the object
  for (n in seq_along(freq))
  {
    measure <- names(freq)[n]
    cat(sprintf("%-40s\n", measure))       # Measure name
    cat(sprintf("%0s %-22s %17s\n",
                "",                        # Indent
                "Passed",                  # Passed
                freq[[measure]]$pass))     # Test result
    cat(sprintf("%0s %-22s %17s\n",
                "",
                "Frequency",               # Frequency
                paste0(substr(freq[[measure]]$value, 1, 5), "."))) # Entry given in metadata

    for (i in seq_along(freq[[measure]]$value))  # for all frequency enties
    {
      cat(sprintf("%0s %-22s %17s\n",
                  "",
                  "n columns exp.",              # expected number of entries
                  freq[[measure]]$n_col_exp))
      cat(sprintf("%0s %-22s %17s\n",
                  "",
                  "n columns obs.",              # observed number of entries
                  freq[[measure]]$n_col_obs))
    }
  }
  cat("\n")

  # Description
  cat(object$description, "\n")
}
