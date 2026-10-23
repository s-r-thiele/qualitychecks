#' Check whether the number of observed columns matches the specified
#' measurement frequency
#'
#' @description
#' For every rating and questionnaire declared in `metadata`, determines
#' how many columns are expected in `data` based on the declared
#' measurement frequency (`experimentwise`, `daywise`, `phasewise`,
#' `trialwise`; `blockwise` is not yet implemented), and compares that to
#' the number of columns actually observed.
#'
#' The expected number of columns also depends on how many timepoints were
#' specified for a given measure. This is determined directly from
#' `metadata` here (the `<measure>_timepoint` column), duplicating the
#' equivalent logic in `fun_check_timepoint()` on purpose: keeping the two
#' checks independent of one another means `fun_check_frequency()` can be
#' understood, tested, and called on its own, without needing the result
#' of `fun_check_timepoint()` as an input.
#'
#' @note
#' `pmeasure_names` is accepted but currently unused: physiological
#' measures are not yet included in the frequency check. Known
#' limitation, not addressed here.
#'
#' If a questionnaire has no declared aggregation (relying on its item
#' count to determine the expected number of columns) but has no matching
#' row in `info_measures$Abbreviation`, that item count cannot be
#' determined. This is reported as a failure (`pass = FALSE`), so it
#' reliably surfaces in `passed` rather than being silently missed - a
#' `warning()` is raised and an explanatory `error` message is recorded on
#' its entry in the result, distinguishing this case ("we're missing
#' reference data") from a genuine data problem in the study export. All
#' other measures are still evaluated normally. Note that a questionnaire
#' *with* a declared aggregation (e.g. `"sum"`) never needs `info_measures`
#' for this particular check: once aggregated to a single score, exactly
#' one column is expected regardless of the item count, so a missing
#' `info_measures` entry has no effect here (it still matters for
#' `fun_check_aggregation()`, which needs the item count to compute the
#' expected value range).
#'
#' @param data A data frame containing the measurement data.
#' @param metadata A data frame of study metadata, containing
#'   `<measure>_frequency`, `<measure>_timepoint`, and (for questionnaires)
#'   `<measure>_aggregation` columns for every rating/questionnaire
#'   administered.
#' @param study_design A data frame describing the study's phases and
#'   trial/timing structure (`timeToNext`, `timeToNextUnit`, `cspTrials`,
#'   `csmTrials`, `gsTrials`), used for `daywise`/`phasewise`/`trialwise`
#'   frequencies.
#' @param rating_names Character vector of rating abbreviations, as
#'   returned by `fun_which_ratings()` (may still carry a `"us_"` prefix).
#' @param rating_names_fix Character vector of rating abbreviations with
#'   any `"us_"` prefix stripped, as returned by `fix_rating_names()`.
#' @param quest_names Character vector of questionnaire abbreviations, as
#'   returned by `fun_which_questionnaires()`.
#' @param pmeasure_names Character vector of physiological-measure
#'   abbreviations. Currently unused (see Note).
#' @param info_measures A data frame of reference values (`Abbreviation`,
#'   `n_items`, ...) for questionnaires, used to determine the number of
#'   items when a questionnaire's aggregation is unspecified.
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with a top-level `passed` (logical) and `description`
#'   (character), plus one named entry per rating/questionnaire (named by
#'   its abbreviation), each a list with `value` (the specified
#'   frequency), `pass` (logical), `n_col_exp` (expected column count),
#'   and `n_col_obs` (observed column count).
#' @keywords internal
#' @noRd
fun_check_frequency <- function(data, metadata, study_design,
                                rating_names, rating_names_fix,
                                quest_names, pmeasure_names,
                                info_measures, data_format){

  if (data_format == "wide"){

    # Create structure of results object

    check <- list()

    # Structure frequency
    check$passed <- logical()

    check$description <- character()

    # Create variable with relevant measures
    measures_clean <- c(rating_names_fix, quest_names)

    if (length(rating_names) == 0 | length(rating_names_fix) == 0){
      ratings <- character(0)
    }
    else {
      ratings <- paste0("rating_", rating_names)
    }

    if (length(quest_names) == 0){
      questionnaires <- character(0)
    }
    else{
      questionnaires <- paste0("quest_", quest_names)
    }

    measures <- c(ratings, questionnaires)

    measures_f <- paste0(measures, "_frequency")

    # Timepoint metadata columns for the same measures (same naming
    # scheme as in fun_check_timepoint()), used below to determine how
    # many timepoints were specified for each measure.
    measures_t <- paste0(measures, "_timepoint")

    # Get frequency info from metadata
    frequencies <- tibble::tibble(metadata[measures_f])

    check_local_f <- logical()

    # Frequency structure
    for (m in seq_along(measures_f))
    {
      # Get names
      measure_name <- measures_f[m]
      measure_name_clean <- measures_clean[m]

      # Get info on measure
      if (measure_name_clean %in% rating_names)
      {
        rating_stim <- "cs"
      }
      else if (measure_name_clean %in% rating_names_fix)
      {
        rating_stim <- "us"
      }
      else if (measure_name_clean %in% quest_names)
      {
        rating_stim <- "quest"
      }
      else {
       rating_stim <- "other"
      }

      # Create structure of result object
      check[[measure_name_clean]] <- list()

      # Get value for selected variable
      freq <- frequencies[[measure_name]]

      check[[measure_name_clean]]$value <- freq
      check[[measure_name_clean]]$pass <- logical()

      # Get amount of timepoints specified for this measure
      # (duplicates the equivalent step in fun_check_timepoint() by
      # design; see the roxygen description above)
      tp <- metadata[[measures_t[m]]]

      if (any(grepl(",", tp)))
      {
        tp <- stringr::str_split_1(tp, ",")
      }

      n_timepoint <- length(tp)

      # Get amount of unique stimuli

      # Baseline
      n_us = 1
      n_csp = 1
      n_csm = 1
      n_gs = 1
      n_items = 1

      # Specific changes
      if (rating_stim == "us")
      {
        n_us <- metadata$us_num
      }
      else if (rating_stim == "cs")
      {
        n_csp <- metadata$csp_num
        n_csm <- metadata$csm_num
        n_gs <- metadata$gs_num
      }
      else if (rating_stim == "quest")
      {
        search_name <- paste0("quest_", measure_name_clean, "_aggregation")
        agg <- metadata[grepl(search_name, names(metadata))]

        if (is.na(agg) == TRUE)
        {
          # Without a declared aggregation, the number of items is needed
          # to determine how many columns to expect - but that requires
          # reference data for this questionnaire in info_measures. If
          # it's missing, this questionnaire cannot be evaluated: counted
          # as a FAILURE (not silently skipped), so the overall check
          # reliably surfaces the gap, and warn clearly instead of
          # silently producing a wrong result (n_col_exp would become
          # numeric(0)).
          if (!(measure_name_clean %in% info_measures$Abbreviation))
          {
            check[[measure_name_clean]]$error <- paste0(
              "No reference data (number of items) found in info_measures ",
              "for questionnaire '", measure_name_clean, "'. This ",
              "questionnaire is reported as failed until reference data ",
              "is added. Please contact the Fearbase team to have it ",
              "added to info_measures."
            )
            check[[measure_name_clean]]$n_col_exp <- NA
            check[[measure_name_clean]]$n_col_obs <- length(names(data)[grepl(paste0(measure_name_clean, ".*", freq, ".*"), names(data))])
            check[[measure_name_clean]]$pass <- FALSE
            check_local_f[m] <- FALSE

            warning(check[[measure_name_clean]]$error, call. = FALSE)

            next
          }

          n_items = info_measures$n_items[info_measures$Abbreviation == measure_name_clean]
        }
      }

      # Get amount of expected columns

      # Warning and test fail if no entry is found for frequency
      if (is.na(freq))
      {
        freq_na_message <- paste0("No entry for the frequency for the measure '",
                                  measure_name_clean, "' in the metadata. This measure ",
                                  "is reported as failed until the information ",
                                  "is provided.")

        check[[measure_name_clean]]$error <- freq_na_message
        check[[measure_name_clean]]$n_col_exp <- NA
        check[[measure_name_clean]]$n_col_obs <- NA

        check_local_f[m] <- FALSE
        check[[measure_name_clean]]$pass <- check_local_f[m]

        warning(freq_na_message, call. = FALSE)

        next
      }

      else if (freq == "experimentwise")
      {
        n_base = 1
      }
      else if (freq == "daywise")
      {
        n_days <- sum(study_design$timeToNext[study_design$timeToNextUnit == "days"])
        n_base = 1*n_days
      }
      else if (freq == "phasewise")
      {
        n_phases = nrow(study_design) # Does not account for "other" or "intervention"

        if (rating_stim == "cs")
        {
          n_stimuli = sum(n_csp, n_csm, n_gs, na.rm = TRUE)
        }
        else {
          n_stimuli = 1
        }

        n_base = 1*n_phases*n_stimuli
      }
      else if (freq == "blockwise")
      {

        blockwise_message <- paste0("As of now, blockwise frequency is not ",
                                    "supported. The measure '", measure_name_clean,
                                    "' will be skipped and reported as failed ",
                                    "until further notice.")

        check[[measure_name_clean]]$error <- blockwise_message
        check[[measure_name_clean]]$n_col_exp <- NA
        check[[measure_name_clean]]$n_col_obs <- NA

        check_local_f[m] <- FALSE
        check[[measure_name_clean]]$pass <- check_local_f[m]

        warning(blockwise_message, call. = FALSE)

        next
        # n_base = 1*n_blocks
      }
      else if (freq == "trialwise")
      {
        # Sum all trials of stimuli
        n_trials_csp = sum(study_design$cspTrials, na.rm = TRUE)*n_csp*n_us
        n_trials_csm = sum(study_design$csmTrials, na.rm = TRUE)*n_csm*n_us
        n_trials_gs = sum(study_design$gsTrials, na.rm = TRUE)*n_gs*n_us

        n_trials = sum(c(n_trials_csp, n_trials_csm, n_trials_gs), na.rm = TRUE)

        n_base = 1*n_trials
      }

      # Get amount of expected columns
      n_col_exp = n_base*n_timepoint*n_items
      check[[measure_name_clean]]$n_col_exp <- n_col_exp

      # Create string to search for in data names
      search_string <- paste0(measure_name_clean, ".*", freq, ".*")

      # Check amount of observed columns
      n_col_obs <- length(names(data)[grepl(search_string, names(data))])
      check[[measure_name_clean]]$n_col_obs <- n_col_obs

      # Check if number of expected and observed columns match
      check_local_f[m] <- n_col_exp == n_col_obs
      check[[measure_name_clean]]$pass <- check_local_f[m]
    }

    # Get global pass
    check$passed <- all(check_local_f)

    # Description

    vars <- check[sapply(check, is.list)]
    test_fail <- character()

    if(check$passed == TRUE)
    {
      messg = c("Expected number of columns for each measurement based on the specification for frequency match the respective number of observed columns of the measures in the data.")
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
      messg = paste0("The expected number of columns based on the specification for frequency does not match the observed number of columns for the measure(s) ", test_fail, ".")
    }

    # Create description with dynamic line breaks
    description = paste(strwrap(messg, width = 41), collapse = "\n")

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
      "Internal error; fun_check_frequency : data_format"
    )
  }

  return(check)

}
