#' Check whether the observed value range fits the specified aggregation
#'
#' @description
#' For every rating and questionnaire, determines the expected value range
#' based on its declared aggregation method (`"None"`/`"avrg"` use the
#' scale range as-is; `"sum"` - questionnaires only - multiplies the item
#' range by the number of items) and compares it to the range actually
#' observed in `data`. Aggregation methods other than these are not yet
#' implemented and raise an error.
#'
#' @note
#' If a questionnaire is administered (present in `quest_names`) but has
#' no matching row in `info_measures$Abbreviation`, its expected range
#' cannot be determined. This is reported as a failure (`pass_quest =
#' FALSE`), so it reliably surfaces in `passed`/`passed_q` rather than
#' being silently missed - a `warning()` is raised and an explanatory
#' `error` message is recorded on its entry in the result, distinguishing
#' this case ("we're missing reference data") from a genuine data problem
#' in the study export. All other questionnaires are still evaluated
#' normally.
#'
#' @param data A data frame containing the measurement data.
#' @param metadata A data frame of study metadata, containing
#'   `<measure>_aggregation` columns for every rating/questionnaire, and
#'   `<rating>_min_scale`/`<rating>_max_scale` columns for every rating.
#' @param rating_names Character vector of rating abbreviations, as
#'   returned by `fun_which_ratings()` (may still carry a `"us_"` prefix).
#'   Currently unused directly (see `rating_names_fix`), kept for
#'   signature consistency with the other `fun_check_*()` functions.
#' @param rating_names_fix Character vector of rating abbreviations with
#'   any `"us_"` prefix stripped, as returned by `fix_rating_names()`.
#' @param quest_names Character vector of questionnaire abbreviations, as
#'   returned by `fun_which_questionnaires()`.
#' @param info_measures A data frame of reference values (`Abbreviation`,
#'   `range_min`, `range_max`, `n_items`) used to determine the expected
#'   range for questionnaires.
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with a top-level `passed` (logical) and `description`
#'   (character), plus `ratings` and `questionnaires` sublists. Each
#'   sublist has its own `passed_r`/`passed_q` and `description_r`/
#'   `description_q`, plus one named entry per rating/questionnaire (named
#'   by its abbreviation) with `aggregation`, `expected_range`,
#'   `observed_range`, and `pass_rat`/`pass_quest`.
#' @keywords internal
#' @noRd
fun_check_aggregation <- function(data, metadata, rating_names, rating_names_fix,
                                  quest_names, info_measures, data_format){

  # Wide Format ####

  if (data_format == "wide"){

    # Ratings
    # Create List structure

    ratings <- list()

    ratings$passed_r <- logical()

    ratings$description_r <- character()

    pass_r <- logical()

    # Loop over all ratings

    for (r in seq_along(rating_names_fix)){

      rating <- rating_names_fix[r]

      rating_var_name <- paste0("rating_", rating)

      ratings[[rating]] <- list()

      # Check aggregation specification

      if (is.na(metadata[grepl(paste0(rating, ".*", "_aggregation"), names(metadata))][[1]]) == TRUE){

        ratings[[rating]]$aggregation <- "None"

      }

      else {

        ratings[[rating]]$aggregation <- metadata[grepl(paste0(rating, ".*", "_aggregation"), names(metadata))]

      }

      # Select expected range

      # None or Average

      if (ratings[[rating]]$aggregation %in% c("None", "avrg")){

        ratings[[rating]]$expected_range <- c(
          metadata[grepl(paste0(rating, ".*", "_min_scale"), names(metadata))][[1]],
          metadata[grepl(paste0(rating, ".*", "_max_scale"), names(metadata))][[1]]
        )

      }

      # # Sum
      #
      # else if (ratings[[rating]]$aggregation == "sum"){
      #
      #   ratings[[rating]]$expected_range <- c(
      #     metadata[[paste0(rating_var_name, "_min_scale")]],
      #     metadata[[paste0(rating_var_name, "_max_scale")]]
      #   )*number_of_ratings # Currently unknown/hard to calculate
      #
      # }

      # Failsafe

      else {
        stop(
          "An aggregation method other than average ('avrg') has not been
            implemented yet for ratings."
        )
      }

      # Get observed range

      index_rating_data <- which(grepl(rating_names_fix[r], names(data)))

      # Check for missing column

      if (length(names(data)[grepl(rating, names(data))]) == 0)
      {
        ratings[[rating]]$observed_range <- as.numeric(c(0,0))
        ratings[[rating]]$error <- c("The provided variable cannot be found in data")
        ratings[[rating]]$pass_rat <- FALSE
        pass_r[r] <- FALSE
      }

      else{
        ratings[[rating]]$observed_range <- range(data[,index_rating_data], na.rm = T)
      }

      # Pass conditions
      if (length(names(data)[grepl(rating, names(data))]) == 0)
      {
        next
      }

      else if (any(is.na(ratings[[rating]]$expected_range)))
      {
        na_expected_range_message <- paste0("At least one entry is missing in the metadata for the expected range for ", rating, ". Please fix the issue and run the function again.")
        stop(na_expected_range_message)
      }

      else if ((min(ratings[[rating]]$observed_range) >=
           min(ratings[[rating]]$expected_range)) &
          (max(ratings[[rating]]$observed_range) <=
           max(ratings[[rating]]$expected_range))){

        ratings[[rating]]$pass_rat <- TRUE
        pass_r[r] <- TRUE
      }

      else {

        ratings[[rating]]$pass_rat <- FALSE
        pass_r[r] <- FALSE
      }

    }

    # Description

    if (all(pass_r) == TRUE){
      description_r = "Specification of all rating aggregations fits the data."
    }

    else if (any(pass_r) == TRUE){
      description_r = paste0("Specification of ",sum(pass_r), " of ", length(pass_r),
                           " rating aggregations fit the data.")
    }
    else {
      description_r = "Specification of all rating aggregations does not fit the data."
    }

    # Ratings pass and description

    ratings$passed_r <- all(pass_r)
    ratings$description_r <- description_r


    # Questionnaires
    # Create list structure

    questionnaires <- list()

    questionnaires$passed_q <- logical()

    questionnaires$description_q <- character()

    pass_q <- logical()

    # Loop over all questionnaires

    for (q in seq_along(quest_names)){

      questionnaire <- quest_names[q]

      quest_var_name <- paste0("quest_", questionnaire)

      questionnaires[[questionnaire]] <- list()

      # Check whether reference data (range/n_items) exists for this
      # questionnaire at all. If not, this questionnaire cannot be
      # evaluated - counted as a FAILURE (not silently skipped), so the
      # overall check reliably surfaces the gap, and warn clearly instead
      # of silently producing a wrong result (e.g. from min()/max() on an
      # empty vector).
      if (!(questionnaire %in% info_measures$Abbreviation)){

        questionnaires[[questionnaire]]$error <- paste0(
          "No reference data (range and number of items) found in ",
          "info_measures for questionnaire '", questionnaire, "'. This ",
          "questionnaire is reported as failed until reference data is ",
          "added. Please contact the Fearbase team to have it added to ",
          "info_measures."
        )
        questionnaires[[questionnaire]]$pass_quest <- FALSE
        pass_q[q] <- FALSE

        warning(questionnaires[[questionnaire]]$error, call. = FALSE)

        next
      }

      # Check aggregation specification

      if (is.na(metadata[[paste0(quest_var_name, "_aggregation")]]) == TRUE){

        questionnaires[[questionnaire]]$aggregation <- "None"

      }

      else {

        questionnaires[[questionnaire]]$aggregation <- metadata[[paste0(quest_var_name, "_aggregation")]]

      }

      # Select expected range

      # None or Average

      if (questionnaires[[questionnaire]]$aggregation %in% c("None", "avrg")){

        questionnaires[[questionnaire]]$expected_range <- c(
          info_measures$range_min[info_measures$Abbreviation == questionnaire],
          info_measures$range_max[info_measures$Abbreviation == questionnaire]
        )

      }

      # Sum

      else if (questionnaires[[questionnaire]]$aggregation == "sum"){

        questionnaires[[questionnaire]]$expected_range <- c(
          info_measures$range_min[info_measures$Abbreviation == questionnaire],
          info_measures$range_max[info_measures$Abbreviation == questionnaire]
        ) * info_measures$n_items[info_measures$Abbreviation == questionnaire]

      }

      # Failsafe

      else {
        stop(
          "Aggregation methods other than average ('avrg') and sum ('sum')
               have not been implemented yet for questionnaires."
        )
      }

      # Get observed range

      index_quest_data <- which(grepl(quest_names[q], names(data)))

      # Check for missing column

      if (length(names(data)[grepl(questionnaire, names(data))]) == 0)
      {
        questionnaires[[questionnaire]]$observed_range <- as.numeric(c(0,0))
        questionnaires[[questionnaire]]$error <- c("The provided variable cannot be found in data")
        questionnaires[[questionnaire]]$pass_quest <- FALSE
        pass_q[q] <- FALSE
      }
      else {
        questionnaires[[questionnaire]]$observed_range <- range(data[,index_quest_data], na.rm = T)
      }

      # Pass conditions

      if (length(names(data)[grepl(questionnaire, names(data))]) == 0)
      {
        next
      }

      else if ((min(questionnaires[[questionnaire]]$observed_range) >=
          min(questionnaires[[questionnaire]]$expected_range)) &
          (max(questionnaires[[questionnaire]]$observed_range) <=
          max(questionnaires[[questionnaire]]$expected_range))){

        questionnaires[[questionnaire]]$pass_quest <- TRUE
        pass_q[q] <- TRUE
      }

      else {

        questionnaires[[questionnaire]]$pass_quest <- FALSE
        pass_q[q] <- FALSE
      }

    }

    # Description

    if (all(pass_q) == TRUE){
      description_q = "Specification of questionnaire aggregation fits the data."
    }

    else if (any(pass_q) == TRUE){
      description_q = paste0("Specification of ",sum(pass_q), " of ", length(pass_q),
      " questionnaire aggregations fit the data.")
    }
    else {
      description_q = "Specification of all questionnaire aggregation does not fit the data."
    }

    # Questionnaire pass and description

    questionnaires$passed_q <- all(pass_q)
    questionnaires$description_q <- description_q

    if ((ratings$passed_r && questionnaires$passed_q) == TRUE){

      description <- "Aggregation for ratings and questionnaires fit the data."
    }

    else if ((ratings$passed_r) == TRUE & (questionnaires$passed_q) == FALSE){
      description <- "Aggregation for ratings fit the data but aggregation for at least one questionnaire does not fit the data."
    }

    else if ((ratings$passed_r) == FALSE & (questionnaires$passed_q) == TRUE){
      description <- "Aggregation for questionnaires fit the data but aggregation for at least one rating does not fit the data."
    }
    else {
      description <- "Aggregation for at least one rating and questionnaire does not fit the data."
    }

    # Results object

    check <- list(
      passed = ratings$passed_r && questionnaires$passed_q,
      description = description,
      ratings = ratings,
      questionnaires = questionnaires
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
      "Internal error; fun_check_aggregation : data_format"
    )
  }

  return(check)

}
