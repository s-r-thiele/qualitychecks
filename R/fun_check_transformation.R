#' Check whether observed descriptive statistics fit the specified
#' transformation(s)
#'
#' @description
#' For every rating and physiological measure, looks up the transformation
#' method(s) declared in `metadata` (comma-separated if more than one) and
#' compares each transformation's expected range/mean/sd (from
#' `info_transformation`) against the observed range/mean/sd of the
#' matching columns in `data`.
#'
#' @param data A data frame containing the measurement data.
#' @param metadata A data frame of study metadata, containing a
#'   `<measure>_transformation` column for every rating/physiological
#'   measure administered.
#' @param rating_names Character vector of rating abbreviations, as
#'   returned by `fun_which_ratings()`. Currently unused directly (see
#'   `rating_names_fix`), kept for signature consistency with the other
#'   `fun_check_*()` functions.
#' @param rating_names_fix Character vector of rating abbreviations with
#'   any `"us_"` prefix stripped, as returned by `fix_rating_names()`.
#' @param pmeasure_names Character vector of physiological-measure
#'   abbreviations, as returned by `fun_which_pmeasures()`.
#' @param info_transformation A data frame of reference values with a
#'   `transformation` column (matching the transformation names used in
#'   `metadata`, e.g. `"None"`, `"log"`, `"range"`, `"sqrt"`, `"z"`,
#'   `"t"`) and list-columns `range`, `mean`, `sd`, each holding a
#'   `c(min, max)` vector of plausible values for that transformation.
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A list with a top-level `passed` (logical) and `description`
#'   (character), plus one named entry per rating/physiological measure
#'   (named by its abbreviation), each a list with `transformations` (one
#'   numbered entry per declared transformation, plus `pass_local`),
#'   `descriptive_measures` (observed `mean`/`sd`/`range` per column), and
#'   `pass_global`.
#' @keywords internal
#' @noRd
fun_check_transformation <- function(data, metadata,
                                     rating_names, rating_names_fix,
                                     pmeasure_names, info_transformation,
                                     data_format){

  # Function to test agreement of observed and expected values
  test_agree <- function(obs, exp)
  {
    if (all(is.na(obs))==TRUE)
    {
      obs_range <- as.numeric(c(NA,NA))
      FALSE
    }

    else {
      obs_range <- range(obs, na.rm = TRUE)

      obs_range[1] >= exp[1] & obs_range[2] <= exp[2]
    }
  }

  # Lookup table for expected transformations

  trans_lookup <- info_transformation |>
    dplyr::select("transformation", "range", "mean", "sd") |>
    tibble::column_to_rownames("transformation")

  if (data_format == "wide"){

    # Combining all variables where transformation is allowed
    measure_names <- c(rating_names_fix, pmeasure_names)

    # Add suffix for search in metadata
    transformation_names <- paste0(measure_names, "_transformation")

    # Create structure of results object
    variables <- list()

    variables$passed <- logical()

    variables$description <- character()

    # Check variables
    # pass_d <- logical()

    pass_l <- logical()

    pass_g <- logical()

    # Loop across all variables with transformations
    for (t in seq_along(measure_names))
    {
      # Variable for current measure
      variable_name <- measure_names[t]

      # Check variable

      pass_d <- logical()

      # Look up entries for the transformation
      transformations <- metadata[[names(metadata)[grepl(transformation_names[t], names(metadata))]]]

      # Structure of results object
      variables[[variable_name]] <- list()

      variables[[variable_name]]$transformations <- list()

      variables[[variable_name]]$descriptive_measures$mean <- list()
      variables[[variable_name]]$descriptive_measures$sd <- list()
      variables[[variable_name]]$descriptive_measures$range <- list()

      # Check for missing columns
      if (length(names(data)[grepl(variable_name, names(data))]) == 0)
      {
        variables[[variable_name]]$descriptive_measures$mean <- NA
        variables[[variable_name]]$descriptive_measures$sd <- NA
        variables[[variable_name]]$descriptive_measures$range <- NA
      }

      else{
        # Get observed data for mean, standard deviation and range
        for (var in names(data)[grepl(variable_name, names(data))])
        {

          # Check for columns of missing data
          if (all(is.na(data[[var]])))
          {
            variables[[variable_name]]$descriptive_measures$mean[[var]] <- NA
            variables[[variable_name]]$descriptive_measures$sd[[var]] <- NA
            variables[[variable_name]]$descriptive_measures$range[[var]] <- NA
          }
          else {
            variables[[variable_name]]$descriptive_measures$mean[[var]] <- mean(data[[var]], na.rm = T)
            variables[[variable_name]]$descriptive_measures$sd[[var]] <- stats::sd(data[[var]], na.rm = T)
            variables[[variable_name]]$descriptive_measures$range[[var]] <- range(data[[var]], na.rm = T)
          }
        }

      }

      # Loop across all entries of transformation for the current measure
      for (transformation in seq_along(transformations))
      {
        # Select the specific entry
        trans_name <- transformations[[transformation]]

        # Split string if the entry consists of more than one transformation
        if (grepl(",", trans_name) == T){
          trans_name <- stringr::str_split_1(trans_name, ",")
        }

        # Loop across all transformations for the specific entry
        for (n in seq_along(trans_name))
        {
          # Structure of results object
          variables[[variable_name]]$transformations[[n]] <- list()

          variables[[variable_name]]$transformations[[n]]$pass_r <- logical()
          variables[[variable_name]]$transformations[[n]]$pass_m <- logical()
          variables[[variable_name]]$transformations[[n]]$pass_sd <- logical()

          # Name entries according to their order
          names(variables[[variable_name]]$transformations)[n] <- as.character(n)

          # Set value of the transformation name
          if (any(is.na(trans_name)))
          {
            value_transformation <- "None"
          }

          else {
            value_transformation <- trans_name[[n]]
          }

          # Save name of transformation in results object
          variables[[variable_name]]$transformations[[n]]$name <- value_transformation

          # Select specific row of transformation
          expected <- trans_lookup[value_transformation, ]

          # Assign expected range, mean and sd for transformation
          variables[[variable_name]]$transformations[[n]]$range_expected <- expected$range[[1]]
          variables[[variable_name]]$transformations[[n]]$mean_expected <- expected$mean[[1]]
          variables[[variable_name]]$transformations[[n]]$sd_expected <- expected$sd[[1]]

          # Test agreement of observed and expected data

          trans <- variables[[variable_name]]$transformations[[n]]

          desc <- variables[[variable_name]]$descriptive_measures

          trans$pass_r <- test_agree(desc$range, trans$range_expected)
          trans$pass_m <- test_agree(desc$mean, trans$mean_expected)
          trans$pass_sd <- test_agree(desc$sd, trans$sd_expected)

          variables[[variable_name]]$transformations[[n]] <- trans

          # Check descriptive agreement
          if ((variables[[variable_name]]$transformations[[n]]$pass_r &
               variables[[variable_name]]$transformations[[n]]$pass_m &
               variables[[variable_name]]$transformations[[n]]$pass_sd) == TRUE)
          {
            pass_d[n] <- TRUE
          }

          else {
            pass_d[n] <- FALSE
          }

        }

        # Save agreement for selected transformation
        variables[[variable_name]]$transformations$pass_local <- all(pass_d)

      }

      # Check agreement of all transformations of one variable
      pass_l <- variables[[variable_name]]$transformations$pass_local

      # Save agreement for all transformations of one variable
      variables[[variable_name]]$pass_global <- all(pass_l)

    }

    # Check agreement of all transformations
    vars <- variables[sapply(variables, is.list)]
    for (m in seq_along(vars))
    {
      measure <- vars[m]
      measure_name <- names(measure)
      pass_g[m] <- vars[[measure_name]]$pass_global
    }

    # Save agreement of all transformations
    variables$passed <- all(pass_g)

    # Create description
    if (variables$passed == TRUE)
    {
      description <- c("Specification of all transformations for all measures fits the data")
    }
    else {
      test_fail <- character()
      for (m in seq_along(vars))
      {
        measure <- vars[m]
        measure_name <- names(measure)
        if (vars[[measure_name]]$pass_global == FALSE)
        {
          failed <- measure_name
          test_fail <- c(test_fail, measure_name)
        }
      }
      test_fail <- paste0("'", test_fail, "'", collapse = ", ")
      description <- paste0("Specification for at least one transformation of the measure(s) ",
                            test_fail, " does not fit the data")
    }

    variables$description <- description

  }

  else if (data_format == "long")
  {
    stop(
      "Long format for data not yet implemented. In the meantime,
         please use wide format data."
    )
  }

  else {
    stop(
      "Internal error; fun_check_transformation : data_format"
    )
  }

  return(variables)

}
