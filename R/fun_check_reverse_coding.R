#' Check reverse-coded questionnaire items against expected distributions
#' (experimental, not yet integrated)
#'
#' @description
#' For every questionnaire with reverse-coded items that is not
#' aggregated, generates a density plot per reverse-coded item comparing
#' the observed distribution to an expected skew-normal distribution
#' (using item-level reference statistics read from
#' `dependencies/<questionnaire>_item_info.CSV`).
#'
#' @section Development status:
#' This function is **not yet part of the `quality_checks()` pipeline**
#' (it is not called by `run_all_checks()`) and is kept in an active
#' development state: unlike the other `fun_check_*()` functions, it does
#' not yet return a `passed`/`message` structure at the top level, and it
#' reads its per-item reference data from a hardcoded relative path
#' (`dependencies/`) rather than through the `dependencies_path` mechanism
#' used elsewhere in this package. Do not rely on its output structure
#' remaining stable.
#'
#' @param data A data frame containing the measurement data.
#' @param metadata A data frame of study metadata, containing
#'   `<measure>_aggregation` columns.
#' @param quest_names Character vector of questionnaire abbreviations, as
#'   returned by `fun_which_questionnaires()`.
#' @param info_measures A data frame of reference values (`Abbreviation`,
#'   `range_min`, `range_max`) used to set plot axis limits.
#' @param info_reverse_coding A data frame with `questionnaire`,
#'   `reverse_coding` (`"yes"`/other), and `items` (list-column of
#'   reverse-coded item numbers) columns.
#' @param data_format Character. Currently only `"wide"` is implemented.
#'
#' @return A named list (by questionnaire abbreviation, only for
#'   questionnaires with unaggregated reverse-coded items), each holding
#'   one `ggplot` object per reverse-coded item (named by column name).
#' @keywords internal
#' @noRd
fun_check_reverse_coding <- function(data, metadata,
                                     quest_names,
                                     info_measures,
                                     info_reverse_coding,
                                     data_format)
{
  if (data_format == "wide"){

    # Create structure of results object
    check <- list()

    for (q in seq_along(quest_names))
    {
      # Select current questionnaire
      quest <- quest_names[q]

      # Check if the questionnaire is aggregated
      quest_name <- paste0("quest_", quest, "_aggregation")
      entry <- metadata[grepl(quest_name, names(metadata))][[1]]
      has_aggregation <- ifelse(is.na(entry) == TRUE, FALSE, TRUE)

      # Check if the questionnaire has reverse coded items
      has_revit <- ifelse(info_reverse_coding$reverse_coding[info_reverse_coding$questionnaire == quest] == "yes", TRUE, FALSE)

      # Get range of selected questionnaire
      info_measure <- info_measures[info_measures$Abbreviation == quest,]

      if (has_revit == TRUE & has_aggregation == FALSE)
      {

        # Create structure of results object
        check[[quest]] <- list()

        # Get all items of the selected questionnaire
        items <- data[grepl(quest, names(data))]

        # Get info which items are reverse coded
        rev_it <- info_reverse_coding$items[info_reverse_coding$questionnaire == quest][[1]]

        # Search for only the reverse coded items
        search_string <- character()

        for (i in seq_along(rev_it))
        {
          item_number <- rev_it[i]
          search_string[i] <- paste0(quest, ".*", "t", item_number, "_")
        }

        combined_search <- paste(search_string, collapse = "|")

        # Select only the reverse coded items
        items_rev <- items[grepl(combined_search, names(items))]

        # Get the descriptive validation information for all items of the selected questionnaire
        quest_item_info <- utils::read.csv2(paste0("dependencies/", quest, "_item_info.CSV"))

        quest_item_info <- quest_item_info |>
          dplyr::mutate(dplyr::across(tidyselect::everything(), as.numeric))

        # Loop across all reverse coded items
        for (n in seq_along(items_rev))
        {
          # Select item
          item_obs <- items_rev[n]

          # Get item name
          item_name <- names(item_obs)

          # Get the number of the respective item
          n_item <- as.numeric(stringr::str_extract(names(item_obs), "(?<=_t)\\d+"))

          # Select the respective descriptive validation data
          item_exp <- quest_item_info[n_item,]

          # Create visualization

          plot <- ggplot2::ggplot(data = items, ggplot2::aes(x = rlang::.data[[item_name]])) +

            # Observed data density
            ggplot2::geom_density(ggplot2::aes(color = "Observed"))+

            # Expected data density
            ggplot2::stat_function(
              fun = sn::dsn, # In most cases, skewness is also reported so
                             # this information is integrated
              args = list(
                xi = item_exp$mean,             # Mean
                omega = item_exp$sd,            # Standard deviation
                alpha = item_exp$skewness),     # Skewness
              ggplot2::aes(color = "Expected"))+

            # Legend
            ggplot2::scale_color_manual(
              name = NULL,
              values = c(
                "Observed" = "blue",
                "Expected" = "red"))+

            # Set x limits of the plot based on the range of the questionnaire
            ggplot2::xlim(info_measure$range_min-1,
                 info_measure$range_max+1)

          # Save the plot
          check[[quest]][[item_name]] <- plot

        }
      }
    }
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
