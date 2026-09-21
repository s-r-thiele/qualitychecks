#' Determine which questionnaires are present in the metadata
#'
#' @description
#' Cross-references the `"quest_*"` indicator columns present in
#' `metadata` against the questionnaire abbreviations in `codebook`, and
#' returns the abbreviations of the questionnaires actually administered
#' in this study.
#'
#' @note
#' The questionnaire abbreviations are currently selected from
#' `codebook$abbreviation` by a fixed position range (`[24:42]`), not by a
#' category column. This is fragile: any reordering or insertion in the
#' codebook silently changes which rows are treated as questionnaires.
#' Known limitation, to be addressed separately by adapting the codebook's
#' structure; not fixed here.
#'
#' @param metadata A data frame of study metadata, containing a bare
#'   `quest_<abbreviation>` indicator column for every questionnaire
#'   administered (e.g. `quest_stais`).
#' @param codebook A data frame with an `abbreviation` column listing every
#'   measure/rating/questionnaire abbreviation known to the codebook, in
#'   its fixed category order.
#'
#' @return A character vector of the questionnaire abbreviations present
#'   in `metadata` (with the `"quest_"` prefix removed). Empty
#'   (`character(0)`) if none are present.
#' @keywords internal
#' @noRd
fun_which_questionnaires <- function(metadata, codebook){
  
  # Questionnaires in codebook
  
  codebook_quest <- codebook$abbreviation[24:42]
  
  # Get all variables with 'quest' as prefix
  
  quest_vars <- names(metadata)[grepl("^quest_", names(metadata))]
  
  # Select only the questionnaires
  
  quest_provided <- quest_vars[quest_vars %in% c(paste0("quest_", codebook_quest))]
  
  # Remove prefix
  
  quest_names <- sub("^quest_", "", quest_provided)
  
  return(quest_names)
}
