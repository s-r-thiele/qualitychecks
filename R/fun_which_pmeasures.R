#' Determine which physiological measures are present in the metadata
#'
#' @description
#' Cross-references the `"physio_*"` indicator columns present in
#' `metadata` against the physiological-measure abbreviations in
#' `codebook`, and returns the abbreviations of the measures actually
#' administered in this study.
#'
#' @note
#' The physiological-measure abbreviations are currently selected from
#' `codebook$abbreviation` by a fixed position range (`[8:12]`), not by a
#' category column. This is fragile: any reordering or insertion in the
#' codebook silently changes which rows are treated as physiological
#' measures. Known limitation, to be addressed separately by adapting the
#' codebook's structure; not fixed here.
#'
#' @param metadata A data frame of study metadata, containing a bare
#'   `physio_<abbreviation>` indicator column for every physiological
#'   measure administered (e.g. `physio_scr`).
#' @param codebook A data frame with an `abbreviation` column listing every
#'   measure/rating/questionnaire abbreviation known to the codebook, in
#'   its fixed category order.
#'
#' @return A character vector of the physiological-measure abbreviations
#'   present in `metadata` (with the `"physio_"` prefix removed). Empty
#'   (`character(0)`) if none are present.
#' @keywords internal
#' @noRd
fun_which_pmeasures <- function(metadata, codebook){
  
  # Physiological measures in codebook
  
  codebook_pmeasures <- codebook$abbreviation[8:12]
  
  # Get all variables with 'rating' as prefix
  
  pmeasures_vars <- names(metadata)[grepl("^physio_", names(metadata))]
  
  # Select only the ratings
  
  pmeasures_provided <- pmeasures_vars[pmeasures_vars %in% c(paste0("physio_", codebook_pmeasures))]
  
  # Remove prefix
  
  pmeasures_names <- sub("^physio_", "", pmeasures_provided)
  
  return(pmeasures_names)
  
}