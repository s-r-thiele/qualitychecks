#' Determine which ratings are present in the metadata
#'
#' @description
#' Cross-references the `"rating_*"` indicator columns present in
#' `metadata` against the rating abbreviations in `codebook`, and returns
#' the abbreviations of the ratings actually administered in this study.
#'
#' @note
#' The rating abbreviations are currently selected from
#' `codebook$abbreviation` by a fixed position range (`[13:23]`), not by a
#' category column. This is fragile: any reordering or insertion in the
#' codebook silently changes which rows are treated as ratings. Known
#' limitation, to be addressed separately by adapting the codebook's
#' structure; not fixed here.
#'
#' Some rating abbreviations are prefixed with `"us_"` in the codebook
#' (referring to the unconditioned stimulus) while others are not; this
#' function returns the abbreviations as-is, still including any `"us_"`
#' prefix. Use `fix_rating_names()` to strip it where needed for matching
#' against data column names.
#'
#' @param metadata A data frame of study metadata, containing a bare
#'   `rating_<abbreviation>` indicator column for every rating administered
#'   (e.g. `rating_fear`, `rating_us_intrat`).
#' @param codebook A data frame with an `abbreviation` column listing every
#'   measure/rating/questionnaire abbreviation known to the codebook, in
#'   its fixed category order.
#'
#' @return A character vector of the rating abbreviations present in
#'   `metadata` (with the `"rating_"` prefix removed, `"us_"` prefix kept
#'   as-is). Empty (`character(0)`) if none are present.
#' @keywords internal
#' @noRd
fun_which_ratings <- function(metadata, codebook){
  
  # Ratings in codebook
  
  codebook_ratings <- codebook$abbreviation[13:23]
  
  # Get all variables with 'rating' as prefix
  
  rating_vars <- names(metadata)[grepl("^rating_", names(metadata))]
  
  # Select only the ratings
  
  ratings_provided <- rating_vars[rating_vars %in% c(paste0("rating_", codebook_ratings))]
  
  # Remove prefix
  
  rating_names <- sub("^rating_", "", ratings_provided)
  
  return(rating_names)
  
}


