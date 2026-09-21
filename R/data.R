#' Reference values for questionnaires
#'
#' @description
#' The number of items and the plausible per-item response range for
#' every questionnaire the package knows about. Used by
#' `fun_check_aggregation()` and `fun_check_frequency()` as the default
#' reference data (see the `dependencies_path` argument of
#' [quality_checks()]).
#'
#' This is the same content as the CSV file returned by
#' `system.file("extdata", "info_measures.csv", package = "qualitychecks")`,
#' provided here as an ordinary data frame so it can be browsed directly,
#' e.g. with `View(qualitychecks::info_measures)`.
#'
#' @format A data frame with one row per questionnaire and four columns:
#' \describe{
#'   \item{Abbreviation}{Questionnaire abbreviation, matching the
#'     abbreviations used in `codebook.csv` and in variable names (e.g.
#'     `"stais"`).}
#'   \item{n_items}{Number of items the questionnaire has.}
#'   \item{range_min}{Lowest possible response value for a single item.}
#'   \item{range_max}{Highest possible response value for a single item.}
#' }
#'
#' @source Maintained by the Fearbase team in
#'   `inst/extdata/info_measures.csv`; regenerated via
#'   `data-raw/create_data.R` whenever that file changes.
#'
#' @seealso [info_transformation] for the equivalent reference table for
#'   transformations.
"info_measures"

#' Reference values for transformations
#'
#' @description
#' The plausible range, mean, and standard deviation for every
#' transformation the package knows about. Used by
#' `fun_check_transformation()` as the default reference data (see the
#' `dependencies_path` argument of [quality_checks()]).
#'
#' This is the same content as the CSV file returned by
#' `system.file("extdata", "info_transformation.csv", package = "qualitychecks")`,
#' provided here as an ordinary data frame so it can be browsed directly,
#' e.g. with `View(qualitychecks::info_transformation)`.
#'
#' @format A data frame with one row per transformation and four columns:
#' \describe{
#'   \item{transformation}{Transformation abbreviation, matching the
#'     abbreviations used in variable names and in
#'     `<measure>_transformation` metadata columns (e.g. `"z"`, `"log"`,
#'     `"sqrt"`, `"range"`, `"t"`, or `"None"` for untransformed data).}
#'   \item{range}{Plausible range, as the text `"min, max"` (e.g.
#'     `"-Inf, Inf"`). Parsed into a numeric `c(min, max)` vector by
#'     `fun_check_transformation()` at check time.}
#'   \item{mean}{Plausible range for the observed mean, in the same
#'     `"min, max"` text format.}
#'   \item{sd}{Plausible range for the observed standard deviation, in
#'     the same `"min, max"` text format.}
#' }
#'
#' @source Maintained by the Fearbase team in
#'   `inst/extdata/info_transformation.csv`; regenerated via
#'   `data-raw/create_data.R` whenever that file changes.
#'
#' @seealso [info_measures] for the equivalent reference table for
#'   questionnaires.
"info_transformation"
