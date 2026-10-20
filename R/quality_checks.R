#' Run quality checks on a study export
#'
#' @description
#' `quality_checks()` reads the exported study data and compares the metadata
#' specification (ratings, questionnaires, physiological measures, phases,
#' timepoints, frequency, aggregation, transformation) against what is
#' actually observed in the measurement data. The result is a single object
#' of class `quality_checks` that bundles the individual check results and
#' can be inspected with `summary()`.
#'
#' @param path Character. Path to the directory containing `data_wide.csv`,
#'   `metadata.csv`, `study_design.csv`, and `codebook.csv`.
#' @param dependencies_path Character or `NULL` (default). Path to a
#'   directory containing custom versions of `info_measures.csv`,
#'   `info_transformation.csv`, and `info_reverse_coding.csv`. Leave this
#'   at `NULL` to use the reference tables shipped with the package, which
#'   is the recommended default for every measure supported out of the
#'   box. Only set this if you need to check measures the package does
#'   not (yet) know about, or to override a package default for a
#'   specific study; see [package_dependency_path()] for the tables that
#'   ship with the package.
#'
#' @return An object of class `quality_checks`: a named list with one entry
#'   per check (`participant_n`, `participant_id`, `phases`, `ratings`,
#'   `questionnaires`, `aggregation`, `transformation`, `timepoint`,
#'   `frequency`). Each entry additionally carries its own S3 class
#'   (e.g. `quality_checks_ratings`) so it can be summarised individually.
#'
#'   The object also contains an `inputs` entry (not a check) with the
#'   material the checks were run on, so that follow-up functions (e.g.
#'   plotting) can work on the result object alone: `data`, `data_format`,
#'   `metadata`, `study_design`, and the resolved measure names
#'   `rating_names`, `rating_names_fix` (without `us_` prefix),
#'   `quest_names`, and `pmeasure_names`.
#'
#' @examples
#' \dontrun{
#' # Standard case: use the reference tables shipped with the package
#' qc <- quality_checks("path/to/study_export")
#'
#' # Exceptional case: use your own reference tables instead
#' qc <- quality_checks("path/to/study_export", dependencies_path = "path/to/custom_tables")
#'
#' summary(qc)              # overview of all checks
#' summary(qc, full = TRUE) # overview + full detail for every check
#' summary(qc$transformation)
#' }
#'
#' @export
quality_checks <- function(path, dependencies_path = NULL) {

  inputs <- load_qc_inputs(path, dependencies_path)

  # Which ratings / questionnaires / physiological measures are actually
  # specified in the metadata, cross-referenced against the codebook

  # /!\ ATTENTION /!\
  # As of right now (8/9/26), the ratings, questionnaires and physiological
  # measures are only indexed by their position as opposed to a dynamic process.
  # Every addition to the codebook therefore changes the index. This could be
  # changed by adapting the codebook, but as of now is a known problem.

  # /!\ ATTENTION /!\
  # As of right now (8/9/26), the ratings are insufficiently named with some
  # ratings being named with reference to the US, while others are not. CS is
  # not specifically referenced. This is likely to be changed in coming
  # reworks of the fearbase, but not implemented yet. A helper-function is the
  # function `fix_rating_names()`, which removes the 'us' in front of the
  # respective ratings.

  rating_names     <- fun_which_ratings(inputs$metadata, inputs$codebook)
  rating_names_fix <- fix_rating_names(rating_names)
  quest_names      <- fun_which_questionnaires(inputs$metadata, inputs$codebook)
  pmeasure_names   <- fun_which_pmeasures(inputs$metadata, inputs$codebook)

  checks <- run_all_checks(
    data                = inputs$data,
    data_format         = inputs$data_format,
    metadata            = inputs$metadata,
    study_design        = inputs$study_design,
    info_measures       = inputs$info_measures,
    info_transformation = inputs$info_transformation,
    rating_names        = rating_names,
    rating_names_fix    = rating_names_fix,
    quest_names         = quest_names,
    pmeasure_names      = pmeasure_names
  )

  result <- assign_qc_classes(checks)

  # Keep the inputs in the result so follow-up functions (e.g. plotting) can
  # work on the result object alone. Added AFTER assign_qc_classes() because
  # `inputs` is not a check and has no check class.
  result$inputs <- list(
    data             = inputs$data,
    data_format      = inputs$data_format,
    metadata         = inputs$metadata,
    study_design     = inputs$study_design,
    rating_names     = rating_names,
    rating_names_fix = rating_names_fix,
    quest_names      = quest_names,
    pmeasure_names   = pmeasure_names
  )

  result
}

#' Run every individual quality check
#'
#' @description
#' Internal orchestrator that calls each `fun_check_*()` function with the
#' arguments it needs and collects the results in a single named list. Kept
#' separate from [quality_checks()] so the check logic can be unit-tested
#' without going through file I/O.
#'
#' @return A named list, one entry per check (not yet classed).
#' @keywords internal
#' @noRd
run_all_checks <- function(data, data_format, metadata, study_design,
                           info_measures, info_transformation,
                           rating_names, rating_names_fix,
                           quest_names, pmeasure_names) {

  list(
    participant_n  = fun_check_number_participants(data, metadata, data_format),
    participant_id = fun_check_participant_id(data, data_format),
    phases         = fun_check_phases(data, study_design, data_format),
    ratings        = fun_check_ratings(data, metadata, rating_names_fix, data_format),
    questionnaires = fun_check_questionnaires(data, metadata, quest_names, data_format),
    aggregation    = fun_check_aggregation(
      data, metadata, rating_names, rating_names_fix,
      quest_names, info_measures, data_format
    ),
    transformation = fun_check_transformation(
      data, metadata, rating_names, rating_names_fix,
      pmeasure_names, info_transformation, data_format
    ),
    timepoint      = fun_check_timepoint(
      data, metadata, study_design,
      rating_names, rating_names_fix, quest_names, pmeasure_names,
      data_format
    ),
    frequency      = fun_check_frequency(
      data, metadata, study_design,
      rating_names, rating_names_fix, quest_names, pmeasure_names,
      info_measures, data_format
    )
  )
}

#' Assign S3 classes to a quality-check result list
#' @keywords internal
#' @noRd
assign_qc_classes <- function(checks) {

  check_classes <- c(
    participant_n  = "quality_checks_participant_n",
    participant_id = "quality_checks_participant_id",
    phases         = "quality_checks_phases",
    ratings        = "quality_checks_ratings",
    questionnaires = "quality_checks_questionnaires",
    aggregation    = "quality_checks_aggregation",
    transformation = "quality_checks_transformation",
    timepoint      = "quality_checks_timepoint",
    frequency      = "quality_checks_frequency"
  )

  for (check_name in names(checks)) {
    class(checks[[check_name]]) <- check_classes[[check_name]]
  }

  class(checks) <- "quality_checks"
  checks
}

#' Path to the reference tables shipped with the package
#'
#' @description
#' Returns the directory containing the built-in `info_measures.csv`,
#' `info_transformation.csv`, and `info_reverse_coding.csv` reference tables,
#' i.e. the expected ranges, means, standard deviations, item counts, and
#' reverse-coding specifications for every questionnaire, rating, and
#' physiological measure the package supports out of the box.
#' Useful for inspecting or copying the defaults as a starting point for a
#' custom table (see the `dependencies_path` argument of
#' [quality_checks()]).
#'
#' @return A file path (character).
#' @export
package_dependency_path <- function(){
  system.file("extdata", package = "qualitychecks")
}

#' Resolve the dependency tables directory to actually use
#'
#' @description
#' Defaults to the package-internal reference tables
#' ([package_dependency_path()]). If a custom `dependencies_path` is
#' supplied, it is used instead and a message makes the deviation from the
#' package defaults explicit, since this is meant to be the exception
#' rather than the rule.
#'
#' @keywords internal
#' @noRd
resolve_dependencies_path <- function(dependencies_path) {

  if (is.null(dependencies_path)) {
    default_path <- package_dependency_path()
    if (!nzchar(default_path)) {
      stop(
        "Could not locate the package's built-in reference tables. ",
        "Please reinstall the package, or supply `dependencies_path` ",
        "pointing to your own info_measures.csv, info_transformation.csv, ",
        "and info_reverse_coding.csv.",
        call. = FALSE
      )
    }
    return(default_path)
  }

  message(
    "Using custom reference tables from '", dependencies_path, "' instead ",
    "of the package defaults. Make sure they follow the same structure as ",
    "the files returned by package_dependency_path()."
  )
  dependencies_path
}

#' Load and validate every input file required by [quality_checks()]
#'
#' @description
#' Reads the measurement data, metadata, study design, codebook, and the
#' dependency lookup tables, and raises an informative error naming the
#' missing/misnamed file if anything is not found. Separated from
#' [quality_checks()] so loading can be tested or swapped out independently
#' of the check logic.
#'
#' @return A named list with entries `data`, `data_format`, `metadata`,
#'   `study_design`, `codebook`, `info_measures`, `info_transformation`,
#'   `info_reverse_coding`.
#' @keywords internal
#' @noRd
load_qc_inputs <- function(path, dependencies_path) {

  dependencies_path <- resolve_dependencies_path(dependencies_path)

  measurement <- load_measurement_data(path)

  metadata <- read_required_csv(
    file.path(path, "metadata.csv"), readr::read_csv,
    label = "meta data", show_col_types = FALSE
  )
  study_design <- read_required_csv(
    file.path(path, "study_design.csv"), utils::read.csv,
    label = "study design"
  )
  codebook <- read_required_csv(
    file.path(path, "codebook.csv"), utils::read.csv,
    label = "codebook"
  )
  info_measures <- read_required_csv(
    file.path(dependencies_path, "info_measures.csv"), utils::read.csv2,
    label = "dependency info_measures"
  )

  info_transformation <- read_required_csv(
    file.path(dependencies_path, "info_transformation.csv"), utils::read.csv2,
    label = "dependency info_transformation"
  )
  info_transformation <- parse_numeric_list_columns(
    info_transformation, c("range", "mean", "sd")
  )

  info_reverse_coding <- read_required_csv(
    file.path(dependencies_path, "info_reverse_coding.csv"), utils::read.csv2,
    label = "dependency info_reverse_coding", na.strings = "NA"
  )
  info_reverse_coding <- parse_numeric_list_columns(
    info_reverse_coding, "items"
  )

  list(
    data                 = measurement$data,
    data_format          = measurement$data_format,
    metadata             = metadata,
    study_design         = study_design,
    codebook             = codebook,
    info_measures        = info_measures,
    info_transformation  = info_transformation,
    info_reverse_coding  = info_reverse_coding
  )
}

#' Load the measurement data, preferring wide over long format
#' @keywords internal
#' @noRd
load_measurement_data <- function(path) {

  wide_file <- file.path(path, "data_wide.csv")
  long_file <- file.path(path, "data_long.csv")

  if (file.exists(wide_file)) {
    return(list(
      data = readr::read_csv(wide_file, show_col_types = FALSE),
      data_format = "wide"
    ))
  }

  if (file.exists(long_file)) {
    return(list(
      data = readr::read_csv(long_file, show_col_types = FALSE),
      data_format = "long"
    ))
  }

  stop(
    "Unable to locate measurement data in '", path, "'. Please verify that ",
    "your measurement data is a '.csv' file named either 'data_wide.csv' or ",
    "'data_long.csv'.",
    call. = FALSE
  )
}

#' Read a required CSV file, or stop with an informative message
#' @keywords internal
#' @noRd
read_required_csv <- function(file, reader, label, ...) {

  if (!file.exists(file)) {
    stop(
      "Unable to locate ", label, ". Please verify that '", file, "' ",
      "exists and is a valid '.csv' file.",
      call. = FALSE
    )
  }

  reader(file, ...)
}

#' Split comma-separated numeric strings into list-columns
#'
#' @description
#' Splits each value in the given columns of `df` on `", "` and converts
#' the pieces to numeric, turning e.g. `"0, 10"` into `c(0, 10)`. Used to
#' parse the `range`/`mean`/`sd` columns of `info_transformation` and the
#' `items` column of `info_reverse_coding`.
#'
#' @note
#' Columns are explicitly coerced to character first. This matters for a
#' reference table with zero data rows (e.g. an `info_reverse_coding.csv`
#' that only has a header so far, since reverse coding is still under
#' development): R types an all-empty column as `logical`, not
#' `character`, and `strsplit()` errors on a non-character input. The
#' coercion keeps that case a harmless zero-row result instead of a crash.
#'
#' @param df A data frame.
#' @param columns Character vector of column names in `df` to parse.
#'
#' @return `df`, with the given columns replaced by list-columns of
#'   numeric vectors.
#' @keywords internal
#' @noRd
parse_numeric_list_columns <- function(df, columns) {
  for (col in columns) {
    df[[col]] <- lapply(strsplit(as.character(df[[col]]), ", "), as.numeric)
  }
  df
}

#' Normalise rating names by stripping a leading "us_" prefix
#'
#' @description
#' The codebook currently prefixes some (but not all) rating abbreviations
#' with `"us_"` while the data columns do not. This strips that prefix where
#' present so rating names can be matched against data column names.
#' Elements without the prefix are returned unchanged.
#'
#' @keywords internal
#' @noRd
fix_rating_names <- function(rating_names) {
  sub("^us_", "", rating_names)
}
