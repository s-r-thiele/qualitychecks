# Shared test fixtures.
#
# testthat automatically sources every file in tests/testthat/ that starts
# with "helper" before running any tests, so the functions defined here are
# available in every test file without an explicit source() call.

#' Build a minimal codebook matching the current (index-based) category
#' layout used by fun_which_pmeasures(), fun_which_ratings(), and
#' fun_which_questionnaires():
#'   rows 1-7   other (participant_id, exp_group, sex, gender, age, hormc, intset)
#'   rows 8-12  physiological measures (5)
#'   rows 13-23 ratings (11)
#'   rows 24-42 questionnaires (19)
#'
#' NOTE: this fixture intentionally mirrors the hardcoded index ranges
#' (codebook$abbreviation[8:12], [13:23], [24:42]) so these tests exercise
#' the current, position-based implementation. If that implementation is
#' later changed to filter by a category column instead of position, this
#' fixture (and the tests built on it) will need to be updated accordingly
#' - that is expected, not a regression.
make_test_codebook <- function() {
  data.frame(
    abbreviation = c(
      # 1-7: other
      "participant_id", "exp_group", "sex", "gender", "age", "hormc", "intset",
      # 8-12: physiological measures
      "scr", "scl", "ps", "fps", "hr",
      # 13-23: ratings
      "expect", "us_intrat", "val", "arous", "fear", "stress", "anx",
      "aware", "us_val", "us_arous", "uneas",
      # 24-42: questionnaires
      "stais", "stait", "neo-ffi", "promis", "asi", "ius", "pswq",
      "neo_pi_r", "dass-21", "dass-21-a", "ads-l", "fsq", "sam", "bdi",
      "fpq", "scl_90", "fpq-sf", "sticsa", "vams"
    ),
    stringsAsFactors = FALSE
  )
}

#' Build a small, fully worked-through `quality_checks` result object.
#'
#' Runs the real fun_check_*() functions on a minimal but complete fixture
#' (1 rating "fear", 1 questionnaire "stais") and assembles + classes the
#' result exactly like quality_checks() does, without going through file
#' I/O. Used to test the summary.quality_checks_*() methods against a
#' realistic, internally-consistent object rather than hand-built mocks
#' that might not match what the check functions actually produce.
make_mock_quality_checks <- function() {

  metadata <- data.frame(
    n_subjects = 3,
    rating_fear_aggregation = "avrg", rating_fear_min_scale = 0, rating_fear_max_scale = 10,
    rating_fear_transformation = "raw", rating_fear_timepoint = "pre",
    rating_fear_frequency = "experimentwise",
    quest_stais_aggregation = "sum", quest_stais_timepoint = "pre",
    quest_stais_frequency = "experimentwise",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 5, 9),
    stais___experimentwise_raw_pre_____t1_ = c(30, 40, 50)
  )
  study_design <- data.frame(
    name = "acq", timeToNext = NA, timeToNextUnit = NA,
    cspTrials = 1, csmTrials = 1, gsTrials = 1
  )
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)
  info_transformation <- data.frame(transformation = c("raw", "None"), stringsAsFactors = FALSE)
  info_transformation$range <- list(c(-1000, 1000), c(-1000, 1000))
  info_transformation$mean  <- list(c(-1000, 1000), c(-1000, 1000))
  info_transformation$sd    <- list(c(0, 1000), c(0, 1000))

  checks <- list(
    participant_n  = fun_check_number_participants(data, metadata, "wide"),
    participant_id = fun_check_participant_id(data, "wide"),
    phases         = fun_check_phases(data, study_design, "wide"),
    ratings        = fun_check_ratings(data, metadata, "fear", "wide"),
    questionnaires = fun_check_questionnaires(data, metadata, "stais", "wide"),
    aggregation    = fun_check_aggregation(data, metadata, "fear", "fear", "stais", info_measures, "wide"),
    transformation = fun_check_transformation(data, metadata, "fear", "fear", character(0), info_transformation, "wide"),
    timepoint      = fun_check_timepoint(data, metadata, study_design, "fear", "fear", "stais", character(0), "wide"),
    frequency      = fun_check_frequency(data, metadata, study_design, "fear", "fear", "stais", character(0), info_measures, "wide")
  )

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
  for (nm in names(checks)) class(checks[[nm]]) <- check_classes[[nm]]
  class(checks) <- "quality_checks"

  checks
}

#' Build a minimal metadata data.frame with the "indicator" columns that
#' fun_which_pmeasures()/fun_which_ratings()/fun_which_questionnaires()
#' look for: a bare column named e.g. "rating_fear" signals that "fear"
#' was administered, independent of its detail columns
#' ("rating_fear_frequency" etc.).
#'
#' @param pmeasures Character vector of physiological measure abbreviations
#'   (from the codebook) to mark as present.
#' @param ratings Character vector of rating abbreviations to mark as present.
#' @param questionnaires Character vector of questionnaire abbreviations to
#'   mark as present.
make_test_metadata <- function(pmeasures = character(0),
                                ratings = character(0),
                                questionnaires = character(0)) {
  # paste0("prefix_", character(0)) returns "prefix_" (length 1) rather
  # than character(0) in R - the same quirk documented as a KNOWN BUG in
  # test-fun_check_timepoint.R / test-fun_check_frequency.R. Guarded
  # against explicitly here so this fixture helper itself stays correct
  # for the empty case.
  prefixed <- function(prefix, x) {
    if (length(x) == 0) character(0) else paste0(prefix, x)
  }

  cols <- c(
    stats::setNames(rep(1, length(pmeasures)), prefixed("physio_", pmeasures)),
    stats::setNames(rep(1, length(ratings)), prefixed("rating_", ratings)),
    stats::setNames(rep(1, length(questionnaires)), prefixed("quest_", questionnaires))
  )

  if (length(cols) == 0) return(data.frame())

  as.data.frame(as.list(cols))
}
