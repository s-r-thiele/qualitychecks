# These fixtures always specify at least one rating AND one questionnaire
# (with its "_aggregation" column present). Note: metadata is exported from
# our input mask, where any rating/questionnaire/physiological measure
# marked as present always has an "_aggregation" column (even if its value
# is NA) - so a completely missing "_aggregation" column cannot occur in
# practice and is not tested here.

test_that("a questionnaire missing from info_measures (unspecified aggregation) fails with a warning", {

  metadata <- data.frame(
    quest_stais_frequency  = "experimentwise", quest_stais_timepoint  = "pre",
    quest_stais_aggregation = "sum",
    quest_unknownq_frequency  = "experimentwise", quest_unknownq_timepoint  = "pre",
    quest_unknownq_aggregation = NA_character_, # unspecified -> needs item count
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    stais___experimentwise_raw_pre_____t1_    = c(1, 2, 3),
    unknownq___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )
  # info_measures only has a row for "stais", not "unknownq"
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  expect_warning(
    result <- fun_check_frequency(
      data, metadata, study_design = data.frame(),
      rating_names = character(0), rating_names_fix = character(0),
      quest_names = c("stais", "unknownq"), pmeasure_names = character(0),
      info_measures = info_measures, data_format = "wide"
    ),
    "No reference data.*unknownq.*info_measures"
  )

  # "stais" has a declared aggregation, so it never needed info_measures
  # and was genuinely evaluated
  expect_true(result$stais$pass)
  # "unknownq" is reported as a failure, not silently skipped, so the gap
  # cannot be missed even without looking at the warning
  expect_false(result$unknownq$pass)
  expect_match(result$unknownq$error, "contact the Fearbase team")
  # The observed column count is still reported even though the expected
  # count could not be computed, to aid troubleshooting
  expect_equal(result$unknownq$n_col_obs, 1)
  expect_true(is.na(result$unknownq$n_col_exp))
  # The overall result fails because of the missing reference data, even
  # though the measure that COULD be evaluated passed
  expect_false(result$passed)
})

test_that("a questionnaire with a declared aggregation never needs info_measures, even if missing there", {
  # info_measures is only consulted for the item count when a
  # questionnaire's aggregation is unspecified. With "sum" declared, this
  # questionnaire's frequency check does not depend on info_measures at
  # all, so a missing entry there should NOT trigger a warning or a skip.
  metadata <- data.frame(
    quest_unknownq_frequency  = "experimentwise", quest_unknownq_timepoint  = "pre",
    quest_unknownq_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    unknownq___experimentwise_raw_pre_____t1_sum = c(1, 2, 3)
  )
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  expect_no_warning(
    result <- fun_check_frequency(
      data, metadata, study_design = data.frame(),
      rating_names = character(0), rating_names_fix = character(0),
      quest_names = "unknownq", pmeasure_names = character(0),
      info_measures = info_measures, data_format = "wide"
    )
  )

  expect_true(result$passed)
  expect_false(is.na(result$unknownq$pass))
})

test_that("passes when the observed column count matches an 'experimentwise' spec", {

  metadata <- data.frame(
    rating_fear_frequency  = "experimentwise", rating_fear_timepoint  = "pre",
    quest_stais_frequency  = "experimentwise", quest_stais_timepoint  = "pre",
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  # experimentwise + 1 timepoint -> exactly 1 expected column
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_frequency(
    data, metadata, study_design = data.frame(),
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    info_measures = NULL, data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$fear$pass)
  expect_equal(result$fear$n_col_exp, 1)
  expect_equal(result$fear$n_col_obs, 1)
})

test_that("fails when more columns are observed than the spec expects", {

  metadata <- data.frame(
    rating_fear_frequency  = "experimentwise", rating_fear_timepoint  = "pre",
    quest_stais_frequency  = "experimentwise", quest_stais_timepoint  = "pre",
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  # Two columns match the search pattern, but only 1 is expected
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre____csp1__ = c(1, 2, 3),
    fear_0_10_experimentwise_raw_pre____csm1__ = c(4, 5, 6),
    stais___experimentwise_raw_pre_____t1_   = c(1, 2, 3)
  )

  result <- fun_check_frequency(
    data, metadata, study_design = data.frame(),
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    info_measures = NULL, data_format = "wide"
  )

  expect_false(result$passed)
  expect_false(result$fear$pass)
  expect_equal(result$fear$n_col_exp, 1)
  expect_equal(result$fear$n_col_obs, 2)
  expect_match(result$description, "'fear'")
})

test_that("multiple specified timepoints multiply the expected column count", {

  # Two comma-separated timepoints -> 2 expected columns for the rating,
  # matching the logic duplicated from fun_check_timepoint()
  metadata <- data.frame(
    rating_fear_frequency  = "experimentwise", rating_fear_timepoint  = "pre, post",
    quest_stais_frequency  = "experimentwise", quest_stais_timepoint  = "pre",
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______  = c(1, 2, 3),
    fear_0_10_experimentwise_raw_post______ = c(4, 5, 6),
    stais___experimentwise_raw_pre_____t1_  = c(1, 2, 3)
  )

  result <- fun_check_frequency(
    data, metadata, study_design = data.frame(),
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    info_measures = NULL, data_format = "wide"
  )

  expect_true(result$passed)
  expect_equal(result$fear$n_col_exp, 2)
  expect_equal(result$fear$n_col_obs, 2)
})

test_that("'daywise' frequency uses the sum of study_design$timeToNext (in days)", {

  metadata <- data.frame(
    rating_fear_frequency  = "daywise", rating_fear_timepoint  = "pre",
    quest_stais_frequency  = "experimentwise", quest_stais_timepoint  = "pre",
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  study_design <- data.frame(
    timeToNext     = c(1, 1),
    timeToNextUnit = c("days", "days")
  )
  # daywise over 2 days x 1 timepoint -> 2 expected columns
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_daywise_raw_pre_d1_____ = c(1, 2, 3),
    fear_0_10_daywise_raw_pre_d2_____ = c(4, 5, 6),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_frequency(
    data, metadata, study_design = study_design,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    info_measures = NULL, data_format = "wide"
  )

  expect_true(result$passed)
  expect_equal(result$fear$n_col_exp, 2)
})

test_that("'blockwise' frequency is not yet implemented and errors explicitly", {

  metadata <- data.frame(
    rating_fear_frequency  = "blockwise", rating_fear_timepoint  = "pre",
    quest_stais_frequency  = "experimentwise", quest_stais_timepoint  = "pre",
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_blockwise_raw_pre______      = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  expect_error(
    fun_check_frequency(
      data, metadata, study_design = data.frame(),
      rating_names = "fear", rating_names_fix = "fear",
      quest_names = "stais", pmeasure_names = character(0),
      info_measures = NULL, data_format = "wide"
    ),
    "Blockwise frequency is not implemented yet"
  )
})

test_that("FIXED: no questionnaires at all (quest_names of length 0) no longer errors", {
  # Previously, paste0("quest_", character(0)) returned "quest_" (length 1)
  # instead of character(0), producing a spurious "quest__frequency" lookup
  # that did not exist in metadata. Now explicitly guarded: an empty
  # quest_names yields questionnaires <- character(0) directly, so a study
  # with no questionnaires at all correctly finds nothing to check and
  # passes trivially.
  metadata <- data.frame(
    rating_fear_frequency = "experimentwise", rating_fear_timepoint = "pre",
    stringsAsFactors = FALSE
  )
  data <- data.frame(participant_id = 1:3, fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3))

  result <- fun_check_frequency(
    data, metadata, study_design = data.frame(),
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = character(0), pmeasure_names = character(0),
    info_measures = NULL, data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$fear$pass)
})

test_that("FIXED: no ratings at all (rating_names of length 0) no longer errors", {
  # Mirror image of the fix above, guarding the ratings side.
  metadata <- data.frame(
    quest_stais_frequency = "experimentwise", quest_stais_timepoint = "pre",
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(participant_id = 1:3, stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3))

  result <- fun_check_frequency(
    data, metadata, study_design = data.frame(),
    rating_names = character(0), rating_names_fix = character(0),
    quest_names = "stais", pmeasure_names = character(0),
    info_measures = NULL, data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$stais$pass)
})


