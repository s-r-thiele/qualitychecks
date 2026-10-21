# These fixtures always specify at least one rating AND one questionnaire.
# This sidesteps a real bug (documented in its own test below): paste0()
# with a zero-length vector does not produce a zero-length result in R, so
# passing rating_names/quest_names = character(0) currently crashes this
# function - see the "KNOWN BUG" test at the bottom of this file.

test_that("passes when every specified timepoint has a matching column", {

  metadata <- data.frame(
    rating_fear_timepoint = "pre",
    quest_stais_timepoint = "pre",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$fear$pass)
  expect_true(result$stais$pass)
})

test_that("fails when a specified timepoint has no matching column in the data", {

  metadata <- data.frame(
    rating_fear_timepoint = "pre",
    quest_stais_timepoint = "pre",
    stringsAsFactors = FALSE
  )
  # fear_0_10_experimentwise_raw_pre______ is missing entirely
  data <- data.frame(
    participant_id = 1:3,
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_false(result$passed)
  expect_false(result$fear$pass)
  expect_match(result$description, "'fear'")
})

test_that("multiple comma-separated timepoints are all checked individually", {

  metadata <- data.frame(
    rating_fear_timepoint = "pre,post",
    quest_stais_timepoint = "pre",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3),
    fear_0_10_experimentwise_raw_post______ = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_true(result$passed)
  expect_length(result$fear$value, 2)
})

test_that("a missing (NA) timepoint specification causes an overall fail rather than an error", {

  metadata <- data.frame(
    rating_fear_timepoint = NA_character_,
    quest_stais_timepoint = "pre",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_false(result$passed)
})

test_that("long format is not yet supported and errors explicitly", {

  metadata <- data.frame(rating_fear_timepoint = "pre", stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:3)

  expect_error(
    fun_check_timepoint(
      data, metadata, study_design = NULL,
      rating_names = "fear", rating_names_fix = "fear",
      quest_names = character(0), pmeasure_names = character(0),
      data_format = "long"
    ),
    "Long format for data not yet implemented"
  )
})

test_that("FIXED: no questionnaires at all (quest_names of length 0) no longer errors", {
  # Previously paste0("quest_", character(0)) returned "quest_" (length 1)
  # instead of character(0), producing a spurious "quest__timepoint" lookup
  # that did not exist in metadata. Now explicitly guarded, mirroring the
  # fix already applied to fun_check_frequency(): an empty quest_names
  # yields questionnaires <- character(0) directly, so a study with no
  # questionnaires correctly finds nothing to check and passes trivially.
  metadata <- data.frame(rating_fear_timepoint = "pre", stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:3, fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3))

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = character(0), pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$fear$pass)
})

test_that("FIXED: no ratings at all (rating_names of length 0) no longer errors", {
  # Mirror image of the fix above, guarding the ratings side.
  metadata <- data.frame(quest_stais_timepoint = "pre", stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:3, stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3))

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = character(0), rating_names_fix = character(0),
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$stais$pass)
})

# ---- trialwise exception ---------------------------------------------------

test_that("trialwise measure without a timepoint passes", {

  metadata <- data.frame(
    rating_fear_timepoint = NA_character_,
    rating_fear_frequency = "trialwise",
    quest_stais_timepoint = "pre",
    quest_stais_frequency = "experimentwise",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_trialwise_raw_acq______ = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$fear$pass)
  expect_true(result$fear$check[["NA"]])
})

test_that("a non-trialwise measure without a timepoint still fails", {

  metadata <- data.frame(
    rating_fear_timepoint = NA_character_,
    rating_fear_frequency = "experimentwise",
    quest_stais_timepoint = "pre",
    quest_stais_frequency = "experimentwise",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_false(result$passed)
  expect_true(is.na(result$fear$pass))
  expect_match(result$description, "'fear'")
})

test_that("a timepoint specified for a trialwise measure is still checked", {

  metadata <- data.frame(
    rating_fear_timepoint = "post",
    rating_fear_frequency = "trialwise",
    quest_stais_timepoint = "pre",
    quest_stais_frequency = "experimentwise",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_trialwise_raw_pre______ = c(1, 2, 3),   # no "post" column
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_false(result$passed)
  expect_false(result$fear$pass)
})

test_that("a missing or NA frequency is treated as not trialwise instead of erroring", {

  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )
  run <- function(metadata) {
    fun_check_timepoint(
      data, metadata, study_design = NULL,
      rating_names = "fear", rating_names_fix = "fear",
      quest_names = "stais", pmeasure_names = character(0),
      data_format = "wide"
    )
  }

  # frequency column absent: the existing timepoint logic is unchanged
  md_absent <- data.frame(
    rating_fear_timepoint = "pre", quest_stais_timepoint = "pre",
    stringsAsFactors = FALSE
  )
  expect_true(run(md_absent)$passed)

  # frequency NA, timepoint NA: not trialwise -> fails, no error
  md_na <- data.frame(
    rating_fear_timepoint = NA_character_, rating_fear_frequency = NA_character_,
    quest_stais_timepoint = "pre", quest_stais_frequency = "experimentwise",
    stringsAsFactors = FALSE
  )
  expect_false(run(md_na)$passed)
})

test_that("a trialwise measure may have several comma-separated timepoints that are all checked", {

  metadata <- data.frame(
    rating_fear_timepoint = "pre,post",
    rating_fear_frequency = "trialwise",
    quest_stais_timepoint = "pre",
    quest_stais_frequency = "experimentwise",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_trialwise_raw_pre______ = c(1, 2, 3),
    fear_0_10_trialwise_raw_post_____ = c(1, 2, 3),
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_timepoint(
    data, metadata, study_design = NULL,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", pmeasure_names = character(0),
    data_format = "wide"
  )

  expect_true(result$passed)
  expect_setequal(names(result$fear$check), c("pre", "post"))
})
