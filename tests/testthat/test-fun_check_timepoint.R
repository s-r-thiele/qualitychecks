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
