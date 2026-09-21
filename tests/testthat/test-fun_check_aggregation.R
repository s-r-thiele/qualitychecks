test_that("passes for both a rating (avrg) and a questionnaire (sum) within range", {

  metadata <- data.frame(
    rating_fear_aggregation = "avrg",
    rating_fear_min_scale = 0,
    rating_fear_max_scale = 10,
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 5, 9),
    stais___experimentwise_raw_pre______sum = c(30, 40, 50)
  )
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  result <- fun_check_aggregation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", info_measures = info_measures, data_format = "wide"
  )

  expect_true(result$passed)
  expect_equal(result$ratings$fear$expected_range, c(0, 10))
  # sum aggregation: item range (1-4) x n_items (20) = (20, 80)
  expect_equal(result$questionnaires$stais$expected_range, c(20, 80))
})

test_that("a questionnaire missing from info_measures fails with a warning, others still evaluated normally", {

  metadata <- data.frame(
    quest_stais_aggregation = "sum",
    quest_unknownq_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    stais___experimentwise_raw_pre______sum = c(30, 40, 50),
    unknownq___experimentwise_raw_pre______sum = c(30, 40, 50)
  )
  # info_measures only has a row for "stais", not "unknownq"
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  expect_warning(
    result <- fun_check_aggregation(
      data, metadata,
      rating_names = character(0), rating_names_fix = character(0),
      quest_names = c("stais", "unknownq"), info_measures = info_measures, data_format = "wide"
    ),
    "No reference data.*unknownq.*info_measures"
  )

  # "stais" was genuinely evaluated and passes on its own merits
  expect_true(result$questionnaires$stais$pass_quest)
  # "unknownq" is reported as a failure, not silently skipped, so the gap
  # cannot be missed even without looking at the warning
  expect_false(result$questionnaires$unknownq$pass_quest)
  expect_match(result$questionnaires$unknownq$error, "contact the Fearbase team")
  # The overall result fails because of the missing reference data, even
  # though the questionnaire that COULD be evaluated passed
  expect_false(result$passed)
  expect_match(result$questionnaires$description_q, "1 of 2")
})

test_that("if every questionnaire is missing from info_measures, the whole check fails", {
  metadata <- data.frame(quest_unknownq_aggregation = "sum", stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:3, unknownq___experimentwise_raw_pre______sum = c(30, 40, 50))
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  expect_warning(
    result <- fun_check_aggregation(
      data, metadata,
      rating_names = character(0), rating_names_fix = character(0),
      quest_names = "unknownq", info_measures = info_measures, data_format = "wide"
    ),
    "No reference data"
  )

  expect_false(result$passed)
  expect_false(result$questionnaires$unknownq$pass_quest)
})

test_that("fails when an observed value falls outside the expected range", {

  metadata <- data.frame(
    rating_fear_aggregation = "avrg",
    rating_fear_min_scale = 0,
    rating_fear_max_scale = 10,
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  # 15 is outside the specified 0-10 range
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 5, 15),
    stais___experimentwise_raw_pre______sum = c(30, 40, 50)
  )
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  result <- fun_check_aggregation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", info_measures = info_measures, data_format = "wide"
  )

  expect_false(result$passed)
  expect_false(result$ratings$fear$pass_rat)
})

test_that("a missing column in the data is reported rather than erroring", {

  metadata <- data.frame(
    rating_fear_aggregation = "avrg",
    rating_fear_min_scale = 0,
    rating_fear_max_scale = 10,
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  # rating_fear_* is missing entirely
  data <- data.frame(participant_id = 1:3, stais___experimentwise_raw_pre______sum = c(30, 40, 50))
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  result <- fun_check_aggregation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", info_measures = info_measures, data_format = "wide"
  )

  expect_false(result$passed)
  expect_match(result$ratings$fear$error, "cannot be found in data")
})

test_that("an unspecified (NA) aggregation falls back to 'None' and uses the scale range as-is", {

  metadata <- data.frame(
    rating_fear_aggregation = NA_character_,
    rating_fear_min_scale = 0,
    rating_fear_max_scale = 10,
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 5, 9),
    stais___experimentwise_raw_pre______sum = c(30, 40, 50)
  )
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  result <- fun_check_aggregation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", info_measures = info_measures, data_format = "wide"
  )

  expect_equal(result$ratings$fear$aggregation, "None")
  expect_true(result$passed)
})

test_that("a 'sum' aggregation for a rating is not implemented and errors explicitly", {
  # Only average ('avrg') / unspecified aggregation is implemented for
  # ratings; 'sum' is explicitly a not-yet-implemented failsafe.
  metadata <- data.frame(
    rating_fear_aggregation = "sum",
    rating_fear_min_scale = 0,
    rating_fear_max_scale = 10,
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 5, 9),
    stais___experimentwise_raw_pre______sum = c(30, 40, 50)
  )
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  expect_error(
    fun_check_aggregation(
      data, metadata,
      rating_names = "fear", rating_names_fix = "fear",
      quest_names = "stais", info_measures = info_measures, data_format = "wide"
    ),
    "has not been"
  )
})

test_that("an unimplemented aggregation method for a questionnaire errors explicitly", {
  # Only 'avrg'/unspecified and 'sum' are implemented for questionnaires.
  metadata <- data.frame(
    rating_fear_aggregation = "avrg",
    rating_fear_min_scale = 0,
    rating_fear_max_scale = 10,
    quest_stais_aggregation = "median",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 5, 9),
    stais___experimentwise_raw_pre______sum = c(30, 40, 50)
  )
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  expect_error(
    fun_check_aggregation(
      data, metadata,
      rating_names = "fear", rating_names_fix = "fear",
      quest_names = "stais", info_measures = info_measures, data_format = "wide"
    ),
    "have not been implemented"
  )
})

test_that("long format is not yet supported and errors explicitly", {

  metadata <- data.frame(
    rating_fear_aggregation = "avrg", rating_fear_min_scale = 0, rating_fear_max_scale = 10,
    stringsAsFactors = FALSE
  )
  data <- data.frame(participant_id = 1:3, fear_0_10_experimentwise_raw_pre______ = c(1, 5, 9))

  expect_error(
    fun_check_aggregation(
      data, metadata,
      rating_names = "fear", rating_names_fix = "fear",
      quest_names = character(0), info_measures = NULL, data_format = "long"
    ),
    "Long format for data not yet implemented"
  )
})

test_that("FIXED: no ratings at all (rating_names_fix of length 0) no longer errors", {
  # Previously `for (r in 1:length(rating_names_fix))` hit the classic
  # 1:0 == c(1, 0) pitfall for an empty vector, causing a hard error for
  # any study with no ratings at all. Now uses seq_along(), which
  # correctly produces an empty sequence for a length-0 input, so a study
  # with no ratings simply has nothing to check on the ratings side and
  # passes trivially.
  metadata <- data.frame(quest_stais_aggregation = "sum", stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:3, stais___experimentwise_raw_pre______sum = c(30, 40, 50))
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  result <- fun_check_aggregation(
    data, metadata,
    rating_names = character(0), rating_names_fix = character(0),
    quest_names = "stais", info_measures = info_measures, data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$ratings$passed_r)
})

test_that("FIXED: no questionnaires at all (quest_names of length 0) no longer errors", {
  # Mirror image of the fix above, for `for (q in 1:length(quest_names))`.
  metadata <- data.frame(
    rating_fear_aggregation = "avrg",
    rating_fear_min_scale = 0,
    rating_fear_max_scale = 10,
    stringsAsFactors = FALSE
  )
  data <- data.frame(participant_id = 1:3, fear_0_10_experimentwise_raw_pre______ = c(1, 5, 9))

  result <- fun_check_aggregation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = character(0), info_measures = NULL, data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$questionnaires$passed_q)
})

test_that("FIXED: an infix between the rating name and '_max_scale' now matches correctly", {
  # Previously, the regex used to locate the "*_max_scale" column was
  # paste0(rating, ",*", "_max_scale") - a comma where a dot was clearly
  # intended (a copy-paste slip from the "_min_scale" line just above it,
  # which correctly used ".*"). ",*" only matched when the rating name was
  # immediately followed by "_max_scale" with nothing in between, which
  # happened to work under the naming convention in use but broke as soon
  # as anything else appeared in between (e.g. a "_wording" or other infix
  # segment). Now fixed to ".*", matching "_min_scale"'s pattern; this test
  # documents the fix and should stay green going forward.
  metadata <- data.frame(
    rating_fear_aggregation = "avrg",
    rating_fear_min_scale = 0,
    rating_fear_infix_max_scale = 10, # anything other than a comma before "_max_scale"
    quest_stais_aggregation = "sum",
    stringsAsFactors = FALSE
  )
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre______ = c(1, 5, 9),
    stais___experimentwise_raw_pre_____t1_ = c(30, 40, 50)
  )
  info_measures <- data.frame(Abbreviation = "stais", range_min = 1, range_max = 4, n_items = 20)

  result <- fun_check_aggregation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    quest_names = "stais", info_measures = info_measures, data_format = "wide"
  )

  expect_true(result$passed)
  expect_equal(result$ratings$fear$expected_range, c(0, 10))
  expect_true(result$ratings$fear$pass_rat)
})
