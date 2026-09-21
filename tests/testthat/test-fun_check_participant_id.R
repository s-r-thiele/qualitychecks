test_that("passes when a participant_id column is present (wide format)", {

  data <- data.frame(
    participant_id = 1:5,
    fear_0_10_experimentwise_raw_pre____ = c(1, 2, 3, 4, 5)
  )

  result <- fun_check_participant_id(data, data_format = "wide")

  expect_true(result$passed)
  expect_match(result$message, "has a participant_id variable")
})

test_that("fails when no participant_id column is present (wide format)", {

  data <- data.frame(
    id = 1:5,
    fear_0_10_experimentwise_raw_pre____ = c(1, 2, 3, 4, 5)
  )

  result <- fun_check_participant_id(data, data_format = "wide")

  expect_false(result$passed)
  expect_match(result$message, "no participant_id variable")
})

test_that("also matches column names that merely contain 'participant_id'", {
  # e.g. a prefixed or suffixed variant such as 'participant_id_original'
  data <- data.frame(participant_id_original = 1:5)

  result <- fun_check_participant_id(data, data_format = "wide")

  expect_true(result$passed)
})

test_that("long format is not yet supported and errors explicitly", {

  data <- data.frame(participant_id = 1:5)

  expect_error(
    fun_check_participant_id(data, data_format = "long"),
    "Long format for data not yet implemented"
  )
})

test_that("an unexpected data_format value errors with the internal error message", {

  data <- data.frame(participant_id = 1:5)

  expect_error(
    fun_check_participant_id(data, data_format = "spss"),
    "Internal error"
  )
})
