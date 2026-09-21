test_that("passes when observed rows match the specified n_subjects", {

  data <- data.frame(participant_id = 1:10)
  metadata <- data.frame(n_subjects = 10)

  result <- fun_check_number_participants(data, metadata, data_format = "wide")

  expect_true(result$passed)
  expect_equal(result$observed, 10)
  expect_equal(result$expected, 10)
  expect_match(result$message, "correct")
})

test_that("fails when observed rows differ from the specified n_subjects", {

  data <- data.frame(participant_id = 1:8)
  metadata <- data.frame(n_subjects = 10)

  result <- fun_check_number_participants(data, metadata, data_format = "wide")

  expect_false(result$passed)
  expect_equal(result$observed, 8)
  expect_equal(result$expected, 10)
  expect_match(result$message, "Expected 10 participants, found 8")
})

test_that("an empty data set is reported correctly rather than erroring", {

  data <- data.frame(participant_id = integer(0))
  metadata <- data.frame(n_subjects = 5)

  result <- fun_check_number_participants(data, metadata, data_format = "wide")

  expect_false(result$passed)
  expect_equal(result$observed, 0)
})

test_that("long format is not yet supported and errors explicitly", {

  data <- data.frame(participant_id = 1:10)
  metadata <- data.frame(n_subjects = 10)

  expect_error(
    fun_check_number_participants(data, metadata, data_format = "long"),
    "Long format for data not yet implemented"
  )
})

test_that("an unexpected data_format value errors with the internal error message", {

  data <- data.frame(participant_id = 1:10)
  metadata <- data.frame(n_subjects = 10)

  expect_error(
    fun_check_number_participants(data, metadata, data_format = "spss"),
    "Internal error"
  )
})
