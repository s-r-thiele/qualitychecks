test_that("passes when every questionnaire has at least one matching column", {

  data <- data.frame(
    participant_id = 1:3,
    stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3),
    stait___experimentwise_raw_pre_____t1_ = c(1, 2, 3)
  )

  result <- fun_check_questionnaires(data, metadata = NULL, quest_names = c("stais", "stait"), data_format = "wide")

  expect_true(result$passed)
  expect_equal(result$pass_questionnaires, c(TRUE, TRUE))
  expect_match(result$message, "All questionnaires")
})

test_that("partially fails and reports the count when only some questionnaires are present", {

  data <- data.frame(participant_id = 1:3, stais___experimentwise_raw_pre_____t1_ = c(1, 2, 3))

  result <- fun_check_questionnaires(data, metadata = NULL, quest_names = c("stais", "stait"), data_format = "wide")

  expect_false(result$passed)
  expect_equal(result$pass_questionnaires, c(TRUE, FALSE))
  expect_match(result$message, "1 of 2 questionnaires")
})

test_that("fully fails and reports so when no questionnaire has a matching column", {

  data <- data.frame(participant_id = 1:3, age = c(20, 30, 40))

  result <- fun_check_questionnaires(data, metadata = NULL, quest_names = c("stais", "stait"), data_format = "wide")

  expect_false(result$passed)
  expect_match(result$message, "No variables were found")
})

test_that("long format is not yet supported and errors explicitly", {

  data <- data.frame(participant_id = 1:3)

  expect_error(
    fun_check_questionnaires(data, metadata = NULL, quest_names = "stais", data_format = "long"),
    "Long format for data not yet implemented"
  )
})
