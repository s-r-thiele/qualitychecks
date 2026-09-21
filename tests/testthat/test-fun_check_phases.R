test_that("passes when every phase has at least one matching column", {

  study_design <- data.frame(name = c("acq", "ext", "rex"))
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_phasewise_raw_pre__acq____ = c(1, 2, 3),
    fear_0_10_phasewise_raw_pre__ext____ = c(1, 2, 3),
    fear_0_10_phasewise_raw_pre__rex____ = c(1, 2, 3)
  )

  result <- fun_check_phases(data, study_design, data_format = "wide")

  expect_true(result$passed)
  expect_equal(result$pass_phases, c(TRUE, TRUE, TRUE))
  expect_match(result$message, "All phases")
})

test_that("partially fails and reports the count when only some phases are present", {

  study_design <- data.frame(name = c("acq", "ext", "rex"))
  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_phasewise_raw_pre__acq____ = c(1, 2, 3)
  )

  result <- fun_check_phases(data, study_design, data_format = "wide")

  expect_false(result$passed)
  expect_equal(result$pass_phases, c(TRUE, FALSE, FALSE))
  expect_match(result$message, "1 of 3 phases")
})

test_that("fully fails and reports so when no phase has a matching column", {

  study_design <- data.frame(name = c("acq", "ext"))
  data <- data.frame(participant_id = 1:3, age = c(20, 30, 40))

  result <- fun_check_phases(data, study_design, data_format = "wide")

  expect_false(result$passed)
  expect_match(result$message, "No variables were found")
})

test_that("long format is not yet supported and errors explicitly", {

  study_design <- data.frame(name = "acq")
  data <- data.frame(participant_id = 1:3)

  expect_error(
    fun_check_phases(data, study_design, data_format = "long"),
    "Long format for data not yet implemented"
  )
})
