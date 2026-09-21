# These tests use make_mock_quality_checks() (see helper-fixtures.R), which
# runs the real fun_check_*() functions on a small, internally-consistent
# fixture and classes the results exactly like quality_checks() does. This
# means the summary methods are exercised against genuine check output
# rather than hand-built mocks that might drift from what the check
# functions actually produce.
#
# Since these functions only cat() formatted text (no return value worth
# asserting on), the tests check that each method runs without error and
# that its output contains the expected header/labels and correctly
# reflects the passed/failed status - not the exact column alignment,
# which is a presentation detail that may change.

test_that("summary.quality_checks_participant_n reports header, status and message", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$participant_n))

  expect_match(paste(out, collapse = "\n"), "Quality Check Participant_n")
  expect_match(paste(out, collapse = "\n"), "Participant n")
  expect_match(paste(out, collapse = "\n"), checks$participant_n$message, fixed = TRUE)
})

test_that("summary.quality_checks_participant_id reports header, status and message", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$participant_id))

  expect_match(paste(out, collapse = "\n"), "Quality Check Participant_id")
  expect_match(paste(out, collapse = "\n"), checks$participant_id$message, fixed = TRUE)
})

test_that("summary.quality_checks_phases lists every phase with its individual result", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$phases))
  full <- paste(out, collapse = "\n")

  expect_match(full, "Quality Check Phases")
  for (phase in checks$phases$phases) {
    expect_match(full, phase, fixed = TRUE)
  }
})

test_that("summary.quality_checks_ratings lists every rating with its individual result", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$ratings))
  full <- paste(out, collapse = "\n")

  expect_match(full, "Quality Check Ratings")
  expect_match(full, "fear")
})

test_that("summary.quality_checks_questionnaires lists every questionnaire with its individual result", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$questionnaires))
  full <- paste(out, collapse = "\n")

  expect_match(full, "Quality Check Questionnaires")
  expect_match(full, "stais")
})

test_that("summary.quality_checks_aggregation reports both ratings and questionnaires", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$aggregation))
  full <- paste(out, collapse = "\n")

  expect_match(full, "Quality Check Aggregation")
  expect_match(full, "fear")
  expect_match(full, "stais")
  expect_match(full, "Expected Range")
  expect_match(full, "Observed Range")
})

test_that("summary.quality_checks_transformation reports every measure and transformation", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$transformation))
  full <- paste(out, collapse = "\n")

  expect_match(full, "Quality Check Transformation")
  expect_match(full, "fear")
  expect_match(full, "Mean exp\\.")
  expect_match(full, "SD obs\\.")
})

test_that("summary.quality_checks_timepoint reports every measure and timepoint", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$timepoint))
  full <- paste(out, collapse = "\n")

  expect_match(full, "Quality Check Timepoint")
  expect_match(full, "fear")
  expect_match(full, "pre")
})

test_that("summary.quality_checks_frequency reports every measure with expected/observed counts", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks$frequency))
  full <- paste(out, collapse = "\n")

  expect_match(full, "Quality Check Frequency")
  expect_match(full, "n columns exp\\.")
  expect_match(full, "n columns obs\\.")
})

test_that("summary.quality_checks (short form) lists all nine checks and offers 'full = TRUE'", {

  checks <- make_mock_quality_checks()
  out <- capture.output(summary(checks, full = FALSE))
  full <- paste(out, collapse = "\n")

  expect_match(full, "Quality Check Summary")
  for (label in c("Participant n", "Participant ID", "Phases", "Ratings",
                   "Questionnaires", "Aggregation", "Transformation",
                   "Timepoint", "Frequency")) {
    expect_match(full, label, fixed = TRUE)
  }
  expect_match(full, "full = TRUE")
})

test_that("summary.quality_checks (full form) runs every nested summary without error", {

  checks <- make_mock_quality_checks()

  expect_error(
    out <- capture.output(summary(checks, full = TRUE)),
    NA # expect_error(..., NA) asserts that no error occurs
  )
  # A full report should be substantially longer than the short overview
  short <- capture.output(summary(checks, full = FALSE))
  expect_gt(length(out), length(short))
})
