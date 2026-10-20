test_that("parse_numeric_list_columns splits comma-separated values into numeric vectors", {

  df <- data.frame(
    transformation = c("z", "range"),
    range = c("-1000, 1000", "0, 1"),
    stringsAsFactors = FALSE
  )

  result <- parse_numeric_list_columns(df, "range")

  expect_equal(result$range[[1]], c(-1000, 1000))
  expect_equal(result$range[[2]], c(0, 1))
})

test_that("FIXED: a zero-row reference table no longer errors", {
  # A data.frame read from a CSV with only a header (no data rows) has its
  # columns typed as `logical` by R, not `character` - previously this
  # made strsplit() error with "non-character argument". This matters in
  # practice: info_reverse_coding.csv is legitimately near-empty while
  # reverse coding is still under development. Now the column is coerced
  # to character first, so this correctly produces a zero-row result
  # instead of crashing.
  df <- data.frame(questionnaire = character(0), items = logical(0))

  result <- parse_numeric_list_columns(df, "items")

  expect_length(result$items, 0)
})

test_that("fix_rating_names strips a leading 'us_' prefix where present", {

  expect_equal(fix_rating_names(c("us_intrat", "fear")), c("intrat", "fear"))
  expect_equal(fix_rating_names(character(0)), character(0))
})

test_that("quality_checks() stores the inputs in the result object", {

  path <- system.file("extdata", "example", package = "qualitychecks")
  skip_if(path == "", "example data not installed")

  qc <- suppressMessages(quality_checks(path))

  expect_s3_class(qc, "quality_checks")
  expect_named(
    qc$inputs,
    c("data", "data_format", "metadata", "study_design",
      "rating_names", "rating_names_fix", "quest_names", "pmeasure_names")
  )
  expect_s3_class(qc$inputs$data, "data.frame")
  expect_gt(nrow(qc$inputs$data), 0)
  # inputs is not a check: it must not carry a check class
  expect_false(inherits(qc$inputs, "quality_checks_participant_n"))
  # summary still works with the extra element
  expect_no_error(capture.output(summary(qc)))
})
