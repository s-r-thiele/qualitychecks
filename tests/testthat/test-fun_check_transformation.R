# A small, reusable expected-values lookup table matching the structure
# fun_check_transformation() expects from info_transformation after
# parse_numeric_list_columns() has run on it: list-columns "range",
# "mean", "sd", each holding a c(min, max) vector per transformation.
make_test_info_transformation <- function() {
  df <- data.frame(transformation = c("raw", "z", "None"), stringsAsFactors = FALSE)
  df$range <- list(c(-1000, 1000), c(-5, 5), c(-1000, 1000))
  df$mean  <- list(c(-1000, 1000), c(-5, 5), c(-1000, 1000))
  df$sd    <- list(c(0, 1000), c(0, 5), c(0, 1000))
  df
}

test_that("passes when observed range/mean/sd fit the specified transformation", {

  info_transformation <- make_test_info_transformation()
  metadata <- data.frame(rating_fear_transformation = "raw", stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:5, fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3, 4, 5))

  result <- fun_check_transformation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    pmeasure_names = character(0),
    info_transformation = info_transformation, data_format = "wide"
  )

  expect_true(result$passed)
  expect_true(result$fear$pass_global)
})

test_that("fails when observed values do not fit the specified transformation", {

  info_transformation <- make_test_info_transformation()
  # "z" implies a standardized scale (expected sd range 0-5); raw values of
  # 100-500 clearly are not z-standardized
  metadata <- data.frame(rating_fear_transformation = "z", stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:5, fear_0_10_experimentwise_z_pre______ = c(100, 200, 300, 400, 500))

  result <- fun_check_transformation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    pmeasure_names = character(0),
    info_transformation = info_transformation, data_format = "wide"
  )

  expect_false(result$passed)
  expect_false(result$fear$pass_global)
})

test_that("an unspecified (NA) transformation falls back to the 'None' lookup row", {

  info_transformation <- make_test_info_transformation()
  metadata <- data.frame(rating_fear_transformation = NA_character_, stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:5, fear_0_10_experimentwise_raw_pre______ = c(1, 2, 3, 4, 5))

  result <- fun_check_transformation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    pmeasure_names = character(0),
    info_transformation = info_transformation, data_format = "wide"
  )

  expect_equal(result$fear$transformations[[1]]$name, "None")
  expect_true(result$passed)
})

test_that("a missing column in the data is reported rather than erroring", {

  info_transformation <- make_test_info_transformation()
  metadata <- data.frame(rating_fear_transformation = "raw", stringsAsFactors = FALSE)
  # rating_fear_* is missing entirely
  data <- data.frame(participant_id = 1:5)

  result <- fun_check_transformation(
    data, metadata,
    rating_names = "fear", rating_names_fix = "fear",
    pmeasure_names = character(0),
    info_transformation = info_transformation, data_format = "wide"
  )

  expect_false(result$passed)
})

test_that("long format is not yet supported and errors explicitly", {

  info_transformation <- make_test_info_transformation()
  metadata <- data.frame(rating_fear_transformation = "raw", stringsAsFactors = FALSE)
  data <- data.frame(participant_id = 1:5)

  expect_error(
    fun_check_transformation(
      data, metadata,
      rating_names = "fear", rating_names_fix = "fear",
      pmeasure_names = character(0),
      info_transformation = info_transformation, data_format = "long"
    ),
    "Long format for data not yet implemented"
  )
})
