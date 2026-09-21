test_that("passes when every rating has at least one matching column", {

  data <- data.frame(
    participant_id = 1:3,
    fear_0_10_experimentwise_raw_pre____ = c(1, 2, 3),
    val_0_10_experimentwise_raw_pre____ = c(1, 2, 3)
  )

  result <- fun_check_ratings(data, metadata = NULL, rating_names = c("fear", "val"), data_format = "wide")

  expect_true(result$passed)
  expect_equal(result$pass_ratings, c(TRUE, TRUE))
  expect_match(result$message, "All ratings")
})

test_that("partially fails and reports the count when only some ratings are present", {

  data <- data.frame(participant_id = 1:3, fear_0_10_experimentwise_raw_pre____ = c(1, 2, 3))

  result <- fun_check_ratings(data, metadata = NULL, rating_names = c("fear", "val"), data_format = "wide")

  expect_false(result$passed)
  expect_equal(result$pass_ratings, c(TRUE, FALSE))
  expect_match(result$message, "1 of 2 ratings")
})

test_that("fully fails and reports so when no rating has a matching column", {

  data <- data.frame(participant_id = 1:3, age = c(20, 30, 40))

  result <- fun_check_ratings(data, metadata = NULL, rating_names = c("fear", "val"), data_format = "wide")

  expect_false(result$passed)
  expect_match(result$message, "No variables were found")
})

test_that("long format is not yet supported and errors explicitly", {

  data <- data.frame(participant_id = 1:3)

  expect_error(
    fun_check_ratings(data, metadata = NULL, rating_names = "fear", data_format = "long"),
    "Long format for data not yet implemented"
  )
})
