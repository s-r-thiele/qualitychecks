test_that("returns only the ratings present in metadata", {

  codebook <- make_test_codebook()
  metadata <- make_test_metadata(ratings = c("fear", "us_intrat"))

  result <- fun_which_ratings(metadata, codebook)

  expect_setequal(result, c("fear", "us_intrat"))
})

test_that("returns an empty character vector when none are present", {

  codebook <- make_test_codebook()
  metadata <- make_test_metadata()

  result <- fun_which_ratings(metadata, codebook)

  expect_length(result, 0)
  expect_type(result, "character")
})

test_that("ignores columns outside the codebook's ratings index range", {
  # "stais" is a questionnaire (row 24), not a rating; must be excluded
  # even if it appears with a "rating_" prefix in metadata.
  codebook <- make_test_codebook()
  metadata <- make_test_metadata(ratings = "fear")
  metadata$rating_stais <- 1

  result <- fun_which_ratings(metadata, codebook)

  expect_setequal(result, "fear")
})

test_that("does not strip the 'us_' prefix itself (that is fix_rating_names()'s job)", {

  codebook <- make_test_codebook()
  metadata <- make_test_metadata(ratings = "us_intrat")

  result <- fun_which_ratings(metadata, codebook)

  expect_setequal(result, "us_intrat")
})
