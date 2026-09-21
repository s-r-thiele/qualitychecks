test_that("returns only the questionnaires present in metadata", {

  codebook <- make_test_codebook()
  metadata <- make_test_metadata(questionnaires = c("stais", "stait"))

  result <- fun_which_questionnaires(metadata, codebook)

  expect_setequal(result, c("stais", "stait"))
})

test_that("returns an empty character vector when none are present", {

  codebook <- make_test_codebook()
  metadata <- make_test_metadata()

  result <- fun_which_questionnaires(metadata, codebook)

  expect_length(result, 0)
  expect_type(result, "character")
})

test_that("ignores columns outside the codebook's questionnaire index range", {
  # "fear" is a rating (row 17), not a questionnaire; must be excluded
  # even if it appears with a "quest_" prefix in metadata.
  codebook <- make_test_codebook()
  metadata <- make_test_metadata(questionnaires = "stais")
  metadata$quest_fear <- 1

  result <- fun_which_questionnaires(metadata, codebook)

  expect_setequal(result, "stais")
})
