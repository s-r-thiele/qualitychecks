test_that("returns only the physiological measures present in metadata", {

  codebook <- make_test_codebook()
  metadata <- make_test_metadata(pmeasures = c("scr", "hr"))

  result <- fun_which_pmeasures(metadata, codebook)

  expect_setequal(result, c("scr", "hr"))
})

test_that("returns an empty character vector when none are present", {

  codebook <- make_test_codebook()
  metadata <- make_test_metadata()

  result <- fun_which_pmeasures(metadata, codebook)

  expect_length(result, 0)
  expect_type(result, "character")
})

test_that("ignores columns outside the codebook's pmeasures index range", {
  # "sex" (row 3, outside 8:12) has a "physio_" prefixed column here on
  # purpose; it must NOT be picked up as a physiological measure.
  codebook <- make_test_codebook()
  metadata <- make_test_metadata(pmeasures = "scr")
  metadata$physio_sex <- 1

  result <- fun_which_pmeasures(metadata, codebook)

  expect_setequal(result, "scr")
})

test_that("ignores detail columns and only matches the bare indicator column", {
  # e.g. "physio_scr_frequency" must not itself be picked up as a measure
  codebook <- make_test_codebook()
  metadata <- make_test_metadata(pmeasures = "scr")
  metadata$physio_scr_frequency <- "experimentwise"

  result <- fun_which_pmeasures(metadata, codebook)

  expect_setequal(result, "scr")
})
