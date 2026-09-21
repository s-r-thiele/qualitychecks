# This script generates data/info_measures.rda and
# data/info_transformation.rda from the reference CSVs shipped under
# inst/extdata/. Those CSVs remain the single source of truth (they are
# what quality_checks() actually reads at runtime by default); this
# script only makes the same content additionally available as ordinary,
# browsable package datasets (qualitychecks::info_measures,
# qualitychecks::info_transformation).
#
# Not run automatically - only needed when the reference CSVs change.
# After editing inst/extdata/info_measures.csv or
# inst/extdata/info_transformation.csv, re-run this whole script to keep
# the bundled datasets in sync, then re-document (devtools::document()).

info_measures <- utils::read.csv2(
  system.file("extdata", "info_measures.csv", package = "qualitychecks"),
  stringsAsFactors = FALSE
)

info_transformation <- utils::read.csv2(
  system.file("extdata", "info_transformation.csv", package = "qualitychecks"),
  stringsAsFactors = FALSE
)

usethis::use_data(info_measures, overwrite = TRUE)
usethis::use_data(info_transformation, overwrite = TRUE)
