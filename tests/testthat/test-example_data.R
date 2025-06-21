
# Test Colo829 data -------------------------------------------------------
test_that("example_bootstraps_colo829 works", {
  expect_no_error(example_bootstraps_colo829())
  expect_s3_class(example_bootstraps_colo829(), "data.frame")
  expect_no_error(assert_bootstraps(example_bootstraps_colo829()))
})

test_that("example_catalogue_colo829 works", {
  expect_no_error(example_catalogue_colo829())
  expect_s3_class(example_catalogue_colo829(), "data.frame")
  expect_no_error(assert_catalogue(example_catalogue_colo829()))
})


test_that("All exported example_* functions run without error and return expected types", {
  # Signatures
  expect_no_error(example_signature())
  expect_s3_class(example_signature(), "data.frame")

  expect_no_error(example_signature_empty())
  expect_s3_class(example_signature_empty(), "data.frame")

  # Signature Collections
  expect_no_error(example_signature_collection())
  expect_type(example_signature_collection(), "list")

  expect_no_error(example_signature_collection_tidy())
  expect_s3_class(example_signature_collection_tidy(), "data.frame")

  # Catalogues
  expect_no_error(example_catalogue())
  expect_s3_class(example_catalogue(), "data.frame")

  expect_no_error(example_catalogue_empty())
  expect_s3_class(example_catalogue_empty(), "data.frame")

  # Catalogue Collections
  expect_no_error(example_catalogue_collection())
  expect_type(example_catalogue_collection(), "list")

  expect_no_error(example_catalogue_collection_tidy())
  expect_s3_class(example_catalogue_collection_tidy(), "data.frame")

  # Annotations
  expect_no_error(example_annotations())
  expect_s3_class(example_annotations(), "data.frame")

  # Cohort
  expect_no_error(example_cohort_analysis())
  expect_s3_class(example_cohort_analysis(), "data.frame")

  # Bootstraps
  expect_no_error(example_bootstraps())
  expect_s3_class(example_bootstraps(), "data.frame")

  expect_no_error(example_bootstraps_empty())
  expect_s3_class(example_bootstraps_empty(), "data.frame")

  # Model
  expect_no_error(example_model())
  expect_type(example_model(), "double")  # named numeric vector

  expect_no_error(example_model_empty())
  expect_type(example_model_empty(), "double")

  # Cohort Metadata
  expect_no_error(example_cohort_metadata())
  expect_s3_class(example_cohort_metadata(), "data.frame")

  expect_no_error(example_cohort_metadata_empty())
  expect_s3_class(example_cohort_metadata_empty(), "data.frame")

  # UMAP
  expect_no_error(example_umap())
  expect_s3_class(example_umap(), "data.frame")

  # Similarity Against Cohort
  expect_no_error(example_similarity_against_cohort())
  expect_s3_class(example_similarity_against_cohort(), "data.frame")

  # Colo829 examples
  expect_no_error(example_bootstraps_colo829())
  expect_s3_class(example_bootstraps_colo829(), "data.frame")
  expect_no_error(assert_bootstraps(example_bootstraps_colo829()))

  expect_no_error(example_catalogue_colo829())
  expect_s3_class(example_catalogue_colo829(), "data.frame")
  expect_no_error(assert_catalogue(example_catalogue_colo829()))
})
