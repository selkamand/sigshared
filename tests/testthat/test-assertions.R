# Test assert_signature
test_that("assert_signature works", {

  expect_error(assert_signature(example_signature()), regexp = NA)

  # Duplicated signature
  expect_error(assert_signature(example_invalid_signature_channeldup()), regexp = "duplicat")

  # Non-Character channel
  expect_error(assert_signature(example_invalid_signature_factor_channel()), regexp = "character")

  # Non-Character Channel
  expect_error(assert_signature(example_invalid_signature_numeric_channel()), regexp = "character", fixed=TRUE)

  # Missing Data
  expect_error(assert_signature(example_invalid_signature_missing()), regexp = "missing (NA)", fixed=TRUE)

  # Fractions sum to >1
  expect_error(assert_signature(example_invalid_signature_fraction_sum()), regexp = "Sum of fractions must be approximately equal to 1")

  # Fractions sum to > 1 when only summing to <= 1 is allowed
  expect_error(assert_signature(example_invalid_signature_fraction_sum(), must_sum_to_one = FALSE), regexp = "Sum of fractions must be less than or equal to 1")

  # Fractions sum to <= 1 when summing to <= 1 is allowed
  expect_error(assert_signature(example_invalid_signature_fraction_sum_below_one(), must_sum_to_one = FALSE), regexp = NA)

  # Fractions include negative values
  expect_error(assert_signature(example_invalid_signature_negative_fraction()), regexp = "Found negative fractions")
})

# Test assert_signature_collection
test_that("assert_signature_collection works", {

  # Works as expected
  expect_error(assert_signature_collection(example_signature_collection()), regexp = NA)

  # Invalid collection type (not a list)
  expect_error(assert_signature_collection(example_invalid_signature_collection_not_list()), regexp = "valid signature collection: Collections must be of type list")

  # Empty collection
  expect_error(assert_signature_collection(example_invalid_signature_collection_empty()), regexp = "valid signature collection: No signatures are present in the collection")

  # Duplicated signature names
  expect_error(assert_signature_collection(example_invalid_signature_collection_duplicated_names()), regexp = "valid signature collection: found duplicated signature names")

  # Invalid signature within the collection
  expect_error(assert_signature_collection(example_invalid_signature_collection_invalid_signature()), regexp = "valid signature collection. Signature sig2 fails the following check")

})



# Test assert_catalogue
test_that("assert_catalogue works", {

  expect_error(assert_catalogue(example_catalogue()), regexp = NA)

  # Invalid fraction values
  expect_error(assert_catalogue(example_invalid_catalogue_nonsensical_fraction()), regexp = "NOT a valid catalogue")

  # Allows empty catalogues
  expect_error(assert_catalogue(example_catalogue_empty()), regexp = NA)

  # Missing Data
  expect_error(assert_catalogue(example_invalid_catalogue_missing()), regexp = "found missing (NA) values", fixed = TRUE)

  # Invalid column name
  expect_error(assert_catalogue(example_invalid_catalogue_colname_typo()), regexp = "must contain the following columns: [type]", fixed = TRUE)

  # Duplicated channel
  expect_error(assert_catalogue(example_invalid_catalogue_channeldup()), regexp = "duplicat")
})


# Test assert_catalogue_collection
test_that("assert_catalogue_collection works", {

  # Works as expected
  expect_error(assert_catalogue_collection(example_catalogue_collection()), regexp = NA)

  # Invalid collection type (not a list)
  expect_error(assert_catalogue_collection(example_invalid_catalogue_collection_not_list()), regexp = "valid catalogue collection: Collections must be of type list")

  # Empty collection
  expect_error(assert_catalogue_collection(example_invalid_catalogue_collection_empty()), regexp = "valid catalogue collection: No catalogues are present in the collection")

  # Duplicated catalogue names
  expect_error(assert_catalogue_collection(example_invalid_catalogue_collection_duplicated_names()), regexp = "valid catalogue collection: found duplicated catalogue names")

  # Invalid catalogue within the collection
  expect_error(assert_catalogue_collection(example_invalid_catalogue_in_collection()), regexp = "not a valid catalogue collection")
})


# Test assert_signature_annotations
test_that("assert_signature_annotations works", {

  # Works as expected
  expect_error(assert_signature_annotations(example_annotations()), regexp = NA)

  # Works as expected even with required_signatures supplied
  expect_error(assert_signature_annotations(example_annotations(), required_signatures = c("sig1", "sig2")), regexp = NA)

  # Invalid signature column name
  expect_error(assert_signature_annotations(example_invalid_annotations_sig_colname()), regexp = "must contain the following columns: [signature]", fixed = TRUE)

  # Duplicated signatures
  expect_error(assert_signature_annotations(example_invalid_annotations_sig_duplicated()), regexp = "duplicat")

  # Missing Required Signatures
  expect_error(assert_signature_annotations(example_annotations(), required_signatures = c("sig1", "sig2", "sig3")), regexp = "Missing annotations")
})

# Test assert_cohort_analysis
test_that("assert_cohort_analysis works", {

  # Valid data works as expected
  expect_no_error(assert_cohort_analysis(example_cohort_analysis()))

  # Invalid contribution values
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_contribution()), regexp = "must be less than or equal to 1, not 1.2")

  # Negative contribution
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_negative_contribution()), regexp = "Found negative contribution")

  # Negative contribution_absolute
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_negative_contribution_absolute()), regexp = "Found negative contribution_absolute")

  # Invalid p_value column
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_pvalue()), regexp = "numeric, not character")

  # Missing Data
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_missing()), regexp = "ound missing (NA) values", fixed = TRUE)
})


# Test assert_bootstraps
test_that("assert_bootstraps works", {

  # Expect the valid bootstrap to pass without errors
  expect_no_error(assert_bootstraps(example_bootstraps()))

  # Invalid contribution values (greater than 100%)
  expect_error(assert_bootstraps(example_invalid_bootstraps_contribution()), regexp = "exceed 100%")

  # Negative contribution
  expect_error(assert_bootstraps(example_invalid_bootstraps_negative_contribution()), regexp = "Found negative contribution")

  # Negative contribution_absolute
  expect_error(assert_bootstraps(example_invalid_bootstraps_negative_contribution_absolute()), regexp = "Found negative contribution_absolute")

  # Missing data
  expect_error(assert_bootstraps(example_invalid_bootstraps_missing()), regexp = "Found missing (NA) values", fixed = TRUE)

  # 'bootstrap' column as character or factor is accepted
  bootstrap_df <- example_bootstraps()
  bootstrap_df[["bootstrap"]] <- as.character(bootstrap_df[["bootstrap"]])
  expect_no_error(assert_bootstraps(bootstrap_df))

  bootstrap_df[["bootstrap"]] <- factor(bootstrap_df[["bootstrap"]])
  expect_no_error(assert_bootstraps(bootstrap_df))

})

# Test assert_model
test_that("assert_model works", {

  # Valid model should pass without errors
  expect_no_error(assert_model(example_model()))

  # Empty model should pass without errors (unless we specifically don't allow it)
  expect_no_error(assert_model(example_model_empty()))
  expect_error(assert_model(example_model_empty(), allow_empty = FALSE), "Vector is empty")


  # Non-numeric vector (invalid model)
  expect_error(assert_model(example_invalid_non_numeric_model()), regexp = "Must be a numeric vector")

  # Model where contributions sum to more than 1
  expect_error(assert_model(example_invalid_over_one_model()), regexp = "Contributions of all signatures in model should add up to <= 1")

  # Model without any names (completely unnamed elements)
  expect_error(assert_model(example_invalid_unnamed_model()), regexp = "Must be a named vector")

  # Model with some unnamed elements (mix of named and unnamed)
  expect_error(assert_model(example_invalid_mixed_names_model()), regexp = "All elements must be named")

  # Model with duplicate signatures
  expect_error(assert_model(example_invalid_duplicate_signatures_model()), regexp = "duplicate contribution")

  # Model with negative values (invalid contributions)
  expect_error(assert_model(example_invalid_negative_model()), regexp = "Contributions of all signatures in model must be non-negative")

  # Model with missing signatures from the signature collection
  signature_collection <- list("Signature1" = 0.5, "Signature2" = 0.5) # Not a real sig collection but doesn't assertion doesn't care so long as names represent valid signatures
  model_with_invalid_sigs <- c("Sig1" = 0.3, "Sig2" = 0.7)
  expect_error(assert_model(model_with_invalid_sigs, signature_collection), regexp = "invalid signature")

  # Model with NO missing signatures from the signature collection
  model_with_invalid_sigs <- c("Signature1" = 0.3, "Signature2" = 0.7)
  expect_no_error(assert_model(model_with_invalid_sigs, signature_collection))

  # Model with all valid signatures from the signature collection
  valid_signature_model <- c("Signature1" = 0.5, "Signature2" = 0.5)
  expect_no_error(assert_model(valid_signature_model, signature_collection))

})

# Test assert_model
test_that("assert_cohort_metadata works", {

  # Valid cohort metadata passes without error
  expect_no_error(assert_cohort_metadata(example_cohort_metadata()))

  # Valid but empty cohort metadata passes without error
  expect_no_error(assert_cohort_metadata(example_cohort_metadata_empty()))

  # Throws error when duplicate samples present
  expect_error(assert_cohort_metadata(example_invalid_cohort_metadata_duplicate_sample()), regexp = "duplicated sample.*sample1, sample2")

  # Throws error when disease column is missing
  expect_error(assert_cohort_metadata(example_invalid_cohort_metadata_missing_disease()), regexp = "must contain the following columns: [disease]", fixed=TRUE)

  # Throws error when sample column is missing
  expect_error(assert_cohort_metadata(example_invalid_cohort_metadata_missing_sample()), regexp = "must contain the following columns: [sample]", fixed=TRUE)

  # Throws error when sample column is wrong type
  expect_error(assert_cohort_metadata(example_invalid_cohort_metadata_sample_wrong_type()), regexp = "must be of type character or factor, not integer", fixed=TRUE)

  # Throws error when disease column is wrong type
  expect_error(assert_cohort_metadata(example_invalid_cohort_metadata_sample_wrong_type_disease()), regexp = "must be of type character or factor, not integer", fixed=TRUE)

  # Throws error when sample column includes missing values
  expect_error(assert_cohort_metadata(example_invalid_cohort_metadata_na_in_sample()), regexp = "found 2 missing (NA) values in the sample column", fixed=TRUE)
})


