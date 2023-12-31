# Test assert_signature
test_that("assert_signature works", {

  expect_error(assert_signature(example_valid_signature()), regexp = NA)

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

  # Fractions include negative values
  expect_error(assert_signature(example_invalid_signature_negative_fraction()), regexp = "Found negative fractions")
})

# Test assert_signature_collection
test_that("assert_signature_collection works", {

  # Works as expected
  expect_error(assert_signature_collection(example_valid_signature_collection()), regexp = NA)

  # Invalid collection type (not a list)
  expect_error(assert_signature_collection(example_invalid_signature_collection_not_list()), regexp = "valid signature collection: Collections must be of type list")

  # Empty collection
  expect_error(assert_signature_collection(example_invalid_signature_collection_empty()), regexp = "valid signature collection: No signatures are present in the collection")

  # Duplicated signature names
  expect_error(assert_signature_collection(example_invalid_signature_collection_duplicated_names()), regexp = "valid signature collection: found duplicated signature names")

  # Invalid signature within the collection
  expect_error(assert_signature_collection(example_invalid_signature_collection_invalid_signature()), regexp = "valid signature collection. Signature sig2 fails the following check")

})



# Test assert_decomposition
test_that("assert_decomposition works", {

  expect_error(assert_decomposition(example_valid_decomposition()), regexp = NA)

  # Invalid fraction values
  expect_error(assert_decomposition(example_invalid_decomposition_nonsensical_fraction()), regexp = "NOT a valid decomposition")

  # Missing Data
  expect_error(assert_decomposition(example_invalid_decomposition_missing()), regexp = "found missing (NA) values", fixed = TRUE)

  # Invalid column name
  expect_error(assert_decomposition(example_invalid_decomposition_colname_typo()), regexp = "must contain the following columns: [type]", fixed = TRUE)

  # Duplicated channel
  expect_error(assert_decomposition(example_invalid_decomposition_channeldup()), regexp = "duplicat")
})


# Test assert_decomposition_collection
test_that("assert_decomposition_collection works", {

  # Works as expected
  expect_error(assert_decomposition_collection(example_valid_decomposition_collection()), regexp = NA)

  # Invalid collection type (not a list)
  expect_error(assert_decomposition_collection(example_invalid_decomposition_collection_not_list()), regexp = "valid decomposition collection: Collections must be of type list")

  # Empty collection
  expect_error(assert_decomposition_collection(example_invalid_decomposition_collection_empty()), regexp = "valid decomposition collection: No decompositions are present in the collection")

  # Duplicated decomposition names
  expect_error(assert_decomposition_collection(example_invalid_decomposition_collection_duplicated_names()), regexp = "valid decomposition collection: found duplicated decomposition names")

  # Invalid decomposition within the collection
  expect_error(assert_decomposition_collection(example_invalid_decomposition_in_collection()), regexp = "not a valid decomposition collection")
})


# Test assert_signature_annotations
test_that("assert_signature_annotations works", {

  # Works as expected
  expect_error(assert_signature_annotations(example_valid_annotations()), regexp = NA)

  # Works as expected even with required_signatures supplied
  expect_error(assert_signature_annotations(example_valid_annotations(), required_signatures = c("sig1", "sig2")), regexp = NA)

  # Invalid signature column name
  expect_error(assert_signature_annotations(example_invalid_annotations_sig_colname()), regexp = "must contain the following columns: [signature]", fixed = TRUE)

  # Duplicated signatures
  expect_error(assert_signature_annotations(example_invalid_annotations_sig_duplicated()), regexp = "duplicat")

  # Missing Required Signatures
  expect_error(assert_signature_annotations(example_valid_annotations(), required_signatures = c("sig1", "sig2", "sig3")), regexp = "Missing annotations")
})

# Test assert_cohort_analysis
test_that("assert_cohort_analysis works", {

  expect_error(assert_cohort_analysis(example_valid_cohort_analysis()), regexp = NA)

  # Invalid contribution values
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_contribution()), regexp = "must be less than or equal to 1, not 1.2")

  # Negative contribution
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_negative_contribution()), regexp = "Found negative contribution")


  # Negative contribution_absolute
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_negative_contribution_absolute()), regexp = "Found negative contribution_absolute")

  # Invalid bootstraps column
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_bootstraps()), regexp = "number of bootstraps")

  # Missing Data
  expect_error(assert_cohort_analysis(example_invalid_cohort_analysis_missing()), regexp = "ound missing (NA) values", fixed = TRUE)
})
