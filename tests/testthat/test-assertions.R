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


