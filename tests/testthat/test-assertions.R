test_that("assert_signature works", {

  expect_error(assert_signature(example_valid_signature()), regexp = NA)
})


test_that("assert_signature_collection works", {

  expect_error(assert_signature_collection(example_valid_signature_collection()), regexp = NA)

})

test_that("assert_decomposition works", {

  expect_error(assert_decomposition(example_valid_decomposition()), regexp = NA)

})

test_that("assert_decomposition_collection works", {

  expect_error(assert_decomposition_collection(example_valid_decomposition_collection()), regexp = NA)

})



test_that("assert_cohort_analysis works", {

  expect_error(assert_cohort_analysis(example_valid_cohort_analysis()), regexp = NA)

})
