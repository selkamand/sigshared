
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

