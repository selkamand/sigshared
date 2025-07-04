
# Compute Fraction --------------------------------------------------------


test_that("standard positive counts sum to 1", {
  counts <- c(10, 20, 30)
  fracs <- compute_fraction(counts)
  expect_type(fracs, "double")
  expect_equal(sum(fracs), 1)
  expect_equal(fracs, counts / sum(counts))
})

test_that("negative counts are converted to absolute values", {
  counts <- c(-5, 5, -10)
  fracs <- compute_fraction(counts)
  expect_equal(sum(fracs), 1)
  expect_equal(fracs, abs(counts) / sum(abs(counts)))
})

test_that("mixed zero and nonzero counts handled correctly", {
  counts <- c(0, 0, 5, 5)
  fracs <- compute_fraction(counts)
  expect_equal(fracs, c(0, 0, 0.5, 0.5))
})

test_that("all-zero counts with default zero.method returns zeros", {
  counts <- c(0, 0, 0, 0)
  fracs <- compute_fraction(counts)
  expect_equal(fracs, rep(0, length(counts)))
  expect_equal(sum(fracs), 0)
})

test_that("all-zero counts with zero.method = 'zero' returns zeros", {
  counts <- c(0, 0, 0)
  fracs <- compute_fraction(counts, zero.method = "zero")
  expect_equal(fracs, c(0, 0, 0))
})

test_that("all-zero counts with zero.method = 'uniform' returns uniform distribution", {
  counts <- c(0, 0, 0, 0, 0)
  fracs <- compute_fraction(counts, zero.method = "uniform")
  expect_equal(fracs, rep(1/length(counts), length(counts)))
  expect_equal(sum(fracs), 1)
})

test_that("single-element vector > 0 returns 1", {
  expect_equal(compute_fraction(5), 1)
  expect_equal(compute_fraction(-7), 1)
})

test_that("single-element zero vector respects zero.method", {
  expect_equal(compute_fraction(0), 0)
  expect_equal(compute_fraction(0, zero.method = "uniform"), 1)
})

test_that("invalid zero.method argument errors", {
  expect_error(compute_fraction(c(0,0,0), zero.method = "invalid"),
               "must be one of")
})

test_that("non-numeric input errors", {
  expect_error(compute_fraction(c("a", "b", "c")),
               "must be numeric")
})

test_that("counts containing NA throw an error", {
  counts <- c(NA, 1, 2)
  expect_error(compute_fraction(counts), regexp = "must have no missing values")
})

test_that("counts containing Inf throw error", {
  counts <- c(Inf, 1)
  expect_error(compute_fraction(counts), regexp = "must have no infinite values")
})

test_that("length-zero input returns zero-length numeric vector", {
  expect_equal(compute_fraction(numeric(0)), numeric(0))
})

test_that("`validate = FALSE` skips input checks", {

  # Would normally throw an assertion but here should not
  expect_no_error(
    compute_fraction(c(Inf, 1, 2), validate = FALSE)
  )

})
