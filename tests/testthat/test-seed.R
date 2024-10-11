library(testthat)

test_that("with_temp_seed sets the random seed temporarily", {
  # Save the current random seed
  old_seed <- .Random.seed

  # Generate a number with a specific seed
  result1 <- with_temp_seed(123, {
    runif(1)
  })

  # Ensure that running the same code with the same seed gives the same result
  result2 <- with_temp_seed(123, {
    runif(1)
  })

  expect_equal(result1, result2)

  # Ensure that the global random seed is unchanged
  if (!is.null(old_seed)) {
    expect_identical(.Random.seed, old_seed)
  } else {
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
})

test_that("with_temp_seed restores the global random seed", {
  # Save the current random seed
  old_seed <- .Random.seed


  # Use a temporary seed and run some code
  with_temp_seed(123, {
    runif(1)
  })

  # Generate another random number without setting the seed again
  expect_identical(old_seed, .Random.seed)

})

test_that("with_temp_seed works when no initial random seed exists", {
  # Remove the random seed from the environment if it exists
  if (exists(".Random.seed", envir = .GlobalEnv)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  # Ensure the seed does not exist
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

  # Run code with a temporary seed
  with_temp_seed(123, {
    runif(1)
  })

  # Ensure that the seed has not been recreated after the function execution
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("with_temp_seed evaluates the expression correctly", {
  result <- with_temp_seed(123, {
    sample(1:10, 3)
  })

  expected_result <- with_temp_seed(123, {
    sample(1:10, 3)
  })

  expect_equal(result, expected_result)
})
