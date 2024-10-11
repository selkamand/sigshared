library(testthat)

test_that("with_seed sets the random seed temporarily", {

    # Set a global seed
    set.seed(1);
    old_seed <- .Random.seed

    # Make sure we delete .Random.seed when test finishes to reset
    withr::defer(rm(.Random.seed, envir = globalenv()))

    # Generate a number with a specific seed
    result1 <- with_seed(123, {
      runif(1)
    })

    # Ensure that running the same code with the same seed gives the same result
    result2 <- with_seed(123, {
      runif(1)
    })
    expect_equal(result1, result2)

    # Ensure that seed gets reset
    expect_equal(.Random.seed, old_seed)

})
