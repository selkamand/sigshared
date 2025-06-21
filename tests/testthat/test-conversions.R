test_that("Custom error on mismatched channels", {
  sc <- list(
    sig1 = data.frame(
      channel = c("A[T>C]G", "A[T>C]C", "A[T>C]T"),
      type = rep("T>C", 3L),
      fraction = c(0.4, 0.1, 0.5)
    ),
    sig2 = data.frame(
      channel = c("G[T>A]C", "A[T>C]C", "A[T>C]T"), # mismatched
      type = rep("T>C", 3L),
      fraction = c(0.3, 0.4, 0.3)
    )
  )
  expect_error(
    sig_collection_to_matrix(sc),
    "All signatures must have identical channel ordering"
  )
})

test_that("Fraction matrix returns correct structure", {
  sc <- list(
    sig1 = data.frame(
      channel = c("A[T>C]G", "A[T>C]C", "A[T>C]T"),
      type = rep("T>C", 3L),
      fraction = c(0.4, 0.1, 0.5)
    ),
    sig2 = data.frame(
      channel = c("A[T>C]G", "A[T>C]C", "A[T>C]T"),
      type = rep("T>C", 3L),
      fraction = c(0.3, 0.3, 0.4)
    )
  )

  mx <- sig_collection_to_matrix(sc)
  expect_true(is.matrix(mx))
  expect_type(mx, "double")
  expect_equal(dim(mx), c(3, 2))
  expect_equal(rownames(mx), c("A[T>C]G", "A[T>C]C", "A[T>C]T"))
  expect_equal(colnames(mx), c("sig1", "sig2"))
  expect_false(any(is.na(mx)))

  types <- attr(mx, "types")
  expect_type(types, "character")
  expect_equal(names(types), rep("T>C", 3))
  expect_equal(unname(types), c("A[T>C]G", "A[T>C]C", "A[T>C]T"))
})

test_that("Count matrix works for catalogue collection", {
  cc <- example_catalogue_collection()
  mx <- sig_collection_to_matrix(cc, values = "count")
  expect_type(mx, "double")
  expect_false(any(is.na(mx)))
})

test_that("Single-signature collections return single-column matrix", {
  sc <- list(
    sig1 = data.frame(
      channel = c("A[T>C]G", "A[T>C]C", "A[T>C]T"),
      type = rep("T>C", 3L),
      fraction = c(0.2, 0.3, 0.5)
    )
  )

  mx <- sig_collection_to_matrix(sc)
  expect_equal(dim(mx), c(3, 1))
  expect_equal(colnames(mx), "sig1")
  expect_false(any(is.na(mx)))
})
