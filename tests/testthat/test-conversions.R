# Collection to Matrix  --------------------------------------------
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

test_that("Throws error with unnamed collection", {
  sc_unnamed <- example_signature_collection()
  names(sc_unnamed) <- NULL

  expect_error(sig_collection_to_matrix(sc_unnamed), regexp = "must be named")
})

# Collection to Tidy Dataframe --------------------------------------------
test_that("Output is a data frame", {
  sc <- example_signature_collection()
  tidy <- sig_collection_to_tidy(sc)

  expect_s3_class(tidy, "data.frame")
})

test_that("Signature collection returns correct columns", {
  sc <- example_signature_collection()
  tidy <- sig_collection_to_tidy(sc)

  expect_named(tidy, c("signature", "type", "channel", "fraction"))
  expect_true(all(!is.na(tidy$fraction)))
  expect_true(all(tidy$signature %in% names(sc)))
})

test_that("Catalogue collection returns correct columns", {
  cc <- example_catalogue_collection()
  tidy <- sig_collection_to_tidy(cc)

  expect_named(tidy, c("catalogue", "type", "channel", "count", "fraction"))
  expect_true(all(!is.na(tidy$count)))
  expect_true(all(tidy$catalogue %in% names(cc)))
})

test_that("Collection type detection distinguishes signature and catalogue", {
  sc <- example_signature_collection()
  cc <- example_catalogue_collection()

  expect_equal(check_collection_type(sc), "signature")
  expect_equal(check_collection_type(cc), "catalogue")
})

test_that("Single-signature collections are handled", {
  sc <- example_signature_collection()
  sc <- sc[1]  # single element

  tidy <- sig_collection_to_tidy(sc)
  expect_equal(unique(tidy$signature), names(sc))
  expect_equal(ncol(tidy), 4)
})

test_that("Function errors with unnamed elements", {
  sc <- example_signature_collection()
  names(sc) <- NULL

  expect_error(sig_collection_to_tidy(sc), regexp = "must be named")
})

