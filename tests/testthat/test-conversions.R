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
    convert_collection_to_matrix(sc),
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

  mx <- convert_collection_to_matrix(sc)
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
  mx <- convert_collection_to_matrix(cc, values = "count")
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

  mx <- convert_collection_to_matrix(sc)
  expect_equal(dim(mx), c(3, 1))
  expect_equal(colnames(mx), "sig1")
  expect_false(any(is.na(mx)))
})

test_that("Throws error with unnamed collection", {
  sc_unnamed <- example_signature_collection()
  names(sc_unnamed) <- NULL

  expect_error(convert_collection_to_matrix(sc_unnamed), regexp = "must be named")
})

# Collection to Tidy Dataframe --------------------------------------------
test_that("Output is a data frame", {
  sc <- example_signature_collection()
  tidy <- convert_collection_to_tidy_dataframe(sc)

  expect_s3_class(tidy, "data.frame")
})

test_that("Signature collection returns correct columns", {
  sc <- example_signature_collection()
  tidy <- convert_collection_to_tidy_dataframe(sc)

  expect_named(tidy, c("signature", "type", "channel", "fraction"))
  expect_true(all(!is.na(tidy$fraction)))
  expect_true(all(tidy$signature %in% names(sc)))
})

test_that("Catalogue collection returns correct columns", {
  cc <- example_catalogue_collection()
  tidy <- convert_collection_to_tidy_dataframe(cc)

  expect_named(tidy, c("catalogue", "type", "channel", "count", "fraction"))
  expect_true(all(!is.na(tidy$count)))
  expect_true(all(tidy$catalogue %in% names(cc)))
})

test_that("Collection type detection distinguishes signature and catalogue", {
  sc <- example_signature_collection()
  cc <- example_catalogue_collection()

  expect_equal(infer_collection_type(sc), "signature")
  expect_equal(infer_collection_type(cc), "catalogue")
})

test_that("Single-signature collections are handled", {
  sc <- example_signature_collection()
  sc <- sc[1]  # single element

  tidy <- convert_collection_to_tidy_dataframe(sc)
  expect_equal(unique(tidy$signature), names(sc))
  expect_equal(ncol(tidy), 4)
})

test_that("Function errors with unnamed elements", {
  sc <- example_signature_collection()
  names(sc) <- NULL

  expect_error(convert_collection_to_tidy_dataframe(sc), regexp = "must be named")
})


# Reformat tidy dataframe to list ---------------------------------------------------
test_that("Function handles tidy signature input", {
  tidy <- example_signature_collection_tidy()
  out <- convert_tidy_dataframe_to_collection(tidy)

  expect_type(out, "list")
  expect_length(out, 2)
  expect_named(out, c("sig1", "sig2"))
  expect_true(all(c("type", "channel", "fraction") %in% names(out[[1]])))
})

test_that("Function handles tidy catalogue input with count", {
  tidy <- example_catalogue_collection_tidy()
  out <- convert_tidy_dataframe_to_collection(tidy)

  expect_type(out, "list")
  expect_length(out, 3)
  expect_named(out, c("catalogue1", "catalogue2", "catalogue3"))
  expect_true(all(c("type", "channel", "count", "fraction") %in% names(out[[1]])))
})

test_that("Fails if no ID column is present", {
  tidy <- example_signature_collection_tidy()
  tidy$signature <- NULL
  expect_error(convert_tidy_dataframe_to_collection(tidy), "Failed to find an obvious ID column")
})

test_that("Fails with multiple ID columns", {
  tidy <- example_signature_collection_tidy()
  tidy$catalogue <- tidy$signature
  expect_error(convert_tidy_dataframe_to_collection(tidy), "multiple potential ID columns")
})

test_that("Fails if fraction column is missing", {
  tidy <- example_signature_collection_tidy()
  tidy$fraction <- NULL
  expect_error(convert_tidy_dataframe_to_collection(tidy), "missing 1 required name:.*fraction")
})

test_that("Fails if fraction is non-numeric", {
  tidy <- example_signature_collection_tidy()
  tidy$fraction <- as.character(tidy$fraction)
  expect_error(convert_tidy_dataframe_to_collection(tidy), "must be numeric")
})

test_that("Fails if count is present but non-numeric", {
  tidy <- example_catalogue_collection_tidy()
  tidy$count <- as.character(tidy$count)
  expect_error(convert_tidy_dataframe_to_collection(tidy), "must be numeric")
})

test_that("Fails on duplicated (ID, channel) rows", {
  tidy <- example_signature_collection_tidy()
  tidy <- rbind(tidy, tidy[1, ])
  expect_error(convert_tidy_dataframe_to_collection(tidy), "duplicated")
})

test_that("Output list names match ID column", {
  tidy <- example_signature_collection_tidy()
  out <- convert_tidy_dataframe_to_collection(tidy)
  expect_equal(sort(names(out)), sort(unique(tidy$signature)))
})

test_that("Each element has expected columns only", {
  tidy <- example_catalogue_collection_tidy()
  out <- convert_tidy_dataframe_to_collection(tidy)

  expect_true(all(c("type", "channel", "count", "fraction") %in% names(out[[1]])))
  expect_false("catalogue" %in% names(out[[1]]))  # ID col should be removed
})

