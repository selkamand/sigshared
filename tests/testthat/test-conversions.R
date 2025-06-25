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
    sig_collection_reformat_list_to_matrix(sc),
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

  mx <- sig_collection_reformat_list_to_matrix(sc)
  expect_true(is.matrix(mx))
  expect_type(mx, "double")
  expect_equal(dim(mx), c(3, 2))
  expect_equal(rownames(mx), c("A[T>C]G", "A[T>C]C", "A[T>C]T"))
  expect_equal(colnames(mx), c("sig1", "sig2"))
  expect_false(any(is.na(mx)))

  types <- attr(mx, "type")
  expect_type(types, "character")
  expect_equal(types, rep("T>C", 3))
})

test_that("Count matrix works for catalogue collection", {
  cc <- example_catalogue_collection()
  mx <- sig_collection_reformat_list_to_matrix(cc, values = "count")
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

  mx <- sig_collection_reformat_list_to_matrix(sc)
  expect_equal(dim(mx), c(3, 1))
  expect_equal(colnames(mx), "sig1")
  expect_false(any(is.na(mx)))
})

test_that("Throws error with unnamed collection", {
  sc_unnamed <- example_signature_collection()
  names(sc_unnamed) <- NULL

  expect_error(sig_collection_reformat_list_to_matrix(sc_unnamed), regexp = "must be named")
})

# Collection to Tidy Dataframe --------------------------------------------
test_that("Output is a data frame", {
  sc <- example_signature_collection()
  tidy <- sig_collection_reformat_list_to_tidy(sc)

  expect_s3_class(tidy, "data.frame")
})

test_that("Signature collection returns correct columns", {
  sc <- example_signature_collection()
  tidy <- sig_collection_reformat_list_to_tidy(sc)

  expect_named(tidy, c("signature", "type", "channel", "fraction"))
  expect_true(all(!is.na(tidy$fraction)))
  expect_true(all(tidy$signature %in% names(sc)))
})

test_that("Catalogue collection returns correct columns", {
  cc <- example_catalogue_collection()
  tidy <- sig_collection_reformat_list_to_tidy(cc)

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

  tidy <- sig_collection_reformat_list_to_tidy(sc)
  expect_equal(unique(tidy$signature), names(sc))
  expect_equal(ncol(tidy), 4)
})

test_that("Function errors with unnamed elements", {
  sc <- example_signature_collection()
  names(sc) <- NULL

  expect_error(sig_collection_reformat_list_to_tidy(sc), regexp = "must be named")
})


# Reformat tidy dataframe to list ---------------------------------------------------
test_that("Function handles tidy signature input", {
  tidy <- example_signature_collection_tidy()
  out <- sig_collection_reformat_tidy_to_list(tidy)

  expect_type(out, "list")
  expect_length(out, 2)
  expect_named(out, c("sig1", "sig2"))
  expect_true(all(c("type", "channel", "fraction") %in% names(out[[1]])))
})

test_that("Function handles tidy catalogue input with count", {
  tidy <- example_catalogue_collection_tidy()
  out <- sig_collection_reformat_tidy_to_list(tidy)

  expect_type(out, "list")
  expect_length(out, 3)
  expect_named(out, c("catalogue1", "catalogue2", "catalogue3"))
  expect_true(all(c("type", "channel", "count", "fraction") %in% names(out[[1]])))
})

test_that("Fails if no ID column is present", {
  tidy <- example_signature_collection_tidy()
  tidy$signature <- NULL
  expect_error(sig_collection_reformat_tidy_to_list(tidy), "Failed to find an obvious ID column")
})

test_that("Fails with multiple ID columns", {
  tidy <- example_signature_collection_tidy()
  tidy$catalogue <- tidy$signature
  expect_error(sig_collection_reformat_tidy_to_list(tidy), "multiple potential ID columns")
})

test_that("Fails if fraction column is missing", {
  tidy <- example_signature_collection_tidy()
  tidy$fraction <- NULL
  expect_error(sig_collection_reformat_tidy_to_list(tidy), "missing 1 required name:.*fraction")
})

test_that("Fails if fraction is non-numeric", {
  tidy <- example_signature_collection_tidy()
  tidy$fraction <- as.character(tidy$fraction)
  expect_error(sig_collection_reformat_tidy_to_list(tidy), "must be numeric")
})

test_that("Fails if count is present but non-numeric", {
  tidy <- example_catalogue_collection_tidy()
  tidy$count <- as.character(tidy$count)
  expect_error(sig_collection_reformat_tidy_to_list(tidy), "must be numeric")
})

test_that("Fails on duplicated (ID, channel) rows", {
  tidy <- example_signature_collection_tidy()
  tidy <- rbind(tidy, tidy[1, ])
  expect_error(sig_collection_reformat_tidy_to_list(tidy), "duplicated")
})

test_that("Output list names match ID column", {
  tidy <- example_signature_collection_tidy()
  out <- sig_collection_reformat_tidy_to_list(tidy)
  expect_equal(sort(names(out)), sort(unique(tidy$signature)))
})

test_that("Each element has expected columns only", {
  tidy <- example_catalogue_collection_tidy()
  out <- sig_collection_reformat_tidy_to_list(tidy)

  expect_true(all(c("type", "channel", "count", "fraction") %in% names(out[[1]])))
  expect_false("catalogue" %in% names(out[[1]]))  # ID col should be removed
})



# Reformat Matrix to List -------------------------------------------------
test_that("Signature matrix with fraction values returns expected structure", {
  mx <- example_signature_collection_matrix()
  expect_silent(out <- sig_collection_reformat_matrix_to_list(mx, values = "fraction"))

  expect_type(out, "list")
  expect_named(out)
  expect_true(all(vapply(out, is.data.frame, logical(1))))
  expect_true(all(c("channel", "type", "fraction") %in% colnames(out[[1]])))
  expect_false("count" %in% colnames(out[[1]]))
})

test_that("Catalogue matrix with counts returns expected structure including computed fractions", {
  mx <- example_catalogue_collection_matrix()
  expect_silent(out <- sig_collection_reformat_matrix_to_list(mx, values = "count"))

  expect_true(all(c("channel", "type", "count", "fraction") %in% colnames(out[[1]])))
  expect_false(any(is.na(out[[1]]$fraction)))
})

test_that("Type attribute is respected and included", {
  mx <- example_signature_collection_matrix()
  attr(mx, "type") <- c("T>C", "C>T", "G>A")
  out <- sig_collection_reformat_matrix_to_list(mx, values = "fraction")
  expect_equal(out[[1]]$type, attr(mx, "type"))
})

test_that("Custom `types` argument overrides matrix attribute", {
  mx <- example_signature_collection_matrix()
  fake_types <- rep("X>X", nrow(mx))
  out <- sig_collection_reformat_matrix_to_list(mx, types = fake_types, values = "fraction")
  expect_equal(out[[1]]$type, fake_types)
})

test_that("No type attribute falls back to channel names and warns", {
  mx <- example_signature_collection_matrix()
  attr(mx, "type") <- NULL
  expect_message(
    out <- sig_collection_reformat_matrix_to_list(mx, values = "fraction", verbose = TRUE),
    "using channel names as types"
  )
  expect_equal(out[[1]]$type, rownames(mx))
})

test_that("Matrix must be numeric with names", {
  mx <- example_signature_collection_matrix()
  mx_character <- as.matrix(as.character(mx))
  expect_error(sig_collection_reformat_matrix_to_list(mx_character), "must be numeric")

  colnames(mx) <- NULL
  expect_error(sig_collection_reformat_matrix_to_list(mx), "colnames.*must not be NULL")

  rownames(mx) <- NULL
  expect_error(sig_collection_reformat_matrix_to_list(mx), "rownames.*must not be NULL")
})

test_that("Duplicate row or column names throw errors", {
  mx <- example_signature_collection_matrix()
  rownames(mx)[1] <- rownames(mx)[2]
  expect_error(sig_collection_reformat_matrix_to_list(mx), "duplicated")

  mx <- example_signature_collection_matrix()
  colnames(mx)[1] <- colnames(mx)[2]
  expect_error(sig_collection_reformat_matrix_to_list(mx), "duplicated")
})

test_that("Warnings emitted for mismatched values argument", {
  mx <- example_signature_collection_matrix()
  expect_message(
    sig_collection_reformat_matrix_to_list(mx, values = "count", verbose = TRUE),
    "contains only fractional values"
  )

  mx <- example_catalogue_collection_matrix()
  expect_message(
    sig_collection_reformat_matrix_to_list(mx, values = "fraction", verbose = TRUE),
    "does not contain fractional values"
  )
})

test_that("Single-column and single-row matrices work", {
  mx <- example_signature_collection_matrix()
  mx_single_col <- mx[, 1, drop = FALSE]
  attr(mx_single_col, "type") <- attr(mx, "type")

  expect_silent(out <- sig_collection_reformat_matrix_to_list(mx_single_col, values = "fraction"))
  expect_length(out, 1)

  mx_single_row <- mx[1, , drop = FALSE]
  attr(mx_single_row, "type") <- attr(mx, "type")
  expect_silent(out <- sig_collection_reformat_matrix_to_list(mx_single_row, values = "fraction"))
  expect_true(all(lengths(lapply(out, \(x) unique(x$channel))) == 1))
})


