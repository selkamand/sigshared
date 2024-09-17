
# Rename ------------------------------------------------------------------


test_that("rename renames a single column correctly", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
  expect_equal(result$new_a, df$a)
})

test_that("rename renames multiple columns correctly", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  namemap <- c(new_a = "a", new_c = "c")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b", "new_c"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$new_c, df$c)
})

test_that("rename leaves unspecified columns unchanged", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
  expect_equal(result$b, df$b)
})

test_that("rename maintains the original column order", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  namemap <- c(new_b = "b")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("a", "new_b", "c"))
})

test_that("rename handles namemap provided as a named list", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- list(new_a = "a")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
})

test_that("rename throws an error if a specified column does not exist", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a", new_c = "c")
  expect_error(
    rename(df, namemap),
    regexp = "could not find column/s named"
  )
})

test_that("rename throws an error when namemap is empty", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c()
  expect_error(
    rename(df, namemap),
    regexp = "namemap must be a named vector"
  )
})

test_that("rename throws an error when namemap is NULL", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- NULL
  expect_error(
    rename(df, namemap),
    regexp = "namemap must be a named vector"
  )
})

test_that("rename throws an error when namemap elements are unnamed", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a")
  expect_error(
    rename(df, namemap),
    regexp = "namemap must be a named vector"
  )
})

test_that("rename throws an error when namemap is not a named vector", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a", "b")
  expect_error(
    rename(df, namemap),
    regexp = "namemap must be a named vector"
  )
})

test_that("rename throws an error when namemap has empty names", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a", new_b = "b")
  expect_error(
    rename(df, namemap),
    regexp = "all elements in namemap must be named"
  )
})

test_that("rename handles duplicate new column names in namemap", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(dup_name = "a", dup_name = "b")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("dup_name", "dup_name"))
  expect_equal(result[[1]], df$a)
  expect_equal(result[[2]], df$b)
})



# Select ------------------------------------------------------------------

test_that("select returns correct columns", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  result <- select(df, c("a", "c"))
  expect_equal(colnames(result), c("a", "c"))
  expect_equal(ncol(result), 2)
  expect_equal(result$a, df$a)
  expect_equal(result$c, df$c)
})

test_that("select maintains the original data types", {
  df <- data.frame(
    num = 1:5,
    char = letters[1:5],
    factor = factor(letters[1:5]),
    stringsAsFactors = FALSE
  )
  result <- select(df, c("num", "factor"))
  expect_type(result$num, "integer")
  expect_s3_class(result$factor, "factor")
})

test_that("select with non-existent columns throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    select(df, c("a", "d")),
    regexp = "Could not find column/s: \\[d\\]"
  )
})

test_that("select with duplicate columns throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    select(df, c("a", "a")),
    regexp = "must not contain duplicates"
  )
})

test_that("select with non-character columns argument throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    select(df, c(1, 2)),
    regexp = "must be a character vector"
  )
})

test_that("select with columns not a vector throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    select(df, list("a", "b")),
    regexp = "must be a character vector"
  )
})

test_that("select with .data not a data.frame throws an error", {
  df <- matrix(1:10, nrow = 5)
  expect_error(
    select(df, c("a", "b")),
    regexp = "'.data' must be a data.frame"
  )
})

test_that("select with empty columns vector returns empty data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  result <- select(df, character(0))
  expect_equal(ncol(result), 0)
  expect_true(is.data.frame(result))
})

test_that("select with all columns returns original data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  result <- select(df, c("a", "b"))
  expect_equal(result, df)
})

test_that("select does not modify the original data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  df_copy <- df
  select(df, c("a"))
  expect_equal(df, df_copy)
})
