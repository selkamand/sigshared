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
