# A collection of low-dependency utility functions used throughout the sigverse
# (including tidyverse equivalents)

#' Rename columns in a data frame based on a name mapping
#'
#' This function renames columns in a data frame using a provided name mapping.
#' It verifies that all names in the mapping exist in the data frame before
#' renaming, and raises an error if any expected columns are missing.
#'
#' @param df A data frame whose columns will be renamed.
#' @param namemap A named vector or list, where the names represent the new column
#' names and the values represent the current column names in the data frame.
#'
#' @return The data frame with renamed columns.
#'
#' @details The function checks if all the values (current column names) in the
#' `namemap` are present in the data frame. If any expected column names are missing,
#' the function stops with an error. If all expected names are present, the corresponding
#' columns are renamed according to the names provided in `namemap`.
#'
#' @export
#'
#' @examples
#' rename(mtcars, c(miles_per_gallon = "mpg"))
rename <- function(.data, namemap){
  if(!is.vector(names(namemap))){
    stop("rename: namemap must be a named vector")
  }

  if(any(nchar(names(namemap)) == 0)) {
   stop("rename: all elements in namemap must be named")
  }

  missing_names <- setdiff(namemap, colnames(.data))
  if(length(missing_names) > 0 ) stop("rename: could not find column/s named [", paste0(missing_names, collapse = ", "), "]. Valid column names include: [", paste0(colnames(.data), collapse = ", "),"]")

  colnames(.data)[match(namemap, colnames(.data))] <- names(namemap)
  return(.data)
}


#' Select columns from a data frame
#'
#' This function selects columns from a data frame based on a character vector of column names.
#' It ensures that the specified columns exist in the data frame and that there are no duplicates
#' in the column names provided.
#'
#' @param .data A data frame from which to select columns.
#' @param columns A character vector of column names to select from \code{.data}.
#'
#' @return A data frame containing only the specified columns from \code{.data}.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   x = 1:5,
#'   y = letters[1:5],
#'   z = rnorm(5)
#' )
#'
#' # Select columns 'x' and 'z'
#' select(df, c('x', 'z'))
#'
#' @export
select <- function(.data, columns){
  # Assertions
  if(!is.data.frame(.data)){
    stop("select: '.data' must be a data.frame")
  }

  if(!is.vector(columns)){
    stop("select: 'columns' argument must be a character vector")
  }

  if(!is.character(columns)){
    stop("select: 'columns' argument must be a character vector")
  }

  if(anyDuplicated(columns) > 0){
    stop("select: 'columns' argument must not contain duplicates")
  }

  cols_not_found <- setdiff(columns, colnames(.data))
  if(length(cols_not_found) != 0 ){
    stop("Could not find column/s: [", paste0(cols_not_found, collapse = ", "), "]")
  }

  # Return subset data.frame
  return(.data[columns])
}
