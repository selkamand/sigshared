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
rename <- function(df, namemap){
  if(!is.vector(names(namemap))){
    stop("rename: namemap must be a named vector")
  }

  if(any(nchar(names(namemap)) == 0)) {
   stop("rename: all elements in namemap must be named")
  }

  missing_names <- setdiff(namemap, colnames(df))
  if(length(missing_names) > 0 ) stop("rename: could not find column/s named [", paste0(missing_names, collapse = ", "), "]. Valid column names include: [", paste0(colnames(df), collapse = ", "),"]")

  colnames(df)[match(namemap, colnames(df))] <- names(namemap)
  return(df)
}
