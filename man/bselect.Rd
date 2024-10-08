% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidylikes.R
\name{bselect}
\alias{bselect}
\title{Select (and optionally rename) columns from a data frame}
\usage{
bselect(.data, columns)
}
\arguments{
\item{.data}{A data frame from which to select columns.}

\item{columns}{A character vector of column names to select from \code{.data}.
If \code{columns} is a named vector, the selected columns will be renamed in the returned data frame
using the names of \code{columns} as the new column names.}
}
\value{
A data frame containing only the specified columns from \code{.data}.
If \code{columns} is a named vector, the columns in the returned data frame will be renamed accordingly.
}
\description{
This function selects columns from a data frame based on a character vector of column names.
The 'b' in \code{bselect} stands for 'base': this function is built
using base R functions without additional dependencies.
}
\details{
If the \code{columns} vector is named, \code{bselect} will both select and rename the columns
in the resulting data frame. The new column names are taken from the names of the \code{columns} vector,
and the old column names are taken from the values of the \code{columns} vector.

The function checks if the specified columns exist in the data frame and ensures there are no duplicates
in the \code{columns} vector. If \code{columns} is named, the function uses \code{brename} internally
to rename the columns after subsetting.
}
\examples{
# Create a sample data frame
df <- data.frame(
  x = 1:5,
  y = letters[1:5],
  z = rnorm(5)
)

# Select columns 'x' and 'z'
df_selected <- bselect(df, c("x", "z"))
print(df_selected)

# Select and rename columns
df_selected_renamed <- bselect(df, c(new_x = "x", new_z = "z"))
print(df_selected_renamed)

}
