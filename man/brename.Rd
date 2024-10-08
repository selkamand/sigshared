% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidylikes.R
\name{brename}
\alias{brename}
\title{Rename columns in a data frame based on a name mapping}
\usage{
brename(.data, namemap)
}
\arguments{
\item{.data}{A data frame whose columns will be renamed.}

\item{namemap}{A named vector or list, where the names represent the new column
names and the values represent the current column names in the data frame.}
}
\value{
The data frame with renamed columns.
}
\description{
This function renames columns in a data frame using a provided name mapping.
The 'b' in \code{brename} stands for 'base', indicating that this function is built
using base R functions without additional dependencies. This prefix also helps
avoid name clashes with functions in other packages like the tidyverse when both
are loaded into the same environment.
}
\details{
The function checks if all the values (current column names) in the
\code{namemap} are present in the data frame. If any expected column names are missing,
the function stops with an error. If all expected names are present, the corresponding
columns are renamed according to the names provided in \code{namemap}.
}
\examples{
# Create a sample data frame
df <- data.frame(
  old_name1 = 1:5,
  old_name2 = letters[1:5]
)

# Rename columns
df_renamed <- brename(df, c(new_name1 = "old_name1", new_name2 = "old_name2"))
print(df_renamed)

}
