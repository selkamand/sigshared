% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversions.R
\name{sig_collection_reformat_tidy_to_list}
\alias{sig_collection_reformat_tidy_to_list}
\title{Reformat Tidy Signature or Catalogue Data to List Format}
\usage{
sig_collection_reformat_tidy_to_list(signatures)
}
\arguments{
\item{signatures}{A tidy \code{data.frame} with columns: \code{type}, \code{channel}, \code{fraction}, and optionally \code{count},
and an ID column: \code{signature}, \code{catalogue}, or \code{sample}.}
}
\value{
A signature/catalogue collection (see \code{\link[=example_signature_collection]{example_signature_collection()}} and \code{\link[=example_catalogue_collection]{example_catalogue_collection()}})
}
\description{
Converts a tidy data frame of mutational signatures or catalogues
named list of data frames, each with columns \code{channel}, \code{type}, and \code{fraction},
and (for catalogues) \code{count}.
}
\details{
The function supports input with an identifier column named \code{signature}, \code{catalogue}, or \code{sample}.
The output is a named list of tibbles indexed by that identifier.
}
\examples{
tidysigs <- example_signature_collection_tidy()
sig_collection_reformat_tidy_to_list(tidysigs)

tidycatalogues <- example_catalogue_collection_tidy()
sig_collection_reformat_tidy_to_list(tidycatalogues)

}
