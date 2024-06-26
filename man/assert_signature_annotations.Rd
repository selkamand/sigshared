% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{assert_signature_annotations}
\alias{assert_signature_annotations}
\title{Assert object represents a signature}
\usage{
assert_signature_annotations(
  obj,
  required_signatures = NULL,
  msg = NULL,
  call = rlang::caller_env(),
  arg_name = NULL
)
}
\arguments{
\item{obj}{object}

\item{required_signatures}{signatures expected to be annotated. Will throw an error if any of these signatures are missing from the annotation data.frame (character)}

\item{msg}{error message}

\item{call}{internal paramater}

\item{arg_name}{argument}
}
\value{
Throws error if assertion fails, otherwise invisibly returns true
}
\description{
Assert object represents signature annotations
}
\examples{
# Generate Example Datatypes

if(interactive()){

  # Signatures
  signature = example_valid_signature()
  signature_collection = example_valid_signature_collection()
  signature_annotations = example_valid_signature_annotations()

  # Catalogues
  catalogue = example_valid_catalogue()
  catalogue_collection = example_valid_catalogue_collection()

  # Cohort Analysis Results
  cohort_analysis = example_valid_cohort_analysis()

  # Cohort Analysis Results
  catalogue_collection = example_valid_catalogue_collection()

  # Run Assertions
  assert_signature(signature)
  assert_signature_collection(signature_collection)
  assert_catalogue(catalogue)
  assert_catalogue_collection(catalogue_collection)
  assert_cohort_analysis(cohort_analysis)
}
}
