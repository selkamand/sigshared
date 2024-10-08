% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R, R/example_data.R
\name{assert_signature_annotations}
\alias{assert_signature_annotations}
\alias{signature_annotation}
\title{Signature Annotations}
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
\description{
Assert object represents signature annotations

\strong{Signature Annotations}: Signature-level annotations represented as a \code{data.frame} with 4 required columns:
\itemize{
\item \code{signature}: The name of the signature.
\item \code{aetiology}: The cause or origin of the signature.
\item \code{class}: The class of the signature’s aetiology.
\item \code{subclass}: The subclass of the signature’s aetiology.
}
}
