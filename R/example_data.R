
# Signatures --------------------------------------------------------------

#' Exemplar signature and with valid data
#'
#' This function returns an exemplar signature with valid data following the 'sigverse' style.
#'
#' @return A data.frame representing a valid exemplar signature signature in the 'sigverse' format.
#'
#' @examples
#'
#' # Return a valid signature
#' example_valid_signature()
#'
#' # Return a valid but empty signature (all fraction values are 0)
#' example_valid_signature_empty()
#' @name valid_sig
#' @export
example_valid_signature <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.5)
  )
}

#' @rdname valid_sig
#' @export
example_valid_signature_empty <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    fraction = c(0, 0, 0)
  )
}


example_invalid_signature_channeldup <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.3)
  )
}

example_invalid_signature_numeric_channel = function(){
  data.frame(
    channel = c(1:4),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.3)
  )
}

example_invalid_signature_factor_channel = function(){
  data.frame(
    channel = factor(c('A[A->G]G', 'A[A->G]G', 'A[A->G]C', 'A[A->G]T')),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.3)
  )
}

example_invalid_signature_missing = function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', NA),
    fraction = c(0.4, 0.1, 0.2)
  )
}

example_invalid_signature_fraction_sum <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    fraction = c(0.6, 0.6, 0.2)
  )
}

example_invalid_signature_fraction_sum_below_one <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    fraction = c(0.1, 0.1, 0.2)
  )
}


example_invalid_signature_negative_fraction <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    fraction = c(0.4, -0.1, 0.2)
  )
}


# Signature Collections ---------------------------------------------------

#' Exemplar collection of valid signatures
#'
#' This function returns a list of exemplar signatures with valid data following the 'sigverse' style. The list represents a collection of valid signatures.
#'
#' @return A list containing multiple data.frames, each representing a valid signature.
#'
#' @examples
#' example_valid_signature_collection()
#'
#' @export
example_valid_signature_collection <- function(){
  list(
    'sig1' = example_valid_signature(),
    'sig2' = example_valid_signature()
  )
}

example_invalid_signature_collection_not_list <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2)
  )
}

example_invalid_signature_collection_empty <- function(){
  list()  # An empty list, representing an empty signature collection
}

example_invalid_signature_collection_duplicated_names <- function(){
  list(
    'sig1' = example_valid_signature(),
    'sig1' = example_valid_signature()
  )
}

example_invalid_signature_collection_invalid_signature <- function(){
  list(
    'sig1' = example_valid_signature(),
    'sig2' = example_invalid_signature_channeldup()
  )
}



# Catalogues ----------------------------------------------------------


#' Exemplar valid catalogue
#'
#' This function returns a data.frame representing a valid catalogue following the 'sigverse' style.
#'
#' @return A data.frame containing valid catalogue data.
#'
#' @examples
#'
#' # Example of a valid catalogue
#' example_valid_catalogue()
#'
#' # Example of a valid but empty catalogue
#' example_valid_catalogue_empty()
#'
#' @name valid_catalogue
#' @export
example_valid_catalogue <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.18518519, 0.37037037, 0.44444444)
  )
}

#'@rdname valid_catalogue
#'@export
example_valid_catalogue_empty <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(0, 0, 0),
    fraction = c(0, 0, 0)
  )
}


example_invalid_catalogue_nonsensical_fraction <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.2, 0.6, 0.2)
  )
}

example_invalid_catalogue_missing <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, NA),
    fraction = c(0.18518519, 0.37037037, 0.44444444)
  )
}

example_invalid_catalogue_colname_typo <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    typeo = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.18518519, 0.37037037, 0.44444444)
  )
}

example_invalid_catalogue_channeldup <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]C', 'A[A->G]C'),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12, 10),
    fraction = c(0.13513514, 0.27027027, 0.32432432, 0.27027027)
  )
}

# catalogue Collections -----------------------------------------------
#' Exemplar collection of valid catalogues
#'
#' This function returns a list of exemplar catalogues with valid data following the 'sigverse' style. The list represents a collection of valid catalogues.
#'
#' @return A list containing multiple data.frames, each representing a valid catalogue.
#'
#' @examples
#' example_valid_catalogue_collection()
#'
#' @export
example_valid_catalogue_collection <- function(){
  list(
    'decomp1' = example_valid_catalogue(),
    'decomp2' = example_valid_catalogue(),
    'decomp3' = example_valid_catalogue()
  )
}


example_invalid_catalogue_in_collection <- function(){
  list(
    'decomp1' = example_valid_catalogue(),
    'decomp2' = example_invalid_catalogue_nonsensical_fraction()
  )
}


example_invalid_catalogue_collection_not_list <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2)
  )
}

example_invalid_catalogue_collection_empty <- function(){
  list()  # An empty list, representing an empty catalogue collection
}

example_invalid_catalogue_collection_duplicated_names <- function(){
  list(
    'decomp1' = example_valid_catalogue(),
    'decomp1' = example_valid_catalogue()
  )
}


# Annotations -------------------------------------------------------------
#' Exemplar collection of valid annotations
#'
#' This function returns a data.frame representing a collection of valid annotations following the 'sigverse' style.
#'
#' @return A data.frame containing annotations for multiple signatures.
#'
#' @examples
#' example_valid_annotations()
#'
#' @export
example_valid_annotations <- function(){
  data.frame(
    signature = c('sig1', 'sig2'),
    aetiology = c('A clock like signature', 'An AID/APOBEC related signature'),
    class = c('clock-like', 'cytidine deaminases'),
    subclass = c('clock-like', 'cytidine deaminases')
  )
}


example_invalid_annotations_sig_colname <- function(){
  data.frame(
    notsignature = c('sig1', 'sig2'),
    aetiology = c('A clock like signature', 'An AID/APOBEC related signature'),
    class = c('clock-like', 'cytidine deaminases'),
    subclass = c('clock-like', 'cytidine deaminases')
  )
}

example_invalid_annotations_sig_duplicated <- function(){
  data.frame(
    signature = c('sig1', 'sig1'),
    aetiology = c('A clock like signature', 'An AID/APOBEC related signature'),
    class = c('clock-like', 'cytidine deaminases'),
    subclass = c('clock-like', 'cytidine deaminases')
  )
}


# Cohorts -----------------------------------------------------------------

#' Exemplar collection of valid cohort analyses
#'
#' This function returns a data.frame representing a collection of valid cohort analyses following the 'sigverse' style.
#'
#' @return A data.frame containing multiple cohort analyses.
#'
#' @examples
#' example_valid_cohort_analysis()
#'
#' @export
example_valid_cohort_analysis <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.3, 0.7, 0.4, 0.6),
    "bootstraps" = c('0.3; 0.27; 0.32', '0.7;0.72;0.68', '0.4;0.45;0.35', '0.6;0.62;0.55')
  )
}


example_invalid_cohort_analysis_contribution <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.5, 0.7, 0.6, 0.6),  # Contribution values sum to more than 1
    "bootstraps" = c('0.3; 0.27; 0.32', '0.7;0.72;0.68', '0.4;0.45;0.35', '0.6;0.62;0.55')
  )
}

example_invalid_cohort_analysis_negative_contribution <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.3, -0.1, 0.4, 0.6),
    "bootstraps" = c('0.3; 0.27; 0.32', '0.7;0.72;0.68', '0.4;0.45;0.35', '0.6;0.62;0.55')
  )
}

example_invalid_cohort_analysis_negative_contribution_absolute <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, -7, 40, 60),
    "contribution" = c(0.3, 0.7, 0.4, 0.6),
    "bootstraps" = c('0.3; 0.27; 0.32', '0.7;0.72;0.68', '0.4;0.45;0.35', '0.6;0.62;0.55')
  )
}

example_invalid_cohort_analysis_bootstraps <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.3, 0.7, 0.4, 0.6),
    "bootstraps" = c('0.3; 0.27; 0.32', '0.7;0.72;0.68', 'abc', '0.6;0.62;0.55')
  )
}

example_invalid_cohort_analysis_missing <- function(){
  data.frame(
    sample = c('sample1', 'sample1', 'sample2', 'sample2'),
    signature = c('sig1', 'sig2', 'sig1', 'sig2'),
    contribution_absolute = c(3, 7, 40, 60),
    contribution = c(0.3, 0.7, 0.4, 0.6),
    bootstraps = c('0.3; 0.27; 0.32', NA, '0.4;0.45;0.35', '0.6;0.62;0.55')
  )
}

