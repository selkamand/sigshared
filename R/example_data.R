# Roxygen Templates -------------------------------------------------------

#' @name signature
#'
#' @title Signature Data Type
#'
#' @description
#' **Signature**: A mutational signature profile represented as a `data.frame` with the following 3 columns:
#' - `type`: The type of mutation (e.g., A>G, C>T).
#' - `channel`: The mutational channel (e.g., A\\[A->G\\]G, C\\[C->T\\]G).
#' - `fraction`: The fraction of mutations attributed to this specific type and channel.
NULL

#' @name signature_collection
#'
#' @title Signature Collections
#'
#' @description
#' **Signature Collections**: A list of signature `data.frames`, where each list entry represents a signature, and the name of each entry corresponds to the signature’s name.
NULL

#' @name signature_annotation
#'
#' @title Signature Annotations
#'
#' @description
#' **Signature Annotations**: Signature-level annotations represented as a `data.frame` with 4 required columns:
#' - `signature`: The name of the signature.
#' - `aetiology`: The cause or origin of the signature.
#' - `class`: The class of the signature’s aetiology.
#' - `subclass`: The subclass of the signature’s aetiology.
NULL

#' @name catalogue
#'
#' @title Catalogue Data Type
#'
#' @description
#' **Catalogue**: The mutational profile of a sample, represented as a `data.frame` with 4 required columns:
#' - `channel`: The mutational channel (e.g., A\\[A->G\\]G, C\\[C->T\\]G).
#' - `type`: The type of mutation (higher level classification of channel, e.g. A>G, C>T).
#' - `fraction`: The fraction of mutations attributed to this channel.
#' - `count`: The count of mutations for this channel.
NULL

#' @name catalogue_collections
#'
#' @title Catalogue Collections Data Type
#'
#' @description
#' **Catalogue Collections**: A list of catalogue `data.frames`, where each `data.frame` represents the mutational profile of a sample. Each entry in the list corresponds to a sample, and the name of each entry is the sample identifier. Each catalogue `data.frame` contains the following columns:
#' - `channel`: The mutational channel (e.g., A\\[A->G\\]G, C\\[C->T\\]G).
#' - `type`: The type of mutation (higher level classification of channel, e.g. A>G, C>T).
#' - `fraction`: The fraction of mutations attributed to this channel.
#' - `count`: The count of mutations for this channel.
NULL

#' @name cohort
#'
#' @title Cohort Signature Analysis Results Data Type
#'
#' @description
#' **Cohort**: A `data.frame` containing results from a cohort-level signature analysis, with the following 5 columns:
#' - `sample`: The sample identifier.
#' - `signature`: The name of the signature.
#' - `contribution_absolute`: The absolute contribution of the signature to the sample.
#' - `contribution`: The proportional contribution of the signature.
#' - `p_value`: The p-value representing the statistical significance of the contribution.
NULL

#' @name bootstraps
#'
#' @title Bootstraps Data Type
#'
#' @description
#' **Bootstraps**: A `data.frame` representing the optimal mutational signature exposure contributions for each bootstrap resample. Must contain 1 row per signature & bootstrap combination with the following 5 columns:
#' - `bootstrap`: The bootstrap index.
#' - `signature`: The name of the signature.
#' - `contribution_absolute`: The absolute contribution of the signature.
#' - `contribution`: The percentage contribution of the signature.
NULL

#' @name model
#'
#' @title Signature Model Specification
#'
#' @description
#' **Model**: A named numeric vector representing a signature model where names are signatures and the values are their proportional contributions to the model.
#'
#' [example_model()] returns an exemplar 'sigverse' style signature model specification.
#' The model is represented as a named numeric vector where each name corresponds
#' to a signature, and each value represents its proportional contribution.
#'
#' [assert_model()] asserts an object is a valid sigverse model.
NULL



# Signatures --------------------------------------------------------------


#' Example sigverse signature
#'
#' This function returns an exemplar signature with valid data following the 'sigverse' style.
#'
#' @return A data.frame representing a valid exemplar signature signature in the 'sigverse' format.
#'
#' @examples
#'
#' # Return a valid signature
#' example_signature()
#'
#' # Return a valid but empty signature (all fraction values are 0)
#' example_signature_empty()
#' @export
#' @rdname signature
example_signature <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.5)
  )
}


#' @rdname signature
#' @export
example_signature_empty <- function(){
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
#' example_signature_collection()
#'
#' @export
#' @rdname signature_collection
example_signature_collection <- function(){
  list(
    'sig1' = example_signature(),
    'sig2' = example_signature()
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
    'sig1' = example_signature(),
    'sig1' = example_signature()
  )
}

example_invalid_signature_collection_invalid_signature <- function(){
  list(
    'sig1' = example_signature(),
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
#' example_catalogue()
#'
#' # Example of a valid but empty catalogue
#' example_catalogue_empty()
#'
#' @rdname catalogue
#' @export
example_catalogue <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.18518519, 0.37037037, 0.44444444)
  )
}

#' Example Data
#'
#' See [example_catalogue()]
#'
#' @export
example_catalogue_empty <- function(){
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
#' example_catalogue_collection()
#'
#' @export
#' @rdname catalogue_collections
example_catalogue_collection <- function(){
  list(
    'decomp1' = example_catalogue(),
    'decomp2' = example_catalogue(),
    'decomp3' = example_catalogue()
  )
}


example_invalid_catalogue_in_collection <- function(){
  list(
    'decomp1' = example_catalogue(),
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
    'decomp1' = example_catalogue(),
    'decomp1' = example_catalogue()
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
#' example_annotations()
#'
#' @export
example_annotations <- function(){
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
#' example_cohort_analysis()
#'
#' @export
#' @rdname cohort
#'
example_cohort_analysis <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.3, 0.7, 0.4, 0.6),
    "p_value" = c(0.05, 0.1, 0.2, 0.15)
  )
}


example_invalid_cohort_analysis_contribution <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.5, 0.7, 0.6, 0.6),  # Contribution values sum to more than 1
    "p_value" = c(0.05, 0.1, 0.2, 0.15)
  )
}

example_invalid_cohort_analysis_negative_contribution <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.3, -0.1, 0.4, 0.6),
    "p_value" = c(0.05, 0.1, 0.2, 0.15)
  )
}

example_invalid_cohort_analysis_negative_contribution_absolute <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, -7, 40, 60),
    "contribution" = c(0.3, 0.7, 0.4, 0.6),
    "p_value" = c(0.05, 0.1, 0.2, 0.15)
  )
}

example_invalid_cohort_analysis_pvalue <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.3, 0.7, 0.4, 0.6),
    "p_value" = c("0.05", "0.1", "0.2", "0.15") # Character instead of numeric
  )
}

example_invalid_cohort_analysis_missing <- function(){
  data.frame(
    sample = c('sample1', 'sample1', 'sample2', 'sample2'),
    signature = c('sig1', 'sig2', 'sig1', 'sig2'),
    contribution_absolute = c(3, 7, 40, 60),
    contribution = c(0.3, 0.7, 0.4, 0.6),
    "p_value" = c(0.05, NA, 0.2, 0.15)
  )
}


# Bootstraps --------------------------------------------------------------

#' Exemplar bootstraps with valid data
#'
#' This function returns an exemplar bootstrap with valid data following the 'sigverse' style.
#'
#' @return A data.frame representing a valid exemplar bootstrap in the 'sigverse' format.
#'
#' @examples
#'
#' # Return a valid bootstrap
#' example_bootstraps()
#'
#' # Return a valid but empty bootstrap (all contributions are 0)
#' example_bootstraps_empty()
#' @export
#' @rdname bootstraps
example_bootstraps <- function(){
  data.frame(
    bootstrap = rep(c(1, 2), each = 3L),
    signature = rep(c("Signature1", "Signature2", "Signature3"), 2),
    contribution_absolute = c(300, 690, 10, 440, 500, 60),
    contribution = c(0.3, 0.69, 0.01, 0.44, 0.5, 0.06)
  )
}

#' Example Data
#'
#' See [example_bootstraps()]
#'
#' @export
example_bootstraps_empty <- function(){
  data.frame(
    bootstrap = c(1, 1, 2, 2),
    signature = c('Signature1', 'Signature2', 'Signature1', 'Signature2'),
    contribution_absolute = c(0, 0, 0, 0),
    contribution = c(0, 0, 0, 0) # contribution in percentage
  )
}

#' Invalid bootstrap with contribution > 100
#'
#' This function returns a bootstrap with invalid data where contribution percentage is greater than 100.
#'
#' @return A data.frame representing an invalid bootstrap with contribution > 1
example_invalid_bootstraps_contribution <- function(){
  data.frame(
    bootstrap = c(1, 1, 2, 2),
    signature = c('Signature1', 'Signature2', 'Signature1', 'Signature2'),
    contribution_absolute = c(0.3, 0.7, 0.5, 1.2),  # Invalid
    contribution = c(.30, .70, .50, 1.20) # contribution > 100%
  )
}

#' Invalid bootstrap with negative contribution
#'
#' This function returns a bootstrap with invalid data where contribution values are negative.
#'
#' @return A data.frame representing an invalid bootstrap with negative contribution.
example_invalid_bootstraps_negative_contribution <- function(){
  data.frame(
    bootstrap = c(1, 1, 2, 2),
    signature = c('Signature1', 'Signature2', 'Signature1', 'Signature2'),
    contribution_absolute = c(0.3, 0.7, 0.5, 0.6),
    contribution = c(.30, .70, .50, -.10) # Invalid negative contribution
  )
}

#' Invalid bootstrap with negative contribution_absolute
#'
#' This function returns a bootstrap with invalid data where absolute contribution values are negative.
#'
#' @return A data.frame representing an invalid bootstrap with negative contribution_absolute.
example_invalid_bootstraps_negative_contribution_absolute <- function(){
  data.frame(
    bootstrap = c(1, 1, 2, 2),
    signature = c('Signature1', 'Signature2', 'Signature1', 'Signature2'),
    contribution_absolute = c(0.3, -0.7, 0.5, 0.6),  # Invalid negative contribution_absolute
    contribution = c(.30, .70, .50, .60)
  )
}

#' Invalid bootstrap with missing data
#'
#' This function returns a bootstrap with missing data (NA values).
#'
#' @return A data.frame representing an invalid bootstrap with missing data.
example_invalid_bootstraps_missing <- function(){
  data.frame(
    bootstrap = c(1, 1, 2, NA),  # Missing bootstrap value
    signature = c('Signature1', 'Signature2', NA, 'Signature2'),  # Missing signature value
    contribution_absolute = c(0.3, 0.7, 0.5, 0.6),
    contribution = c(.30, .70, .50, .60)
  )
}


# Model  -----------------------------------------------------

#' Exemplar model specification
#'
#' @return [example_model()] returns a named numeric vector where names correspond
#' signatures and matched values represents their proportional contribution.
#'
#' @examples
#' # Return a valid signature model (representing a combination of
#' # 30% Signature1 & 70% Signature2)
#' example_model()
#'
#' # Return a valid but empty signature model (length zero)
#' example_model_empty()
#'
#' # Assert model is valid
#' assert_model(example_model())
#'
#' @export
#' @rdname model
#' @order 1
example_model <- function(){
 c("Signature1" = 0.3, "Signature2" = 0.7)
}

# Invalid model: Non-numeric values
example_invalid_non_numeric_model <- function() {
  c("Signature1" = "0.5", "Signature2" = "0.3", "Signature3" = "0.2")
}

# Invalid model: Contributions sum to more than 1
example_invalid_over_one_model <- function() {
  c("Signature1" = 0.5, "Signature2" = 0.4, "Signature3" = 0.2)
}

# Invalid model: No names (completely unnamed elements)
example_invalid_unnamed_model <- function() {
  c(0.5, 0.3, 0.2)
}

# Invalid model: Mix of named and unnamed elements
example_invalid_mixed_names_model <- function() {
  c("Signature1" = 0.5, 0.3, "Signature3" = 0.2)
}

# Invalid model: Duplicate signature names
example_invalid_duplicate_signatures_model <- function() {
  c("Signature1" = 0.5, "Signature1" = 0.3, "Signature3" = 0.2)
}

# Invalid model: Negative values
example_invalid_negative_model <- function() {
  c("Signature1" = -0.1, "Signature2" = 0.3, "Signature3" = 0.2)
}


#' Example Data
#'
#' See [example_model()]
#'
#' @export
example_model_empty <- function(){
  numeric(0)
}


# Example Colo829 Data ----------------------------------------------------

#' Exemplar bootstraps with valid data
#'
#' This function returns example bootstrap data from somatic Single Base Substitution (SBS)
#' mutational signature analysis of the COLO829 melanoma cell line, following the 'sigverse' format  (see [example_bootstraps()]).
#'
#' @return A data.frame representing bootstraps from SBS mutational signature analysis of COLO829
#' in the 'sigverse' format. (see [example_bootstraps()])
#'
#' @examples
#'
#' # Return example bootstraps from COLO829 SBS analysis
#' example_bootstraps_colo829()
#'
#' @export
example_bootstraps_colo829 <- function(){
  path <- system.file(package = "sigshared", "examples/colo829_sbs.bootstraps.csv.gz")
  utils::read.csv(file = path, header = TRUE, sep = ",")
}

#' Exemplar catalogue with valid data
#'
#' This function returns example catalogue from somatic Single Base Substitution (SBS)
#' mutational signature analysis of the COLO829 melanoma cell line, following the 'sigverse' format
#' (see [example_catalogue()]).
#'
#' @return A data.frame representing the SBS mutational catalogue of COLO829 in the 'sigverse' format
#' (see [example_catalogue()]).
#'
#' @examples
#'
#' # Return example catalogue from COLO829 SBS analysis
#' example_catalogue_colo829()
#'
#' @export
example_catalogue_colo829 <- function(){
  path <- system.file(package = "sigshared", "examples/colo829_sbs.catalogue.csv.gz")
  utils::read.csv(file = path, header = TRUE, sep = ",")
}


example_exposures_colo829 <- function(){
  path <- system.file(package = "sigshared", "examples/colo829_sbs.catalogue.csv.gz")
  utils::read.csv(file = path, header = TRUE, sep = ",")
}


