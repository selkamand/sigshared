
# Utilities ---------------------------------------------------------------
is_one <- function(n, tolerance = 5e-07){
  isTRUE(all.equal(target = n, current = 1, tolerance = tolerance))
}

is_over_one <- function(n, tolerance = 5e-07){
  n-tolerance > 1
}

# Shorthand for paste0
p <- function(...){
  paste0(...)
}

# Check Functions ---------------------------------------------------------
check_signature = function(obj, must_sum_to_one = TRUE){
  required_cols = c('channel', 'type', 'fraction')

  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature: Signature must be represented as a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature: Signature data.frame must contain the following columns: [', paste0(missing_cols, collapse = ", "),']'))
  }

  # Column Types Unexpected
  if(!is.character(obj[['channel']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: channel column must be of type {.emph character}, not {.emph {class(arg_value[["channel"]])}}')

  if(!is.character(obj[['type']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: type column must be of type {.emph character}, not {.emph {class(arg_value[["type"]])}}')

  if(!is.numeric(obj[['fraction']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: fraction column must be of type {.emph numeric}, not {.emph {class(arg_value[["fraction"]])}}')


  # Duplicated Channels
  if(anyDuplicated(obj[['channel']])){
    duplicated_channel = obj[['channel']][duplicated(obj[['channel']])]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature: Found duplicated channels (',paste0(duplicated_channel, collapse = ", "), ')'))
  }

  # Missing Values
  if(anyNA(obj)){
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: Found missing (NA) values')
  }

  #Fractions include negative values
  if(any(obj[['fraction']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: Found negative fractions')
  }

  # Fractions Sum to 1
  if(must_sum_to_one & !is_one(sum(obj[['fraction']]))){
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature: Sum of fractions must be approximately equal to 1, not ', sum(obj[['fraction']])))
  }

  # Even if fractions < 1 are allowed, ensure fraction is not > 1
  if(!must_sum_to_one & is_over_one(sum(obj[['fraction']]))) {
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature: Sum of fractions must be less than or equal to 1, not ', sum(obj[['fraction']])))
  }

  return(invisible(TRUE))
}

check_catalogue = function(obj, must_sum_to_one = TRUE){
  required_cols = c('channel', 'type', 'count', 'fraction')

  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid catalogue: Signature must be represented as a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid catalogue: Signature data.frame must contain the following columns: [', paste0(missing_cols, collapse = ", "),']'))
  }

  # Column Types Unexpected
  if(!is.character(obj[['channel']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue: channel column must be of type {.emph character}, not {.emph {class(arg_value[["channel"]])}}')

  if(!is.character(obj[['type']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue: type column must be of type {.emph character}, not {.emph {class(arg_value[["type"]])}}')

  if(!is.numeric(obj[['fraction']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue: fraction column must be of type {.emph numeric}, not {.emph {class(arg_value[["fraction"]])}}')

  if(!is.numeric(obj[['count']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue: count column must be of type {.emph numeric}, not {.emph {class(arg_value[["count"]])}}')

  # Duplicated Channels
  if(anyDuplicated(obj[['channel']])){
    duplicated_channel = obj[['channel']][duplicated(obj[['channel']])]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid catalogue: found duplicated channels (',paste0(duplicated_channel, collapse = ", "), ')'))
  }

  # Missing Values
  if(anyNA(obj)){
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue: found missing (NA) values')
  }

  # Fractions include negative values
  if(any(obj[['fraction']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue: found negative fractions')
  }

  # Fractions sum to 1 unless there are no non-zero values of count
  total_count = sum(obj[['count']])
  total_frac = sum(obj[['fraction']])
  empty_catalogue = total_count == 0 & total_frac == 0

  if(must_sum_to_one & !is_one(sum(obj[['fraction']])) & !empty_catalogue){
    # Only exception is if there are no mutations at all, in which case fraction can be 0
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid catalogue: Sum of fractions must be approximately equal to 1, not ', sum(obj[['fraction']])))
  }

  # Even if fractions < 1 are allowed, ensure fraction is not > 1
  if(!must_sum_to_one & is_over_one(sum(obj[['fraction']]))) {
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid catalogue: Sum of fractions must be less than or equal to 1, not ', sum(obj[['fraction']])))
  }

  # Fractions dont make sense given count
  calculated_fraction = obj[['count']]/sum(obj[['count']])
  observed_fraction = obj[['fraction']]

  # Calculated and observed fraction are approximately equal
  if(must_sum_to_one & !isTRUE(all.equal(calculated_fraction, observed_fraction, tolerance = 5e-07)) & !empty_catalogue){
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue: fraction is not explained by counts')
  }

  #Return TRUE
  return(invisible(TRUE))
}

check_signature_collection <- function(obj){

  # Is not list
  if(!is.list(obj) | is.data.frame(obj))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature collection: Collections must be of type {.emph list}, not {class(arg_value)}')

  # Length Greater than 0
  if(length(obj) == 0){
    return('{.arg {arg_name}} is {.strong NOT} a valid signature collection: No signatures are present in the collection')
  }

  # Duplicated names
  signames = names(obj)
  if(anyDuplicated(signames)){
    duplicated_signames = signames[duplicated(signames)]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature collection: found duplicated signature names (',paste0(duplicated_signames, collapse = ", "), ')'))
  }

  # Assert each signature in the collection
  for (i in seq_along(obj)){
    sig = obj[[i]]
    name = signames[i]
    err_message = check_signature(sig)

    err_message = sub(x=err_message, pattern = "^.*?:(.*)$", replacement = "\\1")
    if(err_message != TRUE) {
      err_message = paste0('{arg_name} is not a valid signature collection. Signature {.emph ',name,'} fails the following check:\f\f', err_message, collapse = "")
      return(err_message)
    }
  }

  # Return TRUE if passing
  return(invisible(TRUE))
}


check_catalogue_collection <- function(obj){

  # Is not list
  if(!is.list(obj) | is.data.frame(obj))
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue collection: Collections must be of type {.emph list}, not {class(arg_value)}')

  # Length Greater than 0
  if(length(obj) == 0){
    return('{.arg {arg_name}} is {.strong NOT} a valid catalogue collection: No catalogues are present in the collection')
  }

  # Duplicated names
  signames = names(obj)
  if(anyDuplicated(signames)){
    duplicated_signames = signames[duplicated(signames)]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid catalogue collection: found duplicated catalogue names (',paste0(duplicated_signames, collapse = ", "), ')'))
  }

  # Assert each catalogue in the collection
  for (i in seq_along(obj)){
    sig = obj[[i]]
    name = signames[i]
    err_message = check_catalogue(sig)

    err_message = sub(x=err_message, pattern = "^.*?:(.*)$", replacement = "\\1")
    if(err_message != TRUE) {
      err_message = paste0('{arg_name} is not a valid catalogue collection. Signature {.emph ',name,'} fails the following check:\f\f', err_message, collapse = "")
      return(err_message)
    }
  }

  # Return TRUE if passing
  return(invisible(TRUE))
}

check_signature_annotation <- function(obj, required_signatures = NULL){
  required_cols = c('signature', 'aetiology', 'class', 'subclass')

  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: Annotations must be a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: Annotation data.frame must contain the following columns: [', paste0(missing_cols, collapse = ", "),']'))
  }

  # Column Types Unexpected
  if(!is.character(obj[['signature']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: signature column must be of type {.emph character}, not {.emph {class(arg_value[["channel"]])}}')

  if(!is.character(obj[['aetiology']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: aetiology column must be of type {.emph character}, not {.emph {class(arg_value[["aetiology"]])}}')

  if(!is.character(obj[['class']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: class column must be of type {.emph character}, not {.emph {class(arg_value[["class"]])}}')

  if(!is.character(obj[['subclass']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: type subclass must be of type {.emph character}, not {.emph {class(arg_value[["subclass"]])}}')

  # Duplicated Signatures
  if(anyDuplicated(obj[['signature']])){
    duplicated = obj[['signature']][duplicated(obj[['signature']])]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: Found duplicated signature (',paste0(duplicated, collapse = ", "), ')'))
  }

  # Missing Values
  if(anyNA(obj[['signature']])){
    return('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: Found missing (NA) values in signature column')
  }

  # Check for required signatures
  if (!is.null(required_signatures)) {
    missing_required = setdiff(required_signatures, obj[['signature']])
    if (length(missing_required) > 0) {
      return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: Missing annotations for the following signatures: [', paste0(missing_required, collapse = ", "),']'))
    }
  }

  return(invisible(TRUE))
}


check_cohort_analysis <- function(obj){
  required_cols = c("sample", "signature", "contribution_absolute", "contribution", "p_value")


  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Cohort analyses must be represented as a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Cohort analysis data.frame must contain the following columns: [', paste0(missing_cols, collapse = ", "),']'))
  }

  # Column Types Unexpected
  if(!is.character(obj[['sample']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: sample column must be of type {.emph character}, not {.emph {class(arg_value[["channel"]])}}')

  if(!is.character(obj[['signature']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: signature column must be of type {.emph character}, not {.emph {class(arg_value[["signature"]])}}')

  if(!is.numeric(obj[['contribution']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: contribution column must be of type {.emph numeric}, not {.emph {class(arg_value[["contribution"]])}}')

  if(!is.numeric(obj[['contribution_absolute']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: contribution_absolute column must be of type {.emph numeric}, not {.emph {class(arg_value[["contribution_absolute"]])}}')

  if(!is.numeric(obj[['p_value']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: p_value column must be of type {.emph numeric}, not {.emph {class(arg_value[["p_value"]])}}')

  # Missing Values
  if(anyNA(obj)){
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Found missing (NA) values')
  }

  # Contributions sum to >1
  unique_samples <- unique(obj[['sample']])
  for (sample in unique_samples) {
    sample_contributions <- obj[['contribution']][obj[['sample']] == sample]
    if (sum(sample_contributions) > 1) {
      return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Sum of contribution for sample [', sample, '] must be less than or equal to 1, not ', sum(sample_contributions)))
    }
  }

  # Contributions include negative values
  if(any(obj[['contribution']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Found negative contribution')
  }

  # Contributions include negative values
  if(any(obj[['contribution_absolute']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Found negative contribution_absolute')
  }

  # Check that all bootstraps have the same number of semicolons
  num_semicolons <- nchar(as.character(obj[["bootstraps"]])) - nchar(gsub(";", "", as.character(obj[["bootstraps"]])))
  if(!all(num_semicolons[-1] == num_semicolons[1])){
    return("{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Not all signatures have the same number of bootstraps")
  }


  return(invisible(TRUE))
}

check_bootstraps <- function(obj){
  required_cols = c('bootstrap', 'signature', 'contribution_absolute', 'contribution')

  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: Bootstrap data must be represented as a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: Bootstrap data.frame must contain the following columns: [', paste0(missing_cols, collapse = ", "),']'))
  }

  # Column Types Unexpected
  if(!(is.numeric(obj[['bootstrap']]) | is.character(obj[['bootstrap']]) | is.factor(obj[['bootstrap']])))
    return('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: bootstrap column must be of type {.emph numeric}, {.emph character} or {.emph factor}, not {.emph {class(arg_value[["bootstrap"]])}}')

  if(!is.character(obj[['signature']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: signature column must be of type {.emph character}, not {.emph {class(arg_value[["signature"]])}}')

  if(!is.numeric(obj[['contribution_absolute']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: contribution_absolute column must be of type {.emph numeric}, not {.emph {class(arg_value[["contribution_absolute"]])}}')

  if(!is.numeric(obj[['contribution']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: contribution column must be of type {.emph numeric}, not {.emph {class(arg_value[["contribution"]])}}')

  # Missing Values
  if(anyNA(obj)){
    return('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: Found missing (NA) values')
  }

  # Contributions include negative values
  if(any(obj[['contribution']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: Found negative contribution values')
  }

  # Contributions include negative values
  if(any(obj[['contribution_absolute']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: Found negative contribution_absolute values')
  }

  # Check if any bootstrap signature  has a contribution > 1
  contribution_sums <- stats::ave(obj[["contribution"]], obj[["bootstrap"]], FUN = sum)
  if (any(is_over_one(contribution_sums))) {
    return('{.arg {arg_name}} is {.strong NOT} a valid bootstrap analysis: Total contributions in one or more bootstraps exceed 100%')
  }

  #Return TRUE if passing
  return(invisible(TRUE))
}


check_model <- function(obj, signature_collection = NULL, allow_empty=TRUE){

  # Is not a numeric vector
  is_numeric_vector <- is.numeric(obj) & is.vector(obj)
  if(!is_numeric_vector){
    return("{.arg {arg_name}} is {.strong NOT} a valid signature model specification: Must be a numeric vector, not a {.emph {class(obj)}}")
  }

  # Allow empty vectors (skip the rest of the tests)
  if(length(obj) == 0){
    if(allow_empty)
      return(invisible(TRUE))
    else
      return("{.arg {arg_name}} is {.strong NOT} a valid signature model specification: Vector is empty")
  }

  # Sums to > 1
  if(is_over_one(sum(obj))){
    return("{.arg {arg_name}} is {.strong NOT} a valid signature model specification: Contributions of all signatures in model should add up to <= 1, not [{sum(obj)}]")
  }

  # Completely unnamed
  model_signatures <- names(obj)
  if(!is.vector(model_signatures)){
    return("{.arg {arg_name}} is {.strong NOT} a valid signature model specification: Must be a {.emph named} vector where names represent the signatures to combine.")
  }

  # Mix of named & unnamed elements
  if(any(nchar(model_signatures) == 0)) {
    n_missing_names <- sum(nchar(model_signatures) == 0)
    return(p("{.arg {arg_name}} is {.strong NOT} a valid signature model specification: All elements must be named. Found [",n_missing_names, "] unnamed signature contributions"))
  }

  # Duplicate Signatures
  if(anyDuplicated(model_signatures) > 0){
    dups <- model_signatures[duplicated(model_signatures)]
    ndups = length(dups)
    return(p("{.arg {arg_name}} is {.strong NOT} a valid signature model specification: Found [",ndups,"] signatures with duplicate contributions [",p(dups, collapse=", "),"]"))
  }

  # Missing from the signature collection
  if(!is.null(signature_collection)){
    valid_signatures = names(signature_collection)
    invalid_sigs <- setdiff(model_signatures, valid_signatures)
    n_invalid_sigs <- length(invalid_sigs)
    if(n_invalid_sigs > 0)
      return(p("{.arg {arg_name}} is {.strong NOT} a valid signature model specification: Includes [",n_invalid_sigs,"] invalid signature/s missing from your signature collection [",p(invalid_sigs, collapse=", "),"]"))
  }


  # Contains negatives
  if(any(obj < 0)){
    negative_sigs <- model_signatures[obj < 0]
    n_negative_sigs <- sum(obj < 0)
    return(p("{.arg {arg_name}} is {.strong NOT} a valid signature model specification: Contributions of all signatures in model must be non-negative. Found ",n_negative_sigs," signatures had negative contributions [",p(negative_sigs, collapse=", "),"]"))
  }


  #Return TRUE if passing
  return(invisible(TRUE))
}

check_cohort_metadata <- function(obj){
  required_cols = c('sample', 'disease')

  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort metadata dataset: Metadata must be represented as a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort metadata dataset: Metadata data.frame must contain the following columns: [', paste0(missing_cols, collapse = ", "),']'))
  }

  # Column Types Unexpected
  if(!is.character(obj[['sample']]) & !is.factor(obj[['sample']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort metadata dataset: sample column must be of type {.emph character} or {.emph factor}, not {.emph {class(arg_value[["sample"]])}}')

  # Column Types Unexpected
  if(!is.character(obj[['disease']]) & !is.factor(obj[['disease']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort metadata dataset: disease column must be of type {.emph character} or {.emph factor}, not {.emph {class(arg_value[["disease"]])}}')

  # Missing Values
  if(anyNA(obj[["sample"]])){
    na_count <- sum(is.na(obj[["sample"]]))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort metadata dataset: found ',na_count,' missing (NA) values in  the {.emph sample} column'))
  }


  # Duplicated Samples
  if(anyDuplicated(obj[['sample']])){
    duplicated_sample = obj[['sample']][duplicated(obj[['sample']])]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort metadata dataset: found duplicated sample/s: (',paste0(duplicated_sample, collapse = ", "), ')'))
  }

  #Return TRUE
  return(invisible(TRUE))
}

# Assertions --------------------------------------------------------------

#' Assert object represents a signature
#'
#' @param obj object
#'
#' @param must_sum_to_one throw an error if the fraction column of the signature data.frame does NOT sum to one.
#' @param msg error message
#' @param arg_name argument
#' @param call internal paramater
#'
#' @return Throws error if assertion fails, otherwise invisibly returns true
#' @export
#'
#' @examples
#'
#' if(interactive()){
#'
#'   # Generate Example Datatypes
#'   # Signatures
#'   signature = example_signature()
#'   signature_collection = example_signature_collection()
#'   signature_annotations = example_signature_annotations()
#'   signature_bootstraps = example_bootstraps()
#'
#'
#'   # Catalogues
#'   catalogue = example_catalogue()
#'   catalogue_collection = example_catalogue_collection()
#'
#'   # Cohort Analysis Results
#'   cohort_analysis = example_cohort_analysis()
#'
#'   # Cohort Analysis Results
#'   catalogue_collection = example_catalogue_collection()
#'
#'   # Cohort Metadata
#'   cohort_metadata = example_cohort_metadata()
#'
#'   # Run Assertions
#'   assert_signature(signature)
#'   assert_signature_collection(signature_collection)
#'   assert_catalogue(catalogue)
#'   assert_catalogue_collection(catalogue_collection)
#'   assert_cohort_analysis(cohort_analysis)
#'   assert_cohort_metadata(cohort_metadata)
#' }
#'
#'
#' @details
#' See \url{https://github.com/selkamand/sigshared} for full descriptions of filetypes
#'
#' @rdname signature
assert_signature <- assertions::assert_create(check_signature)

#' @description
#' Assert object represents a signature collection
#'
#' @inheritParams assert_signature
#' @inherit assert_signature title
#'
#' @export
#' @rdname signature_collection
assert_signature_collection <- assertions::assert_create(check_signature_collection)

#' @description
#' Assert object represents a catalogue
#'
#' @inheritParams assert_signature
#' @inherit assert_signature title
#'
#' @export
#' @rdname catalogue
assert_catalogue <- assertions::assert_create(check_catalogue)

#' @description
#' Assert object represents a collection of catalogues
#'
#' @inheritParams assert_signature
#' @inherit assert_signature title
#'
#' @export
#' @rdname catalogue_collections
assert_catalogue_collection <- assertions::assert_create(check_catalogue_collection)

#' @description
#' Assert object represents signature annotations
#'
#' @param required_signatures signatures expected to be annotated. Will throw an error if any of these signatures are missing from the annotation data.frame (character)
#' @inheritParams assert_signature
#' @inherit assert_signature title
#'
#' @export
#' @rdname signature_annotation
assert_signature_annotations <- assertions::assert_create(check_signature_annotation)

#' @description
#' Assert object represents results of a cohort analysis
#'
#' @inheritParams assert_signature
#' @inherit assert_signature title
#'
#' @export
#' @rdname cohort
assert_cohort_analysis <- assertions::assert_create(check_cohort_analysis)


#' @description
#' Assert object represents a bootstrap data.frame.
#'
#' @inheritParams assert_signature
#' @inherit assert_signature title
#'
#' @export
#' @rdname bootstraps
assert_bootstraps <- assertions::assert_create(check_bootstraps)

#' Signature Model Specification
#'
#' @inheritParams assert_signature
#' @param signature_collection Optionally assert all signatures described in model are present in this signature collection.
#' @param allow_empty Should empty vectors be considered valid models? (flag)
#'
#' @export
#' @rdname model
assert_model <- assertions::assert_create(check_model)

#' Signature Model Specification
#'
#' @inheritParams assert_signature
#'
#' @export
#' @rdname cohort_metadata
#' @return [assert_cohort_metadata()] throws error (if assertion fails) or invisibly returns TRUE if successful.
assert_cohort_metadata <- assertions::assert_create(check_cohort_metadata)


