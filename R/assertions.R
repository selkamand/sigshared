
# Check Functions ---------------------------------------------------------



check_signature = function(obj){
  required_cols = c('channel', 'type', 'fraction')

  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature: Signature must be represented as a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature: Signature data.frame must contain the following columns: [', paste0(missing_cols, collapse = ","),']'))
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

  #Fractions sum to >1
  if(sum(obj[['fraction']]) > 1){
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature: Sum of fractions must be less than or equal to 1, not ', sum(obj[['fraction']])))
  }

  #Fractions include negative values
  if(any(obj[['fraction']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: Found negative fractions')
  }

  return(invisible(TRUE))
}

check_decomposition = function(obj){
  required_cols = c('channel', 'type', 'count', 'fraction')

  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid decomposition: Signature must be represented as a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid decomposition: Signature data.frame must contain the following columns: [', paste0(missing_cols, collapse = ","),']'))
  }

  # Column Types Unexpected
  if(!is.character(obj[['channel']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: channel column must be of type {.emph character}, not {.emph {class(arg_value[["channel"]])}}')

  if(!is.character(obj[['type']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: type column must be of type {.emph character}, not {.emph {class(arg_value[["type"]])}}')

  if(!is.numeric(obj[['fraction']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: fraction column must be of type {.emph numeric}, not {.emph {class(arg_value[["fraction"]])}}')

  if(!is.numeric(obj[['count']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: count column must be of type {.emph numeric}, not {.emph {class(arg_value[["count"]])}}')

  # Duplicated Channels
  if(anyDuplicated(obj[['channel']])){
    duplicated_channel = obj[['channel']][duplicated(obj[['channel']])]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid decomposition: found duplicated channels (',paste0(duplicated_channel, collapse = ", "), ')'))
  }

  # Missing Values
  if(anyNA(obj)){
    return('{.arg {arg_name}} is {.strong NOT} a valid decomposition: found missing (NA) values')
  }

  # Fractions sum to >1
  if(sum(obj[['fraction']]) > 1){
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid decomposition: sum of fractions must be less than or equal to 1, not ', sum(obj[['fraction']])))
  }

  # Fractions include negative values
  if(any(obj[['fraction']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid decomposition: found negative fractions')
  }

  # Fractions dont make sense given count
  calculated_fraction = obj[['count']]/sum(obj[['count']])
  observed_fraction = obj[['fraction']]

  #browser()
  if(!isTRUE(all.equal(calculated_fraction, observed_fraction))){
    return('{.arg {arg_name}} is {.strong NOT} a valid decomposition: fraction is not explained by counts')
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


check_decomposition_collection <- function(obj){

  # Is not list
  if(!is.list(obj) | is.data.frame(obj))
    return('{.arg {arg_name}} is {.strong NOT} a valid decomposition collection: Collections must be of type {.emph list}, not {class(arg_value)}')

  # Length Greater than 0
  if(length(obj) == 0){
    return('{.arg {arg_name}} is {.strong NOT} a valid decomposition collection: No decompositions are present in the collection')
  }

  # Duplicated names
  signames = names(obj)
  if(anyDuplicated(signames)){
    duplicated_signames = signames[duplicated(signames)]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid decomposition collection: found duplicated decomposition names (',paste0(duplicated_signames, collapse = ", "), ')'))
  }

  # Assert each decomposition in the collection
  for (i in seq_along(obj)){
    sig = obj[[i]]
    name = signames[i]
    err_message = check_decomposition(sig)

    err_message = sub(x=err_message, pattern = "^.*?:(.*)$", replacement = "\\1")
    if(err_message != TRUE) {
      err_message = paste0('{arg_name} is not a valid decomposition collection. Signature {.emph ',name,'} fails the following check:\f\f', err_message, collapse = "")
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
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid signature annotation dataset: Annotation data.frame must contain the following columns: [', paste0(missing_cols, collapse = ","),']'))
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
  required_cols = c("sample", "signature", "contribution_absolute", "contribution", "bootstraps")


  # Not a data.frame
  if(!is.data.frame(obj))
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Cohort analyses must be represented as a data.frame, not a ', class(obj), ''))

  # Missing Colnames
  cols = colnames(obj)
  if(!all(required_cols %in% cols)){
    missing_cols = required_cols[!required_cols %in% cols]
    return(paste0('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Cohort analysis data.frame must contain the following columns: [', paste0(missing_cols, collapse = ","),']'))
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

  if(!is.character(obj[['bootstraps']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: bootstraps column must be of type {.emph numeric}, not {.emph {class(arg_value[["bootstraps"]])}}')


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

  # Contributions include negative values >1
  if(any(obj[['contribution']] < 0)){
    return('{.arg {arg_name}} is {.strong NOT} a valid cohort analysis: Found negative contribution')
  }

  # Contributions include negative values >1
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

# Assertions --------------------------------------------------------------

#' Assert object represents a signature
#'
#' @param obj object
#'
#' @param msg error message
#' @param arg_name argument
#' @param call internal paramater
#'
#' @return Throws error if assertion fails, otherwise invisibly returns true
#' @export
#'
#' @examples
#' # Generate Example Datatypes
#'
#' if(interactive()){
#'
#'   # Signatures
#'   signature = example_valid_signature()
#'   signature_collection = example_valid_signature_collection()
#'   signature_annotations = example_valid_signature_annotations()
#'
#'   # Decompositions
#'   decomposition = example_valid_decomposition()
#'   decomposition_collection = example_valid_decomposition_collection()
#'
#'   # Cohort Analysis Results
#'   cohort_analysis = example_valid_cohort_analysis()
#'
#'   # Cohort Analysis Results
#'   decomposition_collection = example_valid_decomposition_collection()
#'
#'   # Run Assertions
#'   assert_signature(signature)
#'   assert_signature_collection(signature_collection)
#'   assert_decomposition(decomposition)
#'   assert_decomposition_collection(decomposition_collection)
#'   assert_cohort_analysis(cohort_analysis)
#' }
assert_signature <- assertions::assert_create(check_signature)

#' @description
#' Assert object represents a signature collection
#'
#' @inherit assert_signature
#'
#' @export
assert_signature_collection <- assertions::assert_create(check_signature_collection)

#' @description
#' Assert object represents a decomposition
#'
#' @inherit assert_signature
#'
#' @export
assert_decomposition <- assertions::assert_create(check_decomposition)

#' @description
#' Assert object represents a collection of decompositions
#'
#' @inherit assert_signature
#'
#' @export
assert_decomposition_collection <- assertions::assert_create(check_decomposition_collection)

#' @description
#' Assert object represents signature annotations
#'
#' @param required_signatures signatures expected to be annotated. Will throw an error if any of these signatures are missing from the annotation data.frame (character)
#' @inherit assert_signature
#'
#' @export
assert_signature_annotations <- assertions::assert_create(check_signature_annotation)

#' @description
#' Assert object represents results of a cohort analysis
#'
#' @inherit assert_signature
#'
#' @export
assert_cohort_analysis <- assertions::assert_create(check_cohort_analysis)


