check_sig = function(obj){
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
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: channel column must be of type {.emph character}, not {.emph {class(arg_value[["type"]])}}')

  if(!is.numeric(obj[['fraction']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: channel column must be of type {.emph numeric}, not {.emph {class(arg_value[["numeric"]])}}')


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
  if(sum(obj[['fraction']] >= 1)){
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
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: channel column must be of type {.emph character}, not {.emph {class(arg_value[["type"]])}}')

  if(!is.numeric(obj[['fraction']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: channel column must be of type {.emph numeric}, not {.emph {class(arg_value[["numeric"]])}}')

  if(!is.numeric(obj[['count']]))
    return('{.arg {arg_name}} is {.strong NOT} a valid signature: channel column must be of type {.emph numeric}, not {.emph {class(arg_value[["count"]])}}')

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
  if(sum(obj[['fraction']] >= 1)){
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

check_sig_collection <- function(obj){

  # Is not list
  if(!is.list(obj))
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
    err_message = check_sig(sig)

    err_message = sub(x=err_message, pattern = "^.*?:(.*)$", replacement = "\\1")
    if(err_message != TRUE) {
      err_message = paste0('{arg_name} is not a valid signature collection. Signature {.emph ',name,'} fails the following check:\f\f', err_message, collapse = "")
      return(err_message)
    }
  }

  # Return TRUE if passing
  return(invisible(TRUE))
}

assert_signature <- assertions::assert_create(check_sig)

assert_signature_collection <- assertions::assert_create(check_sig_collection)

assert_decomposition <- assertions::assert_create(check_decomposition)


#
# assert_decomposition <- function(obj){
#
# }
#
# assert_decomposition_collection <- function(obj){
#
# }
#
#
# assert_signature_collection <- function(obj){
#
# }
#
# assert_cohort_analysis <- function(obj){
#
# }
#
#

# is_sig = function(obj){
#  if()
# }
