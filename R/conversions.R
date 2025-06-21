#' Convert Signature or Catalogue Collection to Matrix
#'
#' Transforms a signature or catalogue collection into a matrix format,
#' with channels as rows and signatures as columns.
#'
#' The resulting matrix includes an attribute `types`, which maps each channel to its corresponding mutation type.
#' This is a named character vector: names are mutation types, and values are channel labels.
#'
#' @param signatures A sigverse signature or catalogue collection. A named list of sigstash signature data.frames.
#' See [example_signature_collection()] or [example_catalogue_collection()].
#' @param values Either `"fraction"` (default) or `"count"`. Use `"count"` only with catalogue collections.
#'
#' @return A numeric matrix with:
#'   - **Rows**: Channels
#'   - **Columns**: Signature names
#'   - **Values**: Fractions or counts (based on `values`)
#'
#'   Includes a `types` attribute mapping mutation types to their corresponding channels.
#' @export
#'
#' @examples
#' # Convert a signature collection
#' collection <- example_signature_collection()
#' mat <- sig_collection_to_matrix(collection)
#'
#' # Convert a catalogue collection using counts
#' catalogue <- example_catalogue_collection()
#' mat_counts <- sig_collection_to_matrix(catalogue, values = "count")
sig_collection_to_matrix <- function(signatures, values = c("fraction", "count")){

  assertions::assert_greater_than(length(signatures), minimum = 0)
  assert_signature_collection(signatures)

  # If user requested 'count' values, validate that input is a catalogue collection
  values <- rlang::arg_match(values)
  if(values == "count")
    assert_catalogue_collection(signatures, msg = "To return counts `signatures` object must be a catalogue collection.")

  # Record channel from the first signature to enforce consistency
  first_sig_channel_order <- signatures[[1]][["channel"]]
  first_sig_type_order <- signatures[[1]][["type"]]

  # For each signature:
  # - Validate that channel order matches the first signature (ensures row alignment)
  # - Extract the requested value column ('fraction' or 'count') as a single-column data frame
  # - Rename the column to the signature name for clarity
  # This prepares a list of aligned columns ready to be combined into a matrix
  ls <- lapply(seq_along(signatures), FUN = \(i){
    sig <- signatures[[i]]
    assertions::assert_identical(
      first_sig_channel_order,
      sig[["channel"]],
      msg = "All signatures must have identical channel ordering"
    )
    df_fraction <- sig[, values, drop=FALSE]
    colnames(df_fraction) <- names(signatures)[i]
    return(df_fraction)
  })

  # Combine columns from all signatures into a single matrix
  mx_wide <- do.call("cbind", ls)
  mx_wide <- as.matrix(mx_wide)
  rownames(mx_wide) <- first_sig_channel_order

  # Attach 'types' attribute: names are mutation types, values are corresponding channels
  channels <- first_sig_channel_order
  names(channels) <- first_sig_type_order
  attr(mx_wide, which = "types") <- channels

  return(mx_wide)
}
