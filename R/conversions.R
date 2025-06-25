#' Convert Signature or Catalogue Collection to Matrix
#'
#' Transforms a signature or catalogue collection into a matrix format,
#' with channels as rows and signatures as columns.
#'
#' The resulting matrix includes an attribute `type`. This vector describing mutation type of each row (channel) in matrix.
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
#' mat <- sig_collection_reformat_list_to_matrix(collection)
#'
#' # Convert a catalogue collection using counts
#' catalogue <- example_catalogue_collection()
#' mat_counts <- sig_collection_reformat_list_to_matrix(catalogue, values = "count")
sig_collection_reformat_list_to_matrix <- function(signatures, values = c("fraction", "count")){

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
  ls <- lapply(seq_along(signatures), function(i){
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
  attr(mx_wide, which = "type") <- first_sig_type_order

  return(mx_wide)
}

#' Convert Signature or Catalogue Collection to a Tidy Data Frame
#'
#' Converts a collection of signatures or catalogues data frames into a single tidy data frame.
#'
#' The function automatically detects whether the input is a signature collection or a catalogue collection,
#' and appends a column (named after the collection type) to identify the originating signature/catalogues.
#'
#' @param signatures A signature or catalogue collection. A named list of data frames, each containing
#'   mutation information (see [example_signature_collection()] or [example_catalogue_collection()]).
#'
#' @return A tidy data frame with columns:
#' - `signature` or `catalogue`: the name of the signature
#' - `type`: the mutation type (e.g., "T>C")
#' - `channel`: the channel (e.g., "A[T>C\\]G")
#' - `fraction`: the normalized mutation fraction
#' - `count` (only for catalogue collections): the mutation count
#'
#' @export
#'
#' @examples
#' sigs <- example_signature_collection()
#' tidy_df <- sig_collection_reformat_list_to_tidy(sigs)
#'
#' cats <- example_catalogue_collection()
#' tidy_cat <- sig_collection_reformat_list_to_tidy(cats)
sig_collection_reformat_list_to_tidy <- function(signatures) {

  # Validate that input is a well-formed signature or catalogue collection
  assert_signature_collection(signatures)

  # Determine whether this is a 'signature' or 'catalogue' collection,
  # based on presence of a 'count' column
  collection_type <- infer_collection_type(signatures)

  # Define the output columns, depending on collection type
  columns <- if (collection_type == "catalogue")
    c(collection_type, "type", "channel", "count", "fraction")
  else
    c(collection_type, "type", "channel", "fraction")

  # For each signature in the collection:
  # - Add a column identifying the originating signature (named after collection type)
  # - Return the updated data frame
  signatures <- lapply(seq_along(signatures),  function(i) {
    signatures[[i]][[collection_type]] <- names(signatures)[i]
    return(signatures[[i]])
  })

  # Combine all data frames into one tidy data frame
  df <- do.call(rbind, signatures)
  rownames(df) <- NULL

  # Reorder columns to match expected tidy structure
  df <- df[columns]

  return(df)
}


#' Infer Collection Type
#'
#' Determines whether a collection is a catalogue or a signature collection,
#' based on the presence of the 'count' column in its entries.
#'
#' @param collection A validated signature or catalogue collection.
#' @return A string: either `"catalogue"` or `"signature"`.
infer_collection_type <- function(collection){
  assert_signature_collection(collection)
  if("count" %in% colnames(collection[[1]]))
    "catalogue"
  else
    "signature"
}

#' Reformat Tidy Signature or Catalogue Data to List Format
#'
#' Converts a tidy data frame of mutational signatures or catalogues
#' named list of data frames, each with columns `channel`, `type`, and `fraction`,
#' and (for catalogues) `count`.
#'
#' The function supports input with an identifier column named `signature`, `catalogue`, or `sample`.
#' The output is a named list of tibbles indexed by that identifier.
#'
#' @param signatures A tidy `data.frame` with columns: `type`, `channel`, `fraction`, and optionally `count`,
#' and an ID column: `signature`, `catalogue`, or `sample`.
#'
#' @return A signature/catalogue collection (see [example_signature_collection()] and [example_catalogue_collection()])
#'
#' @export
#'
#' @examples
#' tidysigs <- example_signature_collection_tidy()
#' sig_collection_reformat_tidy_to_list(tidysigs)
#'
#' tidycatalogues <- example_catalogue_collection_tidy()
#' sig_collection_reformat_tidy_to_list(tidycatalogues)
#'
sig_collection_reformat_tidy_to_list <- function(signatures) {

  # Ensure input is a dataframe
  assertions::assert_dataframe(signatures)

  # Identify column used to distinguish signatures/samples
  id_col <- intersect(c("signature", "catalogue", "sample"), colnames(signatures))
  assertions::assert_length_greater_than(
    id_col, 0,
    msg = "Failed to find an obvious ID column. Please add one of: 'signature', 'catalogue', or 'sample'."
  )
  assertions::assert_length(
    id_col, 1,
    msg = "Found multiple potential ID columns: {id_col}. Please remove all but one."
  )

  # Validate required columns
  assertions::assert_names_include(signatures, c(id_col, "type", "channel", "fraction"))
  assertions::assert_numeric(signatures[["fraction"]])
  if ("count" %in% colnames(signatures)) {
    assertions::assert_numeric(signatures[["count"]])
  }

  # Assert unique (ID + channel) combinations
  assertions::assert_no_duplicates(signatures[c(id_col, "channel")])

  # Convert the ID column to a factor to preserve order during split
  signatures[[id_col]] <- factor(signatures[[id_col]], levels = unique(signatures[[id_col]]))

  # Choose columns to include
  data_cols <- c("type", "channel", "fraction")
  if ("count" %in% colnames(signatures)) {
    data_cols <- c(data_cols, "count")
  }

  # Split and convert
  ls_data <- split(signatures[data_cols], signatures[[id_col]])
  # ls_data <- lapply(ls_data, tibble::as_tibble)

  return(ls_data)
}


#' Convert Matrix of Signatures or Catalogues to List Format
#'
#' Converts a matrix of mutational signatures or catalogues back into a named list of `data.frame`s
#' in the sigverse format. Each column in the matrix is treated as one signature or catalogue.
#'
#' This is the inverse of [sig_collection_reformat_list_to_matrix()]. It supports both
#' signature-style (fraction-only) and catalogue-style (count + fraction) output, depending on the
#' `values` parameter and matrix content.
#'
#' A `type` attribute is optional but recommended on the matrix to map each channel to a mutation type.
#' If `types` is not provided explicitly, the function uses this attribute or falls back to reusing
#' channel names as `type`.
#'
#' @param mx A numeric matrix with:
#'   - **Rows**: Channels (must be named and unique)
#'   - **Columns**: Signatures or catalogues (must be named and unique)
#'   - **Values**: Fractions (for signatures) or counts (for catalogues)
#' @param values Either `"count"` or `"fraction"` (default = `"count"`). This determines how the matrix
#'   values are interpreted. If `"count"` is chosen, `fraction` will be calculated automatically.
#' @param types A character vector describing the mutation `type` of each row/channel`. If `NULL`, this is
#'   inferred from the matrix's `type` attribute or defaulted to rownames.
#' @param verbose Logical. If `TRUE` (default), emits a warning if the matrix values look inconsistent
#'   with the chosen `values` argument (e.g., fractional-looking values with `values = "count"`).
#'
#' @return A named list of signature or catalogue `data.frame`s. Each `data.frame` will contain:
#'   - `channel`: Row names of the matrix
#'   - `type`: As provided or inferred
#'   - `fraction`: Matrix column or computed from `count`
#'   - `count`: Only included if `values = "count"`
#'
#' @seealso [sig_collection_reformat_list_to_matrix()], [sig_collection_reformat_list_to_tidy()], [sig_collection_reformat_tidy_to_list()]
#'
#' @examples
#' # From matrix of signatures
#' mx <- example_signature_collection_matrix()
#' collection <- sig_collection_reformat_matrix_to_list(mx, values = "fraction")
#'
#' # From matrix of catalogues
#' mx_cat <- example_catalogue_collection_matrix()
#' collection_cat <- sig_collection_reformat_matrix_to_list(mx_cat, values = "count")
#'
#' # Edge case: matrix with no 'type' attribute
#' # (will use channel names)
#' mx_notyped <- example_signature_collection_matrix()
#' attr(mx_notyped, "type") <- NULL
#' sig_collection_reformat_matrix_to_list(mx_notyped, values = "fraction")
#'
#' # Custom types
#' mx_min <- matrix(
#'   c(0.1, 0.3, 0.6,
#'     0.2, 0.5, 0.3),
#'   nrow = 3,
#'   ncol = 2,
#'   dimnames = list(
#'     c("C>A", "C>G", "C>T"),
#'     c("S1", "S2")
#'   )
#' )
#' attr(mx_min, "type") <- c("SNV", "SNV", "SNV")
#' sig_collection_reformat_matrix_to_list(mx_min, values = "fraction")
#' @export
sig_collection_reformat_matrix_to_list <- function(mx, values = c("count", "fraction"), types=NULL, verbose = TRUE){

  # Assertions
  assertions::assert_matrix(mx)
  assertions::assert_numeric(mx)
  assertions::assert_non_null(rownames(mx))
  assertions::assert_non_null(colnames(mx))
  assertions::assert(all(nzchar(colnames(mx))), msg = "matrix to list conversion requires column names are all non-zero length strings")
  assertions::assert(all(nzchar(rownames(mx))), msg = "matrix to list conversion requires row names are all non-zero length strings")
  assertions::assert_no_duplicates(rownames(mx))
  assertions::assert_no_duplicates(colnames(mx))
  values = rlang::arg_match(values)
  if(!is.null(types)) {
    assertions::assert_character(types)
    assertions::assert_length(types, length = nrow(mx))
  }

  channels <- rownames(mx)
  samplenames <- colnames(mx)

  values_look_fractional <- all(mx <= 1 & mx >= 0) & any(mx < 1 & mx > 0)

  # Warnings to set values argument appropriately
  if(verbose & values == "count" & values_look_fractional){
    cli::cli_alert_warning("The matrix supplied contains only fractional values. If this matrix represents a signature collection, please remember to set {.emph values}='fraction'")
  }

  if(verbose & values == "fraction" & !values_look_fractional){
    cli::cli_alert_warning("The matrix supplied does not contain fractional values between zero and one. If this matrix represents a catalogue collection, please remember to set {.emph values}='count'")
  }

  # If types are not specified, search for matrix attribute,
  # and if thats not found, just use channel names
  if(is.null(types)){
    types <- attributes(mx)[["type"]]
    if (is.null(types)) {
      types <- rownames(mx)
      if (verbose) cli::cli_alert_info("No 'type' attribute found; using channel names as types.")
    }
  }


  # Split matrix into list
  collection <- apply(
    X = mx,
    MARGIN = 2,
    simplify = FALSE,
    function(colvector){
      if(values == "count"){
        data.frame(
          channel = channels,
          type = types,
          count = colvector,
          fraction = compute_fraction(colvector),
          row.names = NULL
        )
      }
      else {
        data.frame(
          channel = channels,
          type = types,
          fraction = colvector,
          row.names = NULL
        )
      }
    }
  )

  names(collection) <- samplenames

  return(collection)
}
