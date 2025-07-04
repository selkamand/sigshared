#' Convert Raw Counts to Fractional Representation
#'
#' Takes a numeric vector of counts and returns the absolute values divided by the total count.
#' If the sum of absolute counts is zero, the `zero.method` argument controls how to handle that case:
#' - `"zero"` (default): returns a vector of zeros
#' - `"uniform"`: returns a uniform distribution (each element = 1/length(counts))
#'
#' @param counts Numeric vector of counts. Negative values are treated as absolute values.
#' @param zero.method Character; one of `c("zero", "uniform")`.
#'   Specifies behavior when `sum(abs(counts)) == 0`:
#'   - `"zero"`: return a zero vector.
#'   - `"uniform"`: return a uniform probability vector.
#'   Default: `"zero"`.
#' @param validate Logical; if `TRUE` (default), inputs are checked for being numeric, non-NA,
#'   and finite.  If `FALSE`, those checks are skipped (for performance when you know inputs are clean).
#' @return A numeric vector of the same length as `counts`:
#'   - If `sum(abs(counts)) > 0`, returns `abs(counts) / sum(abs(counts))`.
#'   - Otherwise, returns either all zeros or a uniform distribution, as specified.
#' @export
#'
#' @examples
#' # Standard case
#' compute_fraction(c(10, 2, 1000, 50, 250))
#'
#' # All-zero input with default behavior
#' compute_fraction(c(0, 0, 0))
#'
#' # All-zero input, uniform fallback
#' compute_fraction(c(0, 0, 0), zero.method = "uniform")
#'
#' # Skip input validation for speed
#' compute_fraction(c(1,2,3), validate = FALSE)
compute_fraction <- function(counts, zero.method = c("zero", "uniform"), validate = TRUE){

  # Assertions
  if(validate){
    if(!is.numeric(counts)) stop("compute_fraction: 'counts' must be numeric, not a ", toString(class(counts)))
    if(anyNA(counts)) stop("compute_fraction: 'counts' must have no missing values. Found ", sum(is.na(counts)))
    if(any(is.infinite(counts))) stop("compute_fraction: 'counts' must have no infinite values. Found ", sum(is.infinite(counts)))
  }

  total_count <- sum(abs(counts))

  # Deal with cases where all counts are zero
  if(total_count == 0) {

    # Only argument match when necessary (for speed)
    zero.method <- rlang::arg_match(zero.method)

    # If zero.method == "uniform" c(0, 0, 0) will return c(1/3, 1/3, 1/3)
    if(zero.method == "uniform"){
      return(rep(1/length(counts), times = length(counts)))
    }

    # If zero.method == "zero" c(0, 0, 0) will return c(0, 0, 0)
    return(numeric(length(counts)))
  }

  fraction <- abs(counts) / total_count
  return(fraction)
}
