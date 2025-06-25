#' Convert counts into fractions of total counts
#'
#' Convert counts into fractions of total counts, dealing with zero sum counts by setting to zero instead of Inf
#'
#' Uses absolute value of counts for computing fractions.
#'
#' @param counts a numeric vector of counts
#'
#' @returns a numeric vector of fractions
#' @export
#'
#' @examples
#' compute_fraction(c(10, 2, 1000, 50, 250))
#'
compute_fraction <- function(counts){
  assertions::assert_numeric(counts)
  fraction <- abs(counts) / sum(abs(counts), na.rm = TRUE)
  fraction <- ifelse(is_infinite_or_na(fraction), yes = 0, no = fraction)
  return(fraction)
}

is_infinite_or_na <- function(x){
  is.infinite(x) & is.na(x)
}
