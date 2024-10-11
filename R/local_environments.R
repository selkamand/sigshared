#' Run code with a specific random seed without disturbing the global environment
#'
#' Temporarily sets a specific random seed to ensure reproducibility for the
#' provided expression. Once the expression is evaluated, the previous random
#' seed (if it existed) is restored, ensuring that the global random seed
#' state remains unchanged.
#'
#' @param seed [numeric] The random seed to temporarily set during the evaluation of `expr`.
#' @param expr [expression] The expression to be evaluated with the temporary random seed.
#'
#' @return The result of evaluating the expression `expr`.
#' @export
#'
#' @examples
#' # Run a block of code with a specific random seed
#' with_temp_seed(123, {
#'   runif(1)
#' })
#'
#' # The global random seed remains unchanged after running the expression
#' runif(1)
with_temp_seed <- function(seed, expr) {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- .Random.seed
  } else {
    old_seed <- NULL
  }

  set.seed(seed)
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  force(expr)
}
