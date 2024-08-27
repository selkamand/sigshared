#' Signature Aetiology Data-Dictionary
#'
#'
#' @return a data.frame describing valid terms to describe signature aetiology class/subclass
#' @export
#'
#' @examples
#' sig_aetiology_classes()
sig_aetiology_classes <- function(){
  df <- utils::read.csv(system.file(package = "sigshared", "signature_aetiology_classes.csv"))
  df[!colnames(df) %in% c("notes")]
}
