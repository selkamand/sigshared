
#' Create a sigstory Visualization Object
#'
#' Constructs a \code{sigstory} object containing visualizations and data related to mutational signature analysis.
#' Many of the visualizations required for this object can be produced using the \code{sigvis} package.
#'
#' @param sample String. Sample Identifier.
#' @param sigclass String. The class of signature features used (e.g., SBS96, ID83, SV32, etc).
#' @param number_of_mutations Number. Total number of mutations in this sigclass.
#' @param model_fit Number. Measured as cosine similarity between observed profile vs model.
#' @param unexplained_mutations Number. Number of mutations not explained by signature.
#' @param ls_signatures_minified List. A list of signatures (in model) rendered as minified ggplots. Names are concatenations of each signature and its aetiology.
#' @param ls_exposure_dotplots List (optional). A list of signatures (in model) rendered as dotplots describing contribution size compared to a reference cohort. Default is \code{NULL}.
#' @param ls_similar_samples List (optional). A list of the most similar samples in the reference cohort (minified with similarity). Names should be sample ID concatenated with 'disease'. Default is \code{NULL}.
#' @param ls_similar_sample_disease_donut ggplot (optional). A donut plot of the diseases of the top N similar samples. Default is \code{NULL}.
#' @param umap ggplot (optional). UMAP plot showing how sample clusters against the reference cohort. Default is \code{NULL}.
#' @param stability ggplot (optional). Signature stability plot (bootstraps). Default is \code{NULL}.
#' @param analysis_details data.frame. A data.frame describing the details of the analysis (software versions, thresholds, reference cohorts, etc).
#'
#' @return An object of class \code{sigstory} containing the provided data and visualizations.
#' @note Many of the visualizations required for this object can be produced using the \code{sigvis} package.
#' @export
#'
#' @examples
#' \dontrun{
#' sigstory_obj <- sigstory_visualisation(
#'   sample = "Sample1",
#'   sigclass = "SBS96",
#'   number_of_mutations = 500,
#'   model_fit = 0.95,
#'   unexplained_mutations = 25,
#'   ls_signatures_minified = list(Sig1 = ggplot_obj1, Sig2 = ggplot_obj2),
#'   ls_exposure_dotplots = list(Sig1 = ggplot_obj3),
#'   ls_similar_samples = list(SampleA = ggplot_obj4),
#'   ls_similar_sample_disease_donut = ggplot_obj5,
#'   umap = ggplot_obj6,
#'   stability = ggplot_obj7,
#'   analysis_details = data.frame(Detail = "Info")
#' )
#' }
sigstory_visualisation <- function(
    sample, sigclass, number_of_mutations, model_fit, unexplained_mutations,
    ls_signatures_minified, ls_exposure_dotplots = NULL, ls_similar_samples = NULL,
    ls_similar_sample_disease_donut = NULL, umap = NULL, stability = NULL, analysis_details
) {
  ls <- list(
    sample = sample,  # Sample Identifier
    sigclass = sigclass,  # The class of signature features used (e.g., SBS96, ID83, SV32, etc)
    analysis_details = analysis_details,  # Table describing analysis details
    number_of_mutations = number_of_mutations,  # Total number of mutations in this sigclass
    model_fit = model_fit,  # Measured as cosine similarity between observed profile vs model
    unexplained_mutations = unexplained_mutations,  # Number of mutations not explained by signature
    ls_signatures_minified = ls_signatures_minified,  # A list of signatures rendered as minified ggplots
    ls_exposure_dotplots = ls_exposure_dotplots,  # A list of dotplots describing contribution size
    ls_similar_samples = ls_similar_samples,  # Most similar samples in the reference cohort
    ls_similar_sample_disease_donut = ls_similar_sample_disease_donut,  # Donut plot of diseases of top similar samples
    umap = umap,  # UMAP plot showing sample clustering
    stability = stability  # Signature stability plot (bootstraps)
  )

  structure(ls, class = "sigstory")
}
