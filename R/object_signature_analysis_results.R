
#' Create a signature_analysis_result Object
#'
#' Constructs a \code{signature_analysis_result} object containing all the information required to generate a [sigstory_visualisation()] object.
#'
#' @param sample String. Sample Identifier.
#' @param sigclass String. The class of signature features used (e.g., SBS96, ID83, SV32, etc).
#' @param model A named numeric vector where names are signatures and values are their proportional contributions to the model.
#' @param signatures The signature collection used for fitting, in sigstash format (see [sigshared::example_signature_collection()] and [sigstash::sig_load()])
#' @param signature_annotations Signature annotations describing aetiology for signatures in signatures (see [sigshared::example_annotations()] and [sigstash::sig_load_annotations()])
#' @param number_of_mutations Number. Total number of mutations in this sigclass.
#' @param model_fit Number. Measured as cosine similarity between observed profile vs model.
#' @param unexplained_mutations Number. Number of mutations not explained by signature.
#' @param catalogue data.frame. Observed mutation catalogue of the sample. See [sigshared::example_catalogue].
#' @inheritParams sigvis::sig_visualise_bootstraps
#' @param cohort_exposures data.frame. A cohort of signature analysis results used to produce cohort distribution dotplots. See [sigshared::example_cohort_analysis()]
#' @param similarity_against_cohort data.frame describing how similar the observed mutational catalogue is to other samples. See [sigshared::example_similarity_against_cohort()]
#' @param cohort_catalogues a sigverse collection of catalogues in the reference-cohort which the sample was compared against. Must at least include a decomposition for all samples in similarity_against_cohort. Used to create similar sample plots. See [sigshared::example_catalogue_collection()] for example format.
#' @param cohort_metadata data.frame. Sample level metadata describing every sample in \code{cohort_catalogues} See [sigshared::example_cohort_metadata()].
#' @param umap data.frame. UMAP datasetframe showing how the catalogue of the sample-of-interest clusters against cohort_metadata. See [sigshared::example_umap()].
#' @param analysis_details data.frame. describing the details of the analysis (software versions, thresholds, reference cohorts, etc).
#'
#' @return An object of class \code{sigstory} containing the provided data and visualizations.
#' @note Many of the visualizations required for this object can be produced using the \code{sigvis} package.
#' @export
#'
signature_analysis_result <- function(
    sample,
    sigclass,
    model,
    number_of_mutations,
    model_fit,
    unexplained_mutations,
    catalogue,
    signatures,
    signature_annotations,
    analysis_details,
    bootstraps,
    cohort_exposures = NULL,
    similarity_against_cohort = NULL,
    cohort_catalogues = NULL,
    cohort_metadata = NULL,
    umap = NULL
) {

  # Assertions
  assertions::assert_string(sample)
  assertions::assert_string(sigclass)
  assert_model(model)
  assert_bootstraps(bootstraps)
  assert_catalogue(catalogue)

  assertions::assert_number(number_of_mutations)
  assertions::assert_number(model_fit)
  assertions::assert_number(unexplained_mutations)
  assertions::assert_dataframe(analysis_details)

  if(!is.null(cohort_exposures)) assert_cohort_analysis(cohort_exposures)
  if(!is.null(cohort_catalogues)) assert_catalogue_collection(cohort_catalogues)
  if(!is.null(cohort_metadata)) assert_cohort_metadata(cohort_metadata)
  if(!is.null(cohort_catalogues)) assertions::assert(!is.null(cohort_metadata), msg = "{.arg cohort_metadata} argument is required when {.arg cohort_catalogues} are supplied")
  if(!is.null(cohort_catalogues)) assertions::assert(!is.null(cohort_metadata), msg = "{.arg cohort_metadata} argument is required when {.arg cohort_catalogues} are supplied")
  if(!is.null(cohort_metadata)) assertions::assert(!is.null(cohort_catalogues), msg = "{.arg cohort_catalogues} argument is required when {.arg cohort_metadata} is supplied")
  if(!is.null(umap)) assert_umap(umap)
  if(!is.null(umap)) assertions::assert(!is.null(cohort_metadata), msg = "{.arg cohort_metadata} argument is required when {.arg umap} is supplied")

  # Assert that if similarity_against_cohort is supplied, so are a database of cohort_catalogues.
  if(!is.null(similarity_against_cohort)) {
    assertions::assert(!is.null(cohort_catalogues), msg = "{.arg cohort_catalogues} must be supplied when similarity_against_cohort is not NULL")
    catalogue_samples <- names(cohort_catalogues)
    missing_catalogue_samples <- setdiff(catalogue_samples, similarity_against_cohort[["sample"]])
    n_missing = length(missing_catalogue_samples)
    assertions::assert(n_missing == 0, msg = "All samples described in {.arg similarity_against_cohort} must be present in {.arg cohort_catalogues}. Missing {n_missing} samples: [{missing_catalogue_samples}]")
  }

  # Construct Object
  ls <- list(
    sample = sample,  # Sample Identifier
    sigclass = sigclass,  # The class of signature features used (e.g., SBS96, ID83, SV32, etc)
    analysis_details = analysis_details,  # Table describing analysis details
    catalogue = catalogue, # Catalogue of mutations in the sample
    model = model, # Optimal signature model
    number_of_mutations = number_of_mutations,  # Total number of mutations in this sigclass
    model_fit = model_fit,  # Measured as cosine similarity between observed profile vs model
    unexplained_mutations = unexplained_mutations,  # Number of mutations not explained by signature
    bootstraps = bootstraps, # Bootstrap performance
    signatures, # Signature collection used for fitting
    signature_annotations, # Annotations
    cohort_exposures = cohort_exposures,
    similarity_against_cohort = similarity_against_cohort,
    cohort_catalogues = cohort_catalogues,
    cohort_metadata = cohort_metadata,
    umap = umap
  )

  # Set Class
  structure(ls, class = "signature_analysis_result")
}

