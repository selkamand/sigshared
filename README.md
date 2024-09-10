
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sigshared

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/sigshared)](https://CRAN.R-project.org/package=sigshared)
[![R-CMD-check](https://github.com/selkamand/sigshared/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/sigshared/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

**sigshared** contains general utilities used in many other R packages
in the [sigverse](https://github.com/selkamand/sigverse)

Unless you’re developing / maintaining a sigverse package, there is
probably very little reason to ever download this package explicitly. We
reccomend just installing the full sigverse as described
[here](https://github.com/selkamand/sigverse)

## Installation

You can install the development version of sigshared from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("selkamand/sigshared")
```

## Sigverse Data Types

<table style="width:92%;">
<colgroup>
<col style="width: 45%" />
<col style="width: 45%" />
</colgroup>
<thead>
<tr>
<th>Data Structure</th>
<th>Requirements</th>
</tr>
</thead>
<tbody>
<tr>
<td><strong>Signature</strong></td>
<td><p>The profile of a mutational signature</p>
<p>data.frames with 3 columns</p>
<ol type="1">
<li><strong>type</strong></li>
<li><strong>channel</strong></li>
<li><strong>fraction</strong></li>
</ol></td>
</tr>
<tr>
<td><strong>Signature Collections</strong></td>
<td>Lists of signature data.frames, where name of list entry is the name
of the signature</td>
</tr>
<tr>
<td><strong>Signature Annotations</strong></td>
<td><p>Signature level annotations. data.frames with 4 required
columns:</p>
<ol type="1">
<li><strong>signature</strong></li>
<li><strong>aetiology</strong></li>
<li><strong>class</strong></li>
<li><strong>subclass</strong></li>
</ol>
<p>class and subclass of aetiology do not need to conform to any
specific ontology. However, we include the data-dictionary used by
sigstash collections below (see <a
href="#signature-aetiology-classes">Signature Aetiology
Classes</a>)</p></td>
</tr>
<tr>
<td><strong>Catalogue</strong></td>
<td><p>The mutational profile of a sample, described by tallying
mutations belonging to each mutational channel. Catalogues are not
always observational. They can also be simulated from signature models.
data.frames with 4 required columns:</p>
<ol type="1">
<li><p><strong>channel</strong></p></li>
<li><p><strong>type</strong></p></li>
<li><p><strong>fraction</strong></p></li>
<li><p><strong>count</strong></p></li>
</ol></td>
</tr>
<tr>
<td><strong>Catalogue Collections</strong></td>
<td>Lists of catalogue data.frames (1 per sample) where name represents
a sample identifier.</td>
</tr>
<tr>
<td><strong>Cohort Signature Analysis Results</strong></td>
<td><p>data.frame with 4 columns:</p>
<ol type="1">
<li><p><strong>sample</strong></p></li>
<li><p><strong>signature</strong></p></li>
<li><p><strong>contribution_absolute</strong></p></li>
<li><p><strong>contribution</strong></p></li>
<li><p><strong>p_value</strong> (see `?sigstats::sig_com
pute_experimental_p_value()`)</p></li>
</ol></td>
</tr>
<tr>
<td><strong>Bootstraps</strong></td>
<td><p>data.frame with 1 row per signature per bootstrap</p>
<ol type="1">
<li><strong>bootstrap</strong></li>
<li><strong>signature</strong></li>
<li><strong>contribution_absolute</strong></li>
<li><strong>contribution (percentage)</strong></li>
</ol></td>
</tr>
</tbody>
</table>

### Assertions

You can assert an object belongs to each of data types

``` r
library(sigshared)

# Generate Example Datatypes

# Signatures
signature = example_signature()
signature_collection = example_signature_collection()
signature_annotations = example_annotations()

# catalogues
catalogue = example_catalogue()
catalogue_collection = example_catalogue_collection()

# Cohort Analysis Results
cohort_analysis = example_cohort_analysis()


# Assert Signatures
assert_signature(signature)
assert_signature_collection(signature_collection)
assert_signature_annotations(signature_annotations)

# Assert catalogues
assert_catalogue(catalogue)
assert_catalogue_collection(catalogue_collection)

# Assert Analyses
assert_cohort_analysis(cohort_analysis)
```

### Signature Aetiology Classes

``` r
library(knitr)
kable(sig_aetiology_classes())
```

| class                         | subclass                            |
|:------------------------------|:------------------------------------|
| artefact                      | 8-oxo-guanine                       |
| artefact                      | sequencing_artefact                 |
| artefact                      | germline_contamination              |
| artefact                      | oversegmentation                    |
| clock-like                    | clock-like                          |
| dysfunctional_dna_repair      | MMR                                 |
| dysfunctional_dna_repair      | HR                                  |
| dysfunctional_dna_repair      | NER                                 |
| dysfunctional_dna_repair      | BER                                 |
| dysfunctional_dna_repair      | NHEJ                                |
| dysfunctional_dna_replication | proofreading                        |
| dysfunctional_dna_replication | polymerase_mutations                |
| treatment_associated          | chemotherapy_platinum               |
| treatment_associated          | chemotherapy_thiopurine             |
| treatment_associated          | chemotherapy_pyrimidine_antagonists |
| treatment_associated          | chemotherapy_unknown                |
| treatment_associated          | chemotherapy_nitrogen_mustards      |
| treatment_associated          | triazenes                           |
| treatment_associated          | immunosuppression                   |
| environmental_mutagens        | tobacco                             |
| environmental_mutagens        | haloalkanes                         |
| environmental_mutagens        | UV                                  |
| environmental_mutagens        | aristolochic_acid                   |
| plants_and_microbes           | aflatoxin                           |
| plants_and_microbes           | colibactin                          |
| adenosine_deamination         | adenosine_deaminases                |
| cytosine_deamination          | cytidine_deaminases                 |
| cytosine_deamination          | cytosine_deamination                |
| immune                        | ROS                                 |
| dysfunctional_epigenetics     | topology                            |
| chromosomal                   | chromosomal_losses                  |
| chromosomal                   | chromosomal_instability             |
| chromosomal                   | chromothripsis                      |
| ploidy                        | diploid                             |
| ploidy                        | tetraploid                          |
| unknown                       | unknown                             |

### For Developers

#### Argument Naming

*Catalogue*

    catalogue: The mutational profile of a sample, described by tallying mutations belonging to each mutational channel. Catalogues are not always observational. They can also be simulated from signature models. Must be a sigverse-style data.frame (contain colums: channel, type, fraction, count). See \href{https://github.com/selkamand/sigshared?tab=readme-ov-file#sigverse-data-types}{sigshared readme} for details.

*Signature*

    signature: The profile of a mutational signature. Must be a data.frame (with columns: type, channel & fraction). See \href{https://github.com/selkamand/sigshared?tab=readme-ov-file#sigverse-data-types}{sigshared readme} for details.

*Signature collection*

    signatures:     A sigverse signature collection. A named list of sigstash signature data.frames. See \href{https://github.com/selkamand/sigshared?tab=readme-ov-file#sigverse-data-types}{sigshared readme} for details.

*Catalogue collection*

    catalogues: A sigverse-style catalogue collection. A named list of sigstash catalogue data.frames. See \href{https://github.com/selkamand/sigshared?tab=readme-ov-file#sigverse-data-types}{sigshared readme} for details.

*Cohort Signature Analysis Results*

    cohort: A sigverse-style data.frame describing the contributions of signatures in each sample (must contain columns: sample, signature contribution_absolute, contribution). See \href{https://github.com/selkamand/sigshared?tab=readme-ov-file#sigverse-data-types}{sigshared readme} for details.
