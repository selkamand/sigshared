
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
<li><strong>channel</strong></li>
<li><strong>type</strong></li>
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
<li><p><strong>p_value</strong> (see `?sigstats::sig_com p</p></li>
</ol>
<p>ute_experimental_p_value()`)</p></td>
</tr>
<tr>
<td><strong>Bootstraps</strong></td>
<td><p>data.frame with 1 row per signature per bootstrap</p>
<ol type="1">
<li><strong>bootstrap</strong></li>
<li><strong>signature</strong></li>
<li><strong>contribution_absolute</strong></li>
<li><strong>contribution</strong> (percentage)</li>
</ol></td>
</tr>
<tr>
<td><strong>Model Specification</strong></td>
<td>Named numeric vector where names represent signatures and values
represent their proportional contribution to the model.</td>
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

# Catalogues
catalogue = example_catalogue()
catalogue_collection = example_catalogue_collection()

# Cohort Analysis Results
cohort_analysis = example_cohort_analysis()

# Model
model = example_model()

# Assert Signatures
assert_signature(signature)
assert_signature_collection(signature_collection)
assert_signature_annotations(signature_annotations)

# Assert catalogues
assert_catalogue(catalogue)
assert_catalogue_collection(catalogue_collection)

# Assert Analyses
assert_cohort_analysis(cohort_analysis)

# Assert Model
assert_model(model, signature_collection,arg_name = "bob")
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

*Signature Model Specification*

    model: A named numeric vector where names represent signatures and values represent their proportional contribution to the model. See \href{https://github.com/selkamand/sigshared?tab=readme-ov-file#sigverse-data-types}{sigshared readme} for details.

## 

## Example Data

For each data structure used by the sigverse we include a toy example.

``` r
example_annotations()
#>   signature                       aetiology               class
#> 1      sig1          A clock like signature          clock-like
#> 2      sig2 An AID/APOBEC related signature cytidine deaminases
#>              subclass
#> 1          clock-like
#> 2 cytidine deaminases
example_bootstraps()
#>   bootstrap  signature contribution_absolute contribution
#> 1         1 Signature1                   300         0.30
#> 2         1 Signature2                   690         0.69
#> 3         1 Signature3                    10         0.01
#> 4         2 Signature1                   440         0.44
#> 5         2 Signature2                   500         0.50
#> 6         2 Signature3                    60         0.06

example_catalogue()
#>    channel type count  fraction
#> 1 A[A->G]G  A>G     5 0.1851852
#> 2 A[A->G]C  A>G    10 0.3703704
#> 3 A[A->G]T  A>G    12 0.4444444
example_catalogue_collection()
#> $decomp1
#>    channel type count  fraction
#> 1 A[A->G]G  A>G     5 0.1851852
#> 2 A[A->G]C  A>G    10 0.3703704
#> 3 A[A->G]T  A>G    12 0.4444444
#> 
#> $decomp2
#>    channel type count  fraction
#> 1 A[A->G]G  A>G     5 0.1851852
#> 2 A[A->G]C  A>G    10 0.3703704
#> 3 A[A->G]T  A>G    12 0.4444444
#> 
#> $decomp3
#>    channel type count  fraction
#> 1 A[A->G]G  A>G     5 0.1851852
#> 2 A[A->G]C  A>G    10 0.3703704
#> 3 A[A->G]T  A>G    12 0.4444444

example_signature()
#>    channel type fraction
#> 1 A[A->G]G  A>G      0.4
#> 2 A[A->G]C  A>G      0.1
#> 3 A[A->G]T  A>G      0.5
example_signature_collection()
#> $sig1
#>    channel type fraction
#> 1 A[A->G]G  A>G      0.4
#> 2 A[A->G]C  A>G      0.1
#> 3 A[A->G]T  A>G      0.5
#> 
#> $sig2
#>    channel type fraction
#> 1 A[A->G]G  A>G      0.4
#> 2 A[A->G]C  A>G      0.1
#> 3 A[A->G]T  A>G      0.5

example_cohort_analysis()
#>    sample signature contribution_absolute contribution p_value
#> 1 sample1      sig1                     3          0.3    0.05
#> 2 sample1      sig2                     7          0.7    0.10
#> 3 sample2      sig1                    40          0.4    0.20
#> 4 sample2      sig2                    60          0.6    0.15
```

We also include examples from real SBS mutational signature analysis of
the colo829 melanoma cell line

``` r
head(example_catalogue_colo829())
#>   channel type     fraction count
#> 1 A[C>A]A  C>A 0.0035802073   134
#> 2 A[C>A]C  C>A 0.0016565138    62
#> 3 A[C>A]G  C>A 0.0004542054    17
#> 4 A[C>A]T  C>A 0.0015229240    57
#> 5 A[C>G]A  C>G 0.0016565138    62
#> 6 A[C>G]C  C>G 0.0011488725    43
head(example_bootstraps_colo829())
#>   signature bootstrap contribution_absolute contribution
#> 1      SBS1     Rep_1                0.0000  0.000000000
#> 2      SBS2     Rep_1              271.9152  0.007265021
#> 3      SBS3     Rep_1                0.0000  0.000000000
#> 4      SBS4     Rep_1                0.0000  0.000000000
#> 5      SBS5     Rep_1                0.0000  0.000000000
#> 6      SBS6     Rep_1                0.0000  0.000000000
```

## Other Utility Functions

sigverse dependencies are kept minimal. To mitigate the cost to
readability, sigshared includes some baseR implementations of common
data.frame manipulation functions and other utilities.

``` r
# Setup data
mtcars <- mtcars[1:5,1:5]

# Rename a dataframe
brename(mtcars, c(miles_per_gallon = "mpg"))
#>                   miles_per_gallon cyl disp  hp drat
#> Mazda RX4                     21.0   6  160 110 3.90
#> Mazda RX4 Wag                 21.0   6  160 110 3.90
#> Datsun 710                    22.8   4  108  93 3.85
#> Hornet 4 Drive                21.4   6  258 110 3.08
#> Hornet Sportabout             18.7   8  360 175 3.15

# Select a subset of columns
bselect(mtcars, c("mpg"))
#>                    mpg
#> Mazda RX4         21.0
#> Mazda RX4 Wag     21.0
#> Datsun 710        22.8
#> Hornet 4 Drive    21.4
#> Hornet Sportabout 18.7

# Evaluate code with a specific random seed
with_seed(seed = 123, { runif(1) })
#> [1] 0.2875775
```
