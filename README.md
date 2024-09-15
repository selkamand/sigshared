
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

<table style="width:94%;">
<colgroup>
<col style="width: 47%" />
<col style="width: 47%" />
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

## Other Utility Functions

sigverse dependencies are kept minimal. To mitigate the cost to
readability, sigshared includes some baseR implementations of common
data.frame manipulation functions and other utilities.

``` r
# Rename a dataframe
rename(mtcars, c("miles_per_gallon" = "mpg"))
#>                     miles_per_gallon cyl  disp  hp drat    wt  qsec vs am gear
#> Mazda RX4                       21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag                   21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> Datsun 710                      22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> Hornet 4 Drive                  21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> Hornet Sportabout               18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> Valiant                         18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> Duster 360                      14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> Merc 240D                       24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> Merc 230                        22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> Merc 280                        19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> Merc 280C                       17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> Merc 450SE                      16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> Merc 450SL                      17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> Merc 450SLC                     15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> Cadillac Fleetwood              10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> Lincoln Continental             10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> Chrysler Imperial               14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Fiat 128                        32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Honda Civic                     30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> Toyota Corolla                  33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#> Toyota Corona                   21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Dodge Challenger                15.5   8 318.0 150 2.76 3.520 16.87  0  0    3
#> AMC Javelin                     15.2   8 304.0 150 3.15 3.435 17.30  0  0    3
#> Camaro Z28                      13.3   8 350.0 245 3.73 3.840 15.41  0  0    3
#> Pontiac Firebird                19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> Fiat X1-9                       27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Porsche 914-2                   26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> Lotus Europa                    30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Ford Pantera L                  15.8   8 351.0 264 4.22 3.170 14.50  0  1    5
#> Ferrari Dino                    19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> Maserati Bora                   15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> Volvo 142E                      21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#>                     carb
#> Mazda RX4              4
#> Mazda RX4 Wag          4
#> Datsun 710             1
#> Hornet 4 Drive         1
#> Hornet Sportabout      2
#> Valiant                1
#> Duster 360             4
#> Merc 240D              2
#> Merc 230               2
#> Merc 280               4
#> Merc 280C              4
#> Merc 450SE             3
#> Merc 450SL             3
#> Merc 450SLC            3
#> Cadillac Fleetwood     4
#> Lincoln Continental    4
#> Chrysler Imperial      4
#> Fiat 128               1
#> Honda Civic            2
#> Toyota Corolla         1
#> Toyota Corona          1
#> Dodge Challenger       2
#> AMC Javelin            2
#> Camaro Z28             4
#> Pontiac Firebird       2
#> Fiat X1-9              1
#> Porsche 914-2          2
#> Lotus Europa           2
#> Ford Pantera L         4
#> Ferrari Dino           6
#> Maserati Bora          8
#> Volvo 142E             2
```
