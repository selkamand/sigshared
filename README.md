
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sigshared

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/sigshared)](https://CRAN.R-project.org/package=sigshared)

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

<table>
<colgroup>
<col style="width: 37%" />
<col style="width: 62%" />
</colgroup>
<thead>
<tr class="header">
<th>Data Structure</th>
<th>Requirements</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><strong>Signature</strong></td>
<td><p>data.frames with 3 columns</p>
<ol type="1">
<li><strong>channel</strong></li>
<li><strong>type</strong></li>
<li><strong>fraction</strong></li>
</ol></td>
</tr>
<tr class="even">
<td><strong>Signature Collections</strong></td>
<td>Lists of signature data.frames, where name of list entry is the name
of the signature</td>
</tr>
<tr class="odd">
<td><strong>Signature Annotations</strong></td>
<td><p>Signature level annotations. data.frames with 2 required
columns:</p>
<ol type="1">
<li><strong>signature</strong></li>
<li><strong>aetiology</strong></li>
</ol></td>
</tr>
<tr class="even">
<td><strong>Decompositions</strong></td>
<td><p>data.frames with 4 columns</p>
<ol type="1">
<li><strong>channel</strong></li>
<li><strong>type</strong></li>
<li><strong>fraction</strong></li>
<li><strong>count</strong></li>
</ol></td>
</tr>
<tr class="odd">
<td><strong>Decomposition Collections</strong></td>
<td>Lists of decomposition data.frames (1 per sample) where name
represents a sample identifier.</td>
</tr>
<tr class="even">
<td><strong>Cohort Signature Analysis Results</strong></td>
<td><p>data.frame with 4 columns:</p>
<ol type="1">
<li><p><strong>sample</strong></p></li>
<li><p><strong>signature</strong></p></li>
<li><p><strong>contribution_absolute</strong></p></li>
<li><p><strong>contribution
(</strong>percentage<strong>)</strong></p></li>
<li><p><strong>bootstraps</strong> (colon separated string)</p></li>
</ol></td>
</tr>
</tbody>
</table>

You can assert an object belongs to each of data types

``` r
library(sigshared)

# Generate Example Datatypes

# Signatures
signature = example_valid_signature()
signature_collection = example_valid_signature_collection()
signature_annotations = example_valid_signature_annotations()

# Decompositions
decomposition = example_valid_decomposition()
decomposition_collection = example_valid_decomposition_collection()

# Cohort Analysis Results
cohort_analysis = example_valid_cohort_analysis()


# Run Assertions
assert_signature(signature)
assert_signature_collection(signature_collection)
assert_decomposition(decomposition)
assert_decomposition_collection(decomposition_collection)
assert_cohort_analysis(cohort_analysis)
assert_signature_aetiology_collection(decomposition_collection)
```
