
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
```

``` r
example_bootstraps()
#>   bootstrap  signature contribution_absolute contribution
#> 1         1 Signature1                   300         0.30
#> 2         1 Signature2                   690         0.69
#> 3         1 Signature3                    10         0.01
#> 4         2 Signature1                   440         0.44
#> 5         2 Signature2                   500         0.50
#> 6         2 Signature3                    60         0.06
```

``` r

example_catalogue()
#>    channel type count  fraction
#> 1 A[A->G]G  A>G     5 0.1851852
#> 2 A[A->G]C  A>G    10 0.3703704
#> 3 A[A->G]T  A>G    12 0.4444444
```

``` r
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
```

``` r

example_signature()
#>    channel type fraction
#> 1 A[A->G]G  A>G      0.4
#> 2 A[A->G]C  A>G      0.1
#> 3 A[A->G]T  A>G      0.5
```

``` r
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
```

``` r

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
example_catalogue_colo829()
#>    channel type     fraction count
#> 1  A[C>A]A  C>A 0.0035802073   134
#> 2  A[C>A]C  C>A 0.0016565138    62
#> 3  A[C>A]G  C>A 0.0004542054    17
#> 4  A[C>A]T  C>A 0.0015229240    57
#> 5  A[C>G]A  C>G 0.0016565138    62
#> 6  A[C>G]C  C>G 0.0011488725    43
#> 7  A[C>G]G  C>G 0.0005076413    19
#> 8  A[C>G]T  C>G 0.0014427701    54
#> 9  A[C>T]A  C>T 0.0032061558   120
#> 10 A[C>T]C  C>T 0.0202254996   757
#> 11 A[C>T]G  C>T 0.0018702576    70
#> 12 A[C>T]T  C>T 0.0083894411   314
#> 13 A[T>A]A  T>A 0.0032595917   122
#> 14 A[T>A]C  T>A 0.0015496420    58
#> 15 A[T>A]G  T>A 0.0020840013    78
#> 16 A[T>A]T  T>A 0.0090573902   339
#> 17 A[T>C]A  T>C 0.0036870792   138
#> 18 A[T>C]C  T>C 0.0012023084    45
#> 19 A[T>C]G  T>C 0.0023511809    88
#> 20 A[T>C]T  T>C 0.0046222080   173
#> 21 A[T>G]A  T>G 0.0016297959    61
#> 22 A[T>G]C  T>G 0.0012290264    46
#> 23 A[T>G]G  T>G 0.0017901037    67
#> 24 A[T>G]T  T>G 0.0033130277   124
#> 25 C[C>A]A  C>A 0.0508977236  1905
#> 26 C[C>A]C  C>A 0.0063054398   236
#> 27 C[C>A]G  C>A 0.0077214919   289
#> 28 C[C>A]T  C>A 0.0211071925   790
#> 29 C[C>G]A  C>G 0.0011221545    42
#> 30 C[C>G]C  C>G 0.0010152827    38
#> 31 C[C>G]G  C>G 0.0006679491    25
#> 32 C[C>G]T  C>G 0.0012824623    48
#> 33 C[C>T]A  C>T 0.0554130597  2074
#> 34 C[C>T]C  C>T 0.0678101956  2538
#> 35 C[C>T]G  C>T 0.0079352357   297
#> 36 C[C>T]T  C>T 0.0546916747  2047
#> 37 C[T>A]A  T>A 0.0011755905    44
#> 38 C[T>A]C  T>A 0.0018435396    69
#> 39 C[T>A]G  T>A 0.0009084108    34
#> 40 C[T>A]T  T>A 0.0034733355   130
#> 41 C[T>C]A  T>C 0.0018702576    70
#> 42 C[T>C]C  T>C 0.0028053863   105
#> 43 C[T>C]G  T>C 0.0020305654    76
#> 44 C[T>C]T  T>C 0.0093512878   350
#> 45 C[T>G]A  T>G 0.0006946671    26
#> 46 C[T>G]C  T>G 0.0015229240    57
#> 47 C[T>G]G  T>G 0.0016565138    62
#> 48 C[T>G]T  T>G 0.0131452389   492
#> 49 G[C>A]A  C>A 0.0019771294    74
#> 50 G[C>A]C  C>A 0.0008816929    33
#> 51 G[C>A]G  C>A 0.0002137437     8
#> 52 G[C>A]T  C>A 0.0016565138    62
#> 53 G[C>G]A  C>G 0.0003473335    13
#> 54 G[C>G]C  C>G 0.0008282569    31
#> 55 G[C>G]G  C>G 0.0002137437     8
#> 56 G[C>G]T  C>G 0.0007481030    28
#> 57 G[C>T]A  C>T 0.0022175911    83
#> 58 G[C>T]C  C>T 0.0117024687   438
#> 59 G[C>T]G  C>T 0.0019504115    73
#> 60 G[C>T]T  C>T 0.0117559047   440
#> 61 G[T>A]A  T>A 0.0009885647    37
#> 62 G[T>A]C  T>A 0.0004274874    16
#> 63 G[T>A]G  T>A 0.0012824623    48
#> 64 G[T>A]T  T>A 0.0016297959    61
#> 65 G[T>C]A  T>C 0.0018435396    69
#> 66 G[T>C]C  T>C 0.0014427701    54
#> 67 G[T>C]G  T>C 0.0012557444    47
#> 68 G[T>C]T  T>C 0.0107673400   403
#> 69 G[T>G]A  T>G 0.0007748210    29
#> 70 G[T>G]C  T>G 0.0006946671    26
#> 71 G[T>G]G  T>G 0.0020038474    75
#> 72 G[T>G]T  T>G 0.0044886181   168
#> 73 T[C>A]A  C>A 0.0046489259   174
#> 74 T[C>A]C  C>A 0.0039008229   146
#> 75 T[C>A]G  C>A 0.0006145132    23
#> 76 T[C>A]T  C>A 0.0059848242   224
#> 77 T[C>G]A  C>G 0.0024580528    92
#> 78 T[C>G]C  C>G 0.0014694881    55
#> 79 T[C>G]G  C>G 0.0004542054    17
#> 80 T[C>G]T  C>G 0.0045954900   172
#> 81 T[C>T]A  C>T 0.1244522817  4658
#> 82 T[C>T]C  C>T 0.2123276691  7947
#> 83 T[C>T]G  C>T 0.0471304905  1764
#> 84 T[C>T]T  C>T 0.0893181575  3343
#> 85 T[T>A]A  T>A 0.0034733355   130
#> 86 T[T>A]C  T>A 0.0016030779    60
#> 87 T[T>A]G  T>A 0.0011221545    42
#> 88 T[T>A]T  T>A 0.0072405686   271
#> 89 T[T>C]A  T>C 0.0069199530   259
#> 90 T[T>C]C  T>C 0.0025649247    96
#> 91 T[T>C]G  T>C 0.0026450786    99
#> 92 T[T>C]T  T>C 0.0052100032   195
#> 93 T[T>G]A  T>G 0.0019771294    74
#> 94 T[T>G]C  T>G 0.0014694881    55
#> 95 T[T>G]G  T>G 0.0013626162    51
#> 96 T[T>G]T  T>G 0.0070802608   265
```

``` r
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
# Rename a dataframe
rename(mtcars, c(miles_per_gallon = "mpg"))
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
