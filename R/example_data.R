
# Signatures --------------------------------------------------------------


example_valid_signature <- function(){
 data.frame(
   channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
   type = c('A>G', 'A>G', 'A>G'),
   fraction = c(0.4, 0.1, 0.2)
   )
}


example_invalid_signature_channeldup <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.5)
  )
}

example_invalid_signature_numeric_channel = function(){
  data.frame(
    channel = c(1:4),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.5)
  )
}

example_invalid_signature_factor_channel = function(){
  data.frame(
    channel = factor(c('A[A->G]G', 'A[A->G]G', 'A[A->G]C', 'A[A->G]T')),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.5)
  )
}

example_invalid_signature_missing = function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G', NA),
    fraction = c(0.4, 0.1, 0.2, 0.5)
  )
}


example_valid_signature_collection <- function(){
  list(
    sig1 = example_valid_signature(),
    sig2 = example_valid_signature()
    )
}
# Decompositions ----------------------------------------------------------



example_valid_decomposition <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.18518519, 0.37037037, 0.44444444)
  )
}

example_invalid_decomposition_nonsensical_fraction <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.2, 0.6, 0.2)
  )
}

example_invalid_decomposition_missing <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, NA),
    fraction = c(0.18518519, 0.37037037, 0.44444444)
  )
}

example_invalid_decomposition_colname_typo <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    typeo = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.18518519, 0.37037037, 0.44444444)
  )
}

example_invalid_decomposition_channeldup <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12, 10),
    fraction = c(0.18518519, 0.37037037, 0.44444444)
  )
}

example_valid_decomposition_collection <- function(){
 list(
   'decomp1' = example_valid_decomposition(),
   'decomp2' = example_valid_decomposition(),
   'decomp3' = example_valid_decomposition()
   )
}


# Cohorts -----------------------------------------------------------------

example_valid_cohort_analysis <- function(){
  data.frame(
    "sample" = c('sample1', 'sample1', 'sample2', 'sample2'),
    "signature" = c('sig1', 'sig2', 'sig1', 'sig2'),
    "contribution_absolute" = c(3, 7, 40, 60),
    "contribution" = c(0.3, 0.7, 0.4, 0.6),
    "bootstraps" = c('0.3; 0.27; 0.32', '0.7;0.72;0.68;', '0.4;0.45;0.35', '0.6;0.62;0.55')
    )
}
