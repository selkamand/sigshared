
# Signatures --------------------------------------------------------------


example_valid_sig <- function(){
 data.frame(
   channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
   type = c('A>G', 'A>G', 'A>G'),
   fraction = c(0.4, 0.1, 0.2)
   )
}


example_invalid_sig_channeldup <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.5)
  )
}

example_invalid_sig_numeric_channel = function(){
  data.frame(
    channel = c(1:4),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.5)
  )
}

example_invalid_sig_factor_channel = function(){
  data.frame(
    channel = factor(c('A[A->G]G', 'A[A->G]G', 'A[A->G]C', 'A[A->G]T')),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    fraction = c(0.4, 0.1, 0.2, 0.5)
  )
}

example_invalid_sig_missing = function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G', NA),
    fraction = c(0.4, 0.1, 0.2, 0.5)
  )
}


example_valid_sig_collection <- function(){
  list(
    sig1 = example_valid_sig(),
    sig2 = example_valid_sig()
    )
}
# Decompositions ----------------------------------------------------------



example_valid_decomp <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.1852, 0.3704, 0.4444)
  )
}

example_invalid_decomp_nonsensical_fraction <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.2, 0.6, 0.2)
  )
}

example_invalid_decomp_missing <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, NA),
    fraction = c(0.1852, 0.3704, 0.4444)
  )
}

example_invalid_decomp_colname_typo <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T'),
    typeo = c('A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12),
    fraction = c(0.1852, 0.3704, 0.4444)
  )
}

example_invalid_decomp_channeldup <- function(){
  data.frame(
    channel = c('A[A->G]G', 'A[A->G]C', 'A[A->G]T', 'A[A->G]T'),
    type = c('A>G', 'A>G', 'A>G', 'A>G'),
    count = c(5, 10, 12, 10),
    fraction = c(0.1351, 0.2703, 0.3243, 0.2703)
  )
}



