# Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018 Trustees of Columbia University
# Copyright (C) 2018, 2019 Aki Vehtari, Paul Bürkner
# See LICENSE.md for more details

#' Basic version of the Rhat convergence diagnostic
#'
#' Compute the basic Rhat convergence diagnostic for a single variable as
#' described in Gelman et al. (2013). For practical applications, we strongly
#' recommend the improved Rhat convergence diagnostic implemented in
#' [rhat()].
#'
#'
#' @noRd
rhat_basic <- function(x, split = TRUE) {
  split <- as_one_logical(split)
  if (split) {
    x <- split_chains(x)
  }
  .rhat(x)
}

#' Basic version of the effective sample size
#'
#' Compute the basic effective sample size (ESS) estimate for a single variable
#' as described in Gelman et al. (2013). For practical applications, we strongly
#' recommend the improved ESS convergence diagnostics implemented in
#' [ess_bulk()] and [ess_tail()].
#'
#'
#' @noRd
ess_basic <- function(x, split = TRUE) {
  split <- posterior:::as_one_logical(split)
  if (split) {
    x <- split_chains(x)
  }
  .ess(x)
}

#' Rhat convergence diagnostic
#'
#' Compute Rhat convergence diagnostic as the maximum of rank normalized
#' split-Rhat and rank normalized folded-split-Rhat for a single variable
#' as proposed in Vehtari et al. (2019).
#'
#'
#' @noRd
rhat <- function(x) {
  rhat_bulk <- .rhat(z_scale(split_chains(x)))
  rhat_tail <- .rhat(z_scale(split_chains(fold_draws(x))))
  max(rhat_bulk, rhat_tail)
}

#' Bulk effective sample size (bulk-ESS)
#'
#' Compute bulk effective sample size estimate (bulk-ESS) for a single variable.
#' Bulk-ESS is useful as a generic diagnostic for the sampling
#' efficiency in the bulk of the posterior. It is defined as the
#' effective sample size for rank normalized values using split chains.
#'
#'
#' @noRd
ess_bulk <- function(x) {
  .ess(z_scale(split_chains(x)))
}

#' Tail effective sample size (tail-ESS)
#'
#' Compute tail effective sample size estimate (tail-ESS) for a single variable.
#' Tail-ESS is useful for generic diagnostic for the sampling
#' efficiency in the tails of the posterior. It is defined as
#' the minimum of the effective sample sizes for 5% and 95% quantiles.
#'
#'
#' @noRd
ess_tail <- function(x) {
  q05_ess <- ess_quantile(x, 0.05)
  q95_ess <- ess_quantile(x, 0.95)
  min(q05_ess, q95_ess)
}

#' Effective sample sizes for quantiles
#'
#' Compute effective sample size estimates for quantile estimates of
#' a single variable.
#'
#'
#' @noRd
ess_quantile <- function(x, probs = c(0.05, 0.95), names = TRUE) {
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop2("'probs' must contain values between 0 and 1.")
  }
  names <- as_one_logical(names)
  out <- ulapply(probs, .ess_quantile, x = x)
  if (names) {
    names(out) <- paste0("ess_q", probs * 100)
  }
  out
}

#' @rdname ess_quantile
#' @noRd
ess_median <- function(x) {
  .ess_quantile(x, prob = 0.5)
}

# ESS of a single quantile
.ess_quantile <- function(x, prob) {
  x <- as.matrix(x)
  I <- x <= quantile(x, prob)
  .ess(split_chains(I))
}

#' Effective sample size for the mean
#'
#' Compute effective sample size estimate for a mean (expectation)
#' estimate of a single variable.
#' @noRd
ess_mean <- function(x) {
  .ess(split_chains(x))
}

#' Effective sample size for the standard deviation
#'
#' Compute an effective sample size estimate for the standard deviation (SD)
#' estimate of a single variable. This is defined as minimum of effective
#' sample size estimate for mean and mean of squared value.
#'
#'
#' @noRd
ess_sd <- function(x) {
  min(.ess(split_chains(x)), .ess(split_chains(x^2)))
}

#' Monte Carlo standard error for quantiles
#'
#' Compute Monte Carlo standard errors for quantile estimates of a
#' single variable.
#'
#' @noRd
mcse_quantile <- function(x, probs = c(0.05, 0.95), names = TRUE) {
  probs <- as.numeric(probs)
  if (any(probs < 0 | probs > 1)) {
    stop2("'probs' must contain values between 0 and 1.")
  }
  names <- as_one_logical(names)
  out <- ulapply(probs, .mcse_quantile, x = x)
  if (names) {
    names(out) <- paste0("mcse_q", probs * 100)
  }
  out
}

#' @rdname mcse_quantile
#' @noRd
mcse_median <- function(x) {
  .mcse_quantile(x, prob = 0.5)
}

# MCSE of a single quantile
.mcse_quantile <- function(x, prob) {
  ess <- ess_quantile(x, prob)
  p <- c(0.1586553, 0.8413447)
  a <- qbeta(p, ess * prob + 1, ess * (1 - prob) + 1)
  ssims <- sort(x)
  S <- length(ssims)
  th1 <- ssims[max(floor(a[1] * S), 1)]
  th2 <- ssims[min(ceiling(a[2] * S), S)]
  as.vector((th2 - th1) / 2)
}

#' Monte Carlo standard error for the mean
#'
#' Compute Monte Carlo standard error for mean (expectation) of a
#' single variable.
#'
#'
#' @noRd
mcse_mean <- function(x) {
  sd(x) / sqrt(ess_mean(x))
}

#' Monte Carlo standard error for the standard deviation
#'
#' Compute Monte Carlo standard error for standard deviation (SD) of a
#' single variable using Stirling's approximation and assuming
#' approximate normality.
#'
#' @noRd
mcse_sd <- function(x) {
  # assumes normality of x and uses Stirling's approximation
  ess_sd <- ess_sd(x)
  sd(x) * sqrt(exp(1) * (1 - 1 / ess_sd)^(ess_sd - 1) - 1)
}

#' Compute Quantiles
#'
#' Compute quantiles of a sample and return them in a format consistent
#' with other summary functions of the \pkg{posterior} package.
#'
#'
#' @noRd
quantile2 <- function(x, probs = c(0.05, 0.95), names = TRUE, ...) {
  names <- as_one_logical(names)
  out <- quantile(x, probs = probs, ...)
  if (names) {
    names(out) <- paste0("q", probs * 100)
  } else {
    names(out) <- NULL
  }
  out
}

# internal ----------------------------------------------------------------

#' Find the optimal next size for the FFT so that a minimum number of zeros
#' are padded.
#' @param N length of the sequence over which to apply FFT
#' @return the optimal next step size as a single integer
#' @noRd
fft_next_good_size <- function(N) {
  if (N <= 2)
    return(2)
  while (TRUE) {
    m <- N
    while ((m %% 2) == 0) m <- m / 2
    while ((m %% 3) == 0) m <- m / 3
    while ((m %% 5) == 0) m <- m / 5
    if (m <= 1)
      return(N)
    N <- N + 1
  }
}

#' Autocovariance estimates
#'
#' Compute autocovariance estimates for every lag for the specified
#' input sequence using a fast Fourier transform approach. The estimate
#' for lag t is scaled by N-t where N is the length of the sequence.
#'
#' @return A numeric vector of autocovariances at every lag (scaled by N-lag).
#' @keywords internal
#' @noRd
autocovariance <- function(x) {
  N <- length(x)
  M <- fft_next_good_size(N)
  Mt2 <- 2 * M
  yc <- x - mean(x)
  yc <- c(yc, rep.int(0, Mt2 - N))
  transform <- fft(yc)
  ac <- fft(Conj(transform) * transform, inverse = TRUE)
  # use "biased" estimate as recommended by Geyer (1992)
  ac <- Re(ac)[1:N] / (N^2 * 2)
  ac
}

#' Autocorrelation estimates
#'
#' Compute autocorrelation estimates for every lag for the specified
#' input sequence using a fast Fourier transform approach. The estimate
#' for lag t is scaled by N-t where N is the length of the sequence.
#'
#' @return A numeric vector of autocorrelations at every lag (scaled by N-lag).
#' @keywords internal
#' @noRd
autocorrelation <- function(x) {
  ac <- autocovariance(x)
  ac <- ac / ac[1]
}

#' Rank normalization
#'
#' Compute rank normalization for a numeric array. First replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities. Second, normalize
#' ranks via the inverse normal transformation.
#'
#' @return A numeric array of rank normalized values with the same size
#'   and dimension as the input.
#' @keywords internal
#' @noRd
z_scale <- function(x) {
  r <- rank(as.array(x), ties.method = 'average')
  z <- qnorm(backtransform_ranks(r))
  z[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    z <- array(z, dim = dim(x), dimnames = dimnames(x))
  }
  z
}

#' Rank uniformization
#'
#' Compute rank uniformization for a numeric array. First replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities. Second, uniformize
#' ranks to scale `[1/(2S), 1-1/(2S)]`, where `S` is the the number
#' of values.
#'
#' @return A numeric array of uniformized values with the same size
#'   and dimension as the input.
#' @keywords internal
#' @noRd
u_scale <- function(x) {
  r <- rank(as.array(x), ties.method = 'average')
  u <- backtransform_ranks(r)
  u[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    u <- array(u, dim = dim(x), dimnames = dimnames(x))
  }
  u
}

#' Rank values
#'
#' Compute ranks for a numeric array. First replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities. Second, normalize
#' ranks via the inverse normal transformation.
#'
#' @return A numeric array of ranked values with the same size
#'   and dimension as the input.
#' @keywords internal
#' @noRd
r_scale <- function(x) {
  r <- rank(as.array(x), ties.method = 'average')
  r[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    # output should have the input dimension
    r <- array(r, dim = dim(x), dimnames = dimnames(x))
  }
  r
}

#' Backtransformation of ranks
#'
#' @param r array of ranks
#' @param c fractional offset; defaults to c = 3/8 as recommend by Bloom (1985)
#' @noRd
backtransform_ranks <- function(r, c = 3/8) {
  S <- length(r)
  (r - c) / (S - 2 * c + 1)
}

#' Split Markov chains in half
#' @return A 2D array of draws with split chains.
#' @keywords internal
#' @noRd
split_chains <- function(x) {
  x <- as.matrix(x)
  niter <- NROW(x)
  if (niter == 1L) {
    return(x)
  }
  half <- niter / 2
  cbind(x[1:floor(half), ], x[ceiling(half + 1):niter, ])
}

#' Fold draws around their median
#' @return An array or vector of folded draws.
#' @keywords internal
#' @noRd
fold_draws <- function(x) {
  abs(x - median(x))
}

#' Compute the Rhat convergence diagnostic
#' @keywords internal
#' @noRd
.rhat <- function(x) {
  x <- as.matrix(x)
  if (anyNA(x)) {
    return(NA)
  }
  if (any(!is.finite(x))) {
    return(NaN)
  }
  if (posterior:::is_constant(x)) {
    return(NA)
  }
  nchains <- NCOL(x)
  niterations <- NROW(x)
  chain_mean <- numeric(nchains)
  chain_var <- numeric(nchains)
  for (i in seq_len(nchains)) {
    chain_mean[i] <- mean(x[, i])
    chain_var[i] <- var(x[, i])
  }
  var_between <- niterations * var(chain_mean)
  var_within <- mean(chain_var)
  sqrt((var_between / var_within + niterations - 1) / niterations)
}



#' Compute Geyer's initial positive sequence length.
#' From posterior package:
#' https://github.com/jgabry/posterior/blob/4ac3054d029f5ddb6fadf1ef972344d195f530de/R/convergence.R
#' @keywords internal
#' @noRd


.max_t <- function(x) {
  x <- as.matrix(x)
  nchains <- NCOL(x)
  niterations <- NROW(x)
  if (niterations < 3L || anyNA(x)) {
    return(NA)
  }
  if (any(!is.finite(x))) {
    return(NaN)
  }
  if (posterior:::is_constant(x)) {
    return(NA)
  }
  acov_fun <- function(i) autocovariance(x[, i])
  acov <- lapply(seq_len(nchains), acov_fun)
  acov <- do.call(cbind, acov)
  chain_mean <- apply(x, 2, mean)
  mean_var <- mean(acov[1, ]) * niterations / (niterations - 1)
  var_plus <- mean_var * (niterations - 1) / niterations
  if (nchains > 1) {
    var_plus <- var_plus + var(chain_mean)
  }
  
  # Geyer's initial positive sequence
  rho_hat_t <- rep.int(0, niterations)
  t <- 0
  rho_hat_even <- 1
  rho_hat_t[t + 1] <- rho_hat_even
  rho_hat_odd <- 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
  rho_hat_t[t + 2] <- rho_hat_odd
  while (t < NROW(acov) - 5 && !is.nan(rho_hat_even + rho_hat_odd) &&
         (rho_hat_even + rho_hat_odd > 0)) {
    t <- t + 2
    rho_hat_even = 1 - (mean_var - mean(acov[t + 1, ])) / var_plus
    rho_hat_odd = 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
    if ((rho_hat_even + rho_hat_odd) >= 0) {
      rho_hat_t[t + 1] <- rho_hat_even
      rho_hat_t[t + 2] <- rho_hat_odd
    }
  }
  max_t <- t
  return(max_t)
}


#' Compute Geyer's initial positive sequence length for shinystan objects.
#' From posterior package:
#' https://github.com/jgabry/posterior/blob/4ac3054d029f5ddb6fadf1ef972344d195f530de/R/convergence.R
#' @keywords internal
#' @noRd


.max_t_sso <- function(sso, param, chains = 0) {
  
  max_t <- NULL
  for(i in 1:length(param)) {
    if(chains == 0){
      max_t[i] <- .max_t(sso@posterior_sample[, , param[i]])
    } else {
      max_t[i] <- .max_t(sso@posterior_sample[, chains, param[i]])
    }
    
  }
  return(max(max_t))
}