# Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018 Trustees of Columbia University
# Copyright (C) 2018, 2019 Aki Vehtari, Paul Bürkner
# See LICENSE.md for more details

# internal ----------------------------------------------------------------
#' To test is things are constant
#' 
#' @noRd
is_constant <- function(x, tol = .Machine$double.eps) {
  abs(max(x) - min(x)) < tol
}

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
  transform <- stats::fft(yc)
  ac <- stats::fft(Conj(transform) * transform, inverse = TRUE)
  # use "biased" estimate as recommended by Geyer (1992)
  ac <- Re(ac)[1:N] / (N^2 * 2)
  ac
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
  if (is_constant(x)) {
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