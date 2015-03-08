# t distribution with location and scale
.dt_loc_scale <- function(x, df, location, scale) {
  1/scale * dt((x - location)/scale, df)
}

# inverse gamma distribution
.dinversegamma <- function(x, shape, scale) {
  logout <- log(scale)*shape - lgamma(shape) - (1+shape)*log(x) - (scale/x)
  exp(logout)
}
# inverse-chi-sq distribution
.dinversechisq <- function(x, df, scale) {
  df2 <- df / 2
  logout <- df2 * log(df2) - log(gamma(df2)) + df2 * log(scale) - (1 + df2) * log(x) - df2 * scale/x
  out <- exp(logout)
  out[which(x <= 0)] <- 0
  out
}
