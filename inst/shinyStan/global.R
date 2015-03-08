# t distribution with location and scale
.dt_loc_scale <- function(x, df, location, scale) {
  1/scale * dt((x - location)/scale, df)
}
# inverse gamma distribution
.dinversegamma <- function(x, shape, scale) {
  logout <- log(scale)*shape - lgamma(shape) - (1+shape)*log(x) - (scale/x)
  exp(logout)
}

