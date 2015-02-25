#' Add model code to a \code{shinystan} object
#'
#' @param sso A \code{shinystan} object.
#' @param code The code you want to add. See \strong{Details} below for
#' formatting instructions.
#' @return The \code{shinystan} object \code{sso} with \code{code} in the
#' \code{model_code} slot.
#'
#' @details \code{code} should be a character string that can be used as an argument
#' to \code{cat}. See \strong{Examples}, below.
#' @note This is only useful for users who did not run their models using \pkg{rstan}.
#' For \pkg{rstan} users the model code will be automatically available.
#' @seealso \code{cat}
#' @export
#'
#' @examples
#' \dontrun{
#' # Some JAGS-style code we might want to add
#' my_code <- "
#'  model {
#'    for (i in 1:length(Y)) {
#'      Y[i] ~ dpois(lambda[i])
#'      log(lambda[i]) <- inprod(X[i,], theta[])
#'    }
#'    for (j in 1:J) {
#'      theta[j] ~ dt(0.0, 1.0, 1.0)
#'    }
#'  }
#' "
#'
#' # Add the code to a shinystan object X
#' X <- include_model_code(X, my_code)
#' launch_shinystan(X) # code is visible in the Model Code tab
#'
#'}

include_model_code <- function(sso, code) {
  sso_name <- deparse(substitute(sso))
  if (!is.shinystan(sso)) {
    stop(paste(sso_name, "should be a shinystan object."))
  }
  sso@model_code <- code
  sso
}
