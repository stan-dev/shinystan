# shinystan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinystan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.


#' Add new quantity to shinystan object
#' 
#' Add to shinystan object a new parameter as a function of one or two existing
#' parameters.
#' 
#' @export
#' @template args-sso
#' @param fun Function to call, i.e. \code{function(param1)} or 
#'   \code{function(param1,param2)}. See Examples, below.
#' @param param1 Name of first parameter as character string.
#' @param param2 Optional. Name of second parameter as character string.
#' @param new_name Name for the new parameter as character string.
#'   
#' @return sso, updated. See Examples.
#' 
#' @template seealso-drop_parameters
#'
#' @examples
#' # Using example shinystan object 'eight_schools'
#' sso <- eight_schools
#' sso <- generate_quantity(sso, fun = function(x) x^2, 
#'                          param1 = "tau", new_name = "tau_sq")
#' sso <- generate_quantity(sso, fun = "-", 
#'                          param1 = "theta[1]", param2 = "theta[2]", 
#'                          new_name = "theta1minus2")
#'                          
generate_quantity <- function(sso, param1, param2, fun, new_name) {
  sso_check(sso)
  if (isTRUE(new_name %in% slot(sso, "param_names")))
    stop(paste("There is already a parameter named", new_name))
  
  message("\nThis might take a moment for large shinystan objects...")
  
  two_params <- !missing(param2)
  posterior <- slot(sso, "posterior_sample")
  dims <- dim(posterior)
  ndim <- length(dims)
  if (ndim == 3) {
    # i.e. multiple chains
    x_samp <- posterior[, , param1]
    if (two_params)
      y_samp <- posterior[, , param2]
  }
  
  arglist <- if (two_params)
    list(x_samp, y_samp) else list(x_samp)
  temp <- do.call(fun, args = arglist)
  
  new_dim <- dims
  new_dim[[ndim]] <- new_dim[[ndim]] + 1
  new_dim_names <- dimnames(posterior)
  new_dim_names[[ndim]] <- c(new_dim_names[[ndim]], new_name)
  posterior <-
    array(data = c(posterior, temp),
          dim = new_dim,
          dimnames = new_dim_names)
  
  param_dims_new <- slot(sso, "param_dims")
  param_dims_new[[new_name]] <- numeric(0)
  sso_new <- as.shinystan(
    posterior,
    model_name = slot(sso, "model_name"),
    warmup = slot(sso, "n_warmup"),
    param_dims = param_dims_new
  )
  slot(sso_new, "summary") <-
    shinystan_monitor(posterior, warmup = slot(sso, "n_warmup"))
  
  slot_names <- c("sampler_params", "model_code", "user_model_info", "misc")
  for (sn in slot_names)
    slot(sso_new, sn) <- slot(sso, sn)
  
  sso_new
}
