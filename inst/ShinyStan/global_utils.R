# give ShinyStan app access to ggplot functions
load("ggplot_fns.rda")
lapply(ggplot_fns, function(f) {
  try(assign(f, getFromNamespace(f, "ggplot2"), envir = parent.frame(2)), silent = TRUE)
})

# load helper_functions
helpers <- file.path("helper_functions", list.files("helper_functions", full.names = FALSE))
for (h in helpers) 
  source(h, local = TRUE)

# avoid conflict with inline::code if rstan is loaded
code <- shiny::code


# make_param_list ------------------------------------------------------
# generate list of parameter names (formatted for shiny::selectInput)
.make_param_list <- function(object) {
  param_names <- slot(object, "param_names")
  param_dims <- slot(object, "param_dims")
  param_groups <- names(param_dims)
  choices <- list()
  ll <- length(param_dims)
  choices[seq_len(ll)] <- ""
  names(choices) <- param_groups
  for(i in seq_len(ll)) {
    if (length(param_dims[[i]]) == 0) {
      choices[[i]] <- list(param_groups[i])
    }
    else {
      temp <- paste0(param_groups[i],"\\[")
      choices[[i]] <- param_names[grep(temp, param_names)]
    }
  }
  choices
}

# make_param_list_with_groups ------------------------------------------------------
# generate list of parameter names and include parameter groups (formatted for
# shiny::selectInput)
.make_param_list_with_groups <- function(object, sort_j = FALSE) {
  param_names <- slot(object, "param_names")
  param_dims <- slot(object, "param_dims")
  param_groups <- names(param_dims)
  ll <- length(param_dims)
  LL <- sapply(seq_len(ll), function(i) length(param_dims[[i]]))
  choices <- list()
  choices[seq_len(ll)] <- ""
  names(choices) <- param_groups
  for(i in seq_len(ll)) {
    if (LL[i] == 0) {
      choices[[i]] <- list(param_groups[i])
    } else {
      group <- param_groups[i]
      temp <- paste0("^",group,"\\[")
      ch <- param_names[grep(temp, param_names)]

      #       toggle row/column major sorting so e.g. "beta[1,1], beta[1,2],
      #       beta[2,1], beta[2,2]" instead of "beta[1,1], beta[2,1], beta[1,2],
      #       beta[2,2]"
      if (sort_j == TRUE & LL[i] > 1)
        ch <- gtools::mixedsort(ch)

      ch_out <- c(paste0(group,"_as_shinystan_group"), ch)
      names(ch_out) <- c(paste("ALL", group), ch)
      choices[[i]] <- ch_out
    }
  }

  choices
}

# update parameter selection for multi-parameter plots --------------------
# update with regex
.test_valid_regex <- function(pattern) {
  trygrep <- try(grep(pattern, ""), silent = TRUE)
  if (inherits(trygrep, "try-error"))
    FALSE
  else
    TRUE
}
.update_params_with_regex <- function(params, all_param_names, regex_pattern) {
  sel <- which(all_param_names %in% params)
  to_search <- if (length(sel)) 
    all_param_names[-sel] else all_param_names
  if (!length(regex_pattern)) 
    return(params)
  
  to_add <- grep(regex_pattern, to_search, value = TRUE)
  if (!length(to_add)) 
    params 
  else 
    c(params, to_add)
}

# update with groups
.update_params_with_groups <- function(params, all_param_names) {
  as_group <- grep("_as_shinystan_group", params)
  if (!length(as_group)) 
    return(params)
  make_group <- function(group_name) {
    all_param_names[grep(paste0("^",group_name,"\\["), all_param_names)]
  }
  single_params <- params[-as_group]
  grouped_params <- params[as_group]
  groups <- gsub("_as_shinystan_group", "", grouped_params)
  groups <- sapply(groups, make_group)
  c(single_params, unlist(groups))
}


# generate color vectors --------------------------------------------------
color_vector <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h=hues, l=50, c=50)[1:n]
}
color_vector_chain <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h=hues, l=80, c=50)[1:n]
}

# transformations ---------------------------------------------------------
transformation_choices <-
  c(
    "abs", "atanh",
    cauchit = "pcauchy", "cloglog",
    "exp", "expm1",
    "identity", "inverse", inv_logit = "plogis",
    "log", "log10", "log2", "log1p", logit = "qlogis",
    probit = "pnorm",
    "square", "sqrt"
  )

inverse <- function(x) 1/x
cloglog <- function(x) log(-log1p(-x))
square <- function(x) x^2

# extra distributions for density comparisons -----------------------------
# t distribution with location and scale
.dt_loc_scale <- function(x, df, location, scale) {
  1/scale * dt((x - location)/scale, df)
}
# inverse gamma distribution
.dinversegamma <- function(x, shape, scale) {
  logout <- log(scale)*shape - lgamma(shape) - (1+shape)*log(x) - (scale/x)
  exp(logout)
}


# bold/strong text generators ---------------------------------------------
# used both in ui files and server files that call renderUI
strongMed <- function(...) 
  strong(style = "font-size: 14px; margin-bottom: 5px;", ...)
strongBig <- function(...) 
  strong(style = "font-size: 18px; margin-bottom: 5px;", ...)
strong_bl <- function(...) 
  strong(style = "color: #006DCC;", ...)
