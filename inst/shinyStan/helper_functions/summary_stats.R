
# param_summary -----------------------------------------------------------
# summary stats for a single parameter
.param_summary <- function(param, summary) {
  out <- summary[param, c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")]
  out["n_eff"] <- round(out["n_eff"])
  outmat <- matrix(out, 1, length(out))
  colnames(outmat) <- names(out)
  rownames(outmat) <- NULL
  outmat
}


# all_summary -------------------------------------------------------------
# summary stats for all parameters
.all_summary <- function(summary, digits = 2, cols) {
  df <- as.data.frame(summary[, cols])
  df <- round(df, digits)
  if ("n_eff" %in% cols) df[, "n_eff"] <- round(df[, "n_eff"])
  df
}

# tex_summary -------------------------------------------------------------
# prep for latex table
.tex_summary <- function(summary, params, cols) {
  df <- as.data.frame(summary[, cols])
  if ("n_eff" %in% cols) df[, "n_eff"] <- round(df[, "n_eff"])
  out <- cbind(Parameter = rownames(df), df)
  out
}

# sampler_summary ---------------------------------------------------------
.sampler_stuff <- function(X, param, inc_warmup, report) {
  sapply_funs <- function(x, fun_name) {
    funs <- list(maxf = function(x) max(x[,param]), minf = function(x) min(x[,param]),
                 meanf = function(x) mean(x[,param]), sdf = function(x) sd(x[,param]))
    sapply(x, FUN = funs[[fun_name]])
  }
  if (inc_warmup == FALSE) {
    X <- lapply(1:length(sampler_params), function(i) {
      out <- sampler_params[[i]]
      out[-(1:warmup_val), ]
    })
  }
  out <- if (report == "maximum") sapply_funs(X, "maxf") 
  else if (report == "minimum") sapply_funs(X, "minf")
  else if (report == "sd") sapply_funs(X, "sdf")
  else sapply_funs(X, "meanf")
  
  names(out) <- paste0("chain",1:length(out))
  out
}

# summary statistics for algorithm=NUTS or algorithm=HMC sampler parameters
.sampler_summary <- function(sampler_params, warmup_val, inc_warmup = TRUE, 
                             report = "average", digits = 4){ 
  
  params <- colnames(sampler_params[[1]])
  out <- sapply(params, FUN = function(p) .sampler_stuff(X = sampler_params, 
                                                         param = p, 
                                                         inc_warmup = inc_warmup, 
                                                         report = report))
  
  if (length(dim(out)) > 1) { # if multiple chains
    out <- rbind("All chains" = colMeans(out), out)
    colnames(out) <- gsub("__","",colnames(out))
    out <- formatC(round(out, digits), format='f', digits=digits)
    # out <- cbind(Chain = rownames(out), out)
  } else { # if only 1 chain
    names(out) <- gsub("__.chain1", "", names(out))
    out <- t(out)
    out <- round(out, digits)
  }
  out
}

