# Extract the contents of the shiny_stan_object slots and do some additional
# processing

MODEL_NAME <- slot(object, "model_name")
PARAM_NAMES <- slot(object, "param_names")
PARAM_DIMS <- slot(object, "param_dims")
SAMPS_all <- slot(object, "posterior_sample")
SAMPLER_PARAMS <- slot(object, "sampler_params")
N_ITER <- slot(object, "n_iter")
N_CHAIN <- slot(object, "n_chain")
N_WARMUP <- slot(object, "n_warmup")
SAMPS_post_warmup <-
  SAMPS_all[seq(from = N_WARMUP + 1, to = N_ITER), , , drop = FALSE]

MISC <- slot(object, "misc")
MISC_nms <- names(MISC)
STAN_METHOD <- if ("stan_method" %in% MISC_nms)
  MISC$stan_method else "Not Stan"
STAN_ALGORITHM <- if ("stan_algorithm" %in% MISC_nms) 
  MISC$stan_algorithm else "Not Stan"

pp_yrep <- if ("pp_yrep" %in% MISC_nms) 
  MISC[["pp_yrep"]] else NULL
pp_y <- if ("pp_y" %in% MISC_nms) 
  MISC[["pp_y"]] else NULL

SAMPLER_PARAMS_post_warmup <- 
  if (!is.list(SAMPLER_PARAMS) | identical(SAMPLER_PARAMS, list(NA))) 
    FALSE else if (!is.matrix(SAMPLER_PARAMS[[1L]])) 
      FALSE else { 
        lapply(seq_along(SAMPLER_PARAMS), function(i) {
          out <- SAMPLER_PARAMS[[i]]
          out <- if (N_WARMUP == 0) out else out[-(1:N_WARMUP), ]
          rownames(out) <- seq(from = N_WARMUP + 1, to = N_WARMUP + nrow(out))
          out
        })
      }
if (!identical(FALSE, SAMPLER_PARAMS_post_warmup)) {
  .stepsize_pw <-
    .sampler_param_pw(SAMPLER_PARAMS_post_warmup,
                      which = "stepsize__",
                      warmup_val = N_WARMUP)
  .ndivergent_pw <-
    .sampler_param_pw(SAMPLER_PARAMS_post_warmup,
                      which = "divergent__",
                      warmup_val = N_WARMUP)
  .treedepth_pw <-
    .sampler_param_pw(SAMPLER_PARAMS_post_warmup,
                      which = "treedepth__",
                      warmup_val = N_WARMUP)
  .accept_stat_pw <-
    .sampler_param_pw(SAMPLER_PARAMS_post_warmup,
                      which = "accept_stat__",
                      warmup_val = N_WARMUP)
  .energy_pw <-
    .sampler_param_pw(SAMPLER_PARAMS_post_warmup,
                      which = "energy__",
                      warmup_val = N_WARMUP)
}

SUMMARY <- slot(object, "summary")
TABLE_STATS <- SUMMARY
if (!STAN_METHOD == "variational") {
  sel <- colnames(TABLE_STATS) %in% c("Rhat", "n_eff")
  TABLE_STATS <- cbind(TABLE_STATS[, sel], TABLE_STATS[,!sel])
  sel <- NULL
  TABLE_STATS[, "n_eff"] <- round(TABLE_STATS[, "n_eff"])
}

# ppcheck plots from rstanarm
if (isTRUE(MISC$stanreg)) 
  PPC_plots <- MISC$pp_check_plots
