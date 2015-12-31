# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.


# Extract the contents of the shiny_stan_object slots
model_name <- object@model_name
param_names <- object@param_names
samps_all <- object@samps_all
sampler_params <- object@sampler_params
nIter <- object@nIter
nChains <- object@nChains
warmup_val <- object@nWarmup
samps_post_warmup <- samps_all[(warmup_val + 1):nIter,, ,drop = FALSE]

MISC <- object@misc
MISC_nms <- names(MISC)
stan_method <- if ("stan_method" %in% MISC_nms)
  MISC$stan_method else "Not Stan"
stan_algorithm <- if ("stan_algorithm" %in% MISC_nms) 
  MISC$stan_algorithm else "Not Stan"

pp_yrep <- if ("pp_yrep" %in% MISC_nms) MISC[["pp_yrep"]] else NULL
pp_y <- if ("pp_y" %in% MISC_nms) MISC[["pp_y"]] else NULL

sampler_params_post_warmup <- 
  if (!is.list(sampler_params) | identical(sampler_params, list(NA))) 
    FALSE else if (!is.matrix(sampler_params[[1L]])) 
      FALSE else { 
        lapply(1:length(sampler_params), function(i) {
          out <- sampler_params[[i]]
          out <- if (warmup_val == 0) out else out[-(1:warmup_val), ]
          rownames(out) <- (warmup_val + 1):(warmup_val + nrow(out))
          out
        })
      }
if (!identical(FALSE, sampler_params_post_warmup)) {
  .stepsize_pw <- .sampler_param_pw(sampler_params_post_warmup, which = "stepsize__", 
                                    warmup_val = object@nWarmup)
  .ndivergent_pw <- .sampler_param_pw(sampler_params_post_warmup, which = "n_divergent__", 
                                      warmup_val = object@nWarmup)
  .treedepth_pw <- .sampler_param_pw(sampler_params_post_warmup, which = "treedepth__", 
                                     warmup_val = object@nWarmup)
  .accept_stat_pw <- .sampler_param_pw(sampler_params_post_warmup, which = "accept_stat__", 
                                       warmup_val = object@nWarmup)
}

table_stats <- fit_summary <- object@summary
if (!stan_method == "variational") {
  sel <- colnames(table_stats) %in% c("Rhat", "n_eff")
  table_stats <- cbind(table_stats[, sel], table_stats[, !sel])
  sel <- NULL
  table_stats[,"n_eff"] <- round(table_stats[,"n_eff"])
}

from_rstanarm <- if (is.null(MISC$stanreg)) FALSE else MISC$stanreg
if (from_rstanarm) pp_check_plots <- MISC$pp_check_plots

