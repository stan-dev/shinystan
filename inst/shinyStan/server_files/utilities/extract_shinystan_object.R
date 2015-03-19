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



# Extract the content of the shiny_stan_object slots
object <- shinystan_object
model_name <- object@model_name
samps_all <- object@samps_all
sampler_params <- object@sampler_params
nIter <- object@nIter
nChains <- object@nChains
warmup_val <- object@nWarmup
samps_post_warmup <- samps_all[(warmup_val + 1):nIter,, ,drop = FALSE]
fit_summary <- object@summary
param_names <- object@param_names
stan_algorithm <- object@stan_algorithm