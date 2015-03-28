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



# save_samples <- reactive({
#   params <- input$save_params
#   inc_warmup <- input$save_inc_warmup
#   combine_chains <- input$save_combine_chains
#
#   samps <- samps_all
#   if (!inc_warmup) samps <- samps_post_warmup
#
#   params <- .update_params_with_groups(params, object@param_names)
#   nParams <- length(params)
#
#   samps_use <- samps[,, params]
#   if (combine_chains) {
#     n <- dim(samps_use)[1] * dim(samps_use)[2]
#     samps_use <- array(samps_use, c(n, nParams))
#     colnames(samps_use) <- params
#   }
#   samps_use
# })
#
