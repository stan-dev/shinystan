
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
# This file is part of shinyStan
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



# make_param_list_with_groups ------------------------------------------------------
make_param_list_with_groups_sort <- reactive({
  validate(need(!is.null(input$param_plot_sort_j), message = "Loading..."))
  sort_j <- input$param_plot_sort_j
  choices <- list()
  param_groups <- names(object@param_dims)
  ll <- length(object@param_dims)
  LL <- sapply(1:ll, function(i) length(object@param_dims[[i]]))

  choices[1:ll] <- ""
  names(choices) <- param_groups
  for(i in 1:ll) {
    if (LL[i] == 0) {
      choices[[i]] <- list(param_groups[i])
    }
    else {
      group <- param_groups[i]
      temp <- paste0("^",group,"\\[")
      ch <- object@param_names[grep(temp, object@param_names)]
      # the next line avoids parameters whose names include the group name of a
      # different group of parameters being included in the latter group, e.g.
      # if we have b_bias[1], b_bias[2], bias[1], bias[2] then we want to avoid
      # bias[1] and bias[2] being included in the b_bias group
      ch <- ch[which(substr(ch, 1, nchar(group)) == group)]


      if (sort_j == TRUE & (LL[i] > 1)) {
        # change sorting so e.g. "beta[1,1] beta[1,2] beta[2,1] beta[2,2]"
        # instead of "beta[1,1] beta[2,1] beta[1,2] beta[2,2]"
        ch <- gtools::mixedsort(ch)
      }

      ch_out <- c(paste0(group,"_as_shiny_stan_group"), ch)
      names(ch_out) <- c(paste("ALL", group), ch)
      choices[[i]] <- ch_out
    }
  }

  choices
})
