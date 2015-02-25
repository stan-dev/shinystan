# make_param_list_with_groups ------------------------------------------------------
make_param_list_with_groups_sort <- reactive({
#   if (is.null(input$multiparam_sort)) {
#     return()
#   }
#   sort_j <- ifelse(input$multiparam_sort == "j", TRUE, FALSE)
  sort_j <- FALSE
  choices <- list()
  ll <- length(object@param_dims)
  LL <- sapply(1:ll, function(i) length(object@param_dims[[i]]))

  choices[1:ll] <- ""
  names(choices) <- object@param_groups
  for(i in 1:ll) {
    if (LL[i] == 0) {
      choices[[i]] <- list(object@param_groups[i])
    }
    else {
      group <- object@param_groups[i]
      temp <- paste0(group,"\\[")
      ch <- object@param_names[grep(temp, object@param_names)]

      # the next line avoids parameters whose names include the group name of a
      # different group of parameters being included in the latter group, e.g.
      # if we have b_bias[1], b_bias[2], bias[1], bias[2] then we want to avoid
      # bias[1] and bias[2] being included in the b_bias group
      ch <- ch[which(substr(ch, 1, nchar(group)) == group)]


      if (sort_j == TRUE & LL[i] > 1) {
        # change sorting so to, e.g. "beta[1,1] beta[1,2] beta[2,1] beta[2,2]"
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
