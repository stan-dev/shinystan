warningsUI <- function (id) {
  ns <- NS(id)
  
  fluidRow(
    br(),
    # HTML(paste0("<div style='background-color:gray; color:white; 
    #               padding:5px; opacity:.3'>",
    #             "HMC Specific Warnings", 
    #             "</div>")),
    if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") uiOutput(ns("divergence")),
    if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") uiOutput(ns("treedepth")),
    if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") uiOutput(ns("energy")),
    # HTML(paste0("<div style='background-color:gray; color:white; 
    #               padding:5px; opacity:.3'>",
    #             "General Warnings", 
    #             "</div>")),
    br(),
    if(sso@misc$stan_method == "sampling") uiOutput(ns("n_eff")),
    if(sso@misc$stan_method == "sampling") uiOutput(ns("se_mean")),
    if(sso@misc$stan_method == "sampling") uiOutput(ns("rhat"))
  )
  
}

warnings <- function (input, output, session) {
  
  output$divergence <- renderUI({
    ns <- session$ns
    
    checkDivergences <- lapply(sso@sampler_params, "[", , "divergent__") %>%
      lapply(., as.data.frame) %>%
      lapply(., filter, row_number() > sso@n_warmup) %>%
      lapply(., function (x) x > 0 ) %>% lapply(., sum) %>% 
      unlist(.) %>% sum(.) %>%
      paste0(., " of ", (sso@n_iter-sso@n_warmup) * sso@n_chain,
             " iterations ended with a divergence (",
             round((. / ((sso@n_iter-sso@n_warmup) * sso@n_chain)) * 100, 1),
             "%).")
    
    if(lapply(sso@sampler_params, "[", , "divergent__") %>%
       lapply(., as.data.frame) %>%
       lapply(., filter, row_number() > sso@n_warmup) %>%
       lapply(., function (x) x > 0 ) %>% lapply(., sum) %>% 
       unlist(.) %>% sum(.) > 0) {
      HTML(paste0("<div style='background-color:red; color:white; 
                    padding:5px; opacity:.3'>",
                  tags$a(checkDivergences,onclick="customHref('Diagnose');customHref('Divergent Scatter');"),
                   "</div>"))
    } else {
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                  padding:5px; opacity:.3'>",
                  tags$a(checkDivergences,onclick="customHref('Diagnose');customHref('Divergent Scatter');"),
                  "</div>"))
    }
    
  })
  
  
  output$treedepth <- renderUI({
    ns <- session$ns
    
    check_treedepth <- lapply(sso@sampler_params, "[", , "treedepth__") %>%
      lapply(., as.data.frame) %>%
      lapply(., filter, row_number() > sso@n_warmup) %>%
      lapply(., function (x) x == sso@misc$max_td ) %>% lapply(., sum) %>% 
      unlist(.) %>% sum(.) %>%
      paste0(., " of ", (sso@n_iter-sso@n_warmup) * sso@n_chain,
             " iterations saturated the maximum tree depth of ", 
             sso@misc$max_td, " (",
             round((. / ((sso@n_iter-sso@n_warmup) * sso@n_chain)) * 100, 1),
             "%).")
    
    if(lapply(sso@sampler_params, "[", , "treedepth__") %>%
       lapply(., as.data.frame) %>%
       lapply(., filter, row_number() > sso@n_warmup) %>%
       lapply(., function (x) x == sso@misc$max_td ) %>% 
       lapply(., sum) %>% unlist(.) %>% sum(.) > 0) {
        HTML(paste0("<div style='background-color:red; color:white; 
                    padding:5px; opacity:.3'>",
                    tags$a(check_treedepth,onclick="customHref('Diagnose');customHref('Treedepth Information');"),
                    "</div>"))
    } else {
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                  padding:5px; opacity:.3'>",
                  tags$a(check_treedepth,onclick="customHref('Diagnose');customHref('Treedepth Information');"),
                   "</div>"))
    }
  })
  
 
  output$energy <- renderUI({
    ns <- session$ns
    
    check_energy_sso <- function(shinystan.object){
      
      energy <- lapply(sso@sampler_params, "[", , "energy__") %>%
        lapply(., as.data.frame) %>% 
        lapply(., filter, row_number() > sso@n_warmup) 
      
      EBFMIs <- c()
      
      for(chain in 1:sso@n_chain) {
        EBFMIs[chain] <- apply(energy[[chain]], 2, function(x){
          numer <- sum(diff(x)^2)/length(x)
          denom <- var(x)
          return(numer/denom)
        })
      }
      
      bad_chains <- which(EBFMIs < 0.1)
      if (!length(bad_chains)) {
        paste0("E-BFMI indicated no pathological behavior.")
      }
      else {
        EBFMIsWarnings <- NULL
        for(bad in bad_chains){
          EBFMIsWarnings <- c(EBFMIsWarnings, 
                              paste0("Chain ", bad, ": E-BFMI = ", round(EBFMIs[bad], 3), ".<br>"))
        }
        paste(paste0("E-BFMI indicated possible pathological behavior:<br>", 
                   paste(EBFMIsWarnings, collapse = "") ,
                   "E-BFMI below 0.2 indicates you may need to reparameterize your model."))
      }
      
    }
    
    energyWarning <- check_energy_sso(sso)
    
    
    if(check_energy_sso(sso) != "E-BFMI indicated no pathological behavior.") {
      HTML(paste0("<div style='background-color:red; color:white; 
                padding:5px; opacity:.3'>",
                  tags$a(energyWarning,onclick="customHref('Diagnose');customHref('Energy Information');"),
                  "</div>"))
    } else {
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                padding:5px; opacity:.3'>",
                  tags$a(energyWarning,onclick="customHref('Diagnose');customHref('Energy Information');"),
                  "</div>"))
    }
  })
  
  
  output$n_eff <- renderUI({
    
   bad_n_eff <- rownames(sso@summary)[sso@summary[, "n_eff"] / ((sso@n_iter- sso@n_warmup) * sso@n_chain) < .1]
   bad_n_eff <- bad_n_eff[!is.na(bad_n_eff)]
   n_effWarning <- paste("The following parameters have an effective sample size less than 10% of the total sample size:<br>",
                   paste(bad_n_eff, collapse = ", "))
    
    if(length(bad_n_eff) < 1){
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                padding:5px; opacity:.3'>",
                  tags$a("No parameters have an effective sample size less than 10% of the total sample size.", 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</div>"))
    } else {
      HTML(paste0("<div style='background-color:red; color:white; 
                padding:5px; opacity:.3'>",
                  tags$a(n_effWarning, 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</div>"))
    }
  })

  
  output$se_mean <- renderUI({
    
    bad_se_mean <- rownames(sso@summary)[sso@summary[, "se_mean"] / sso@summary[, "sd"] > .1]
    bad_se_mean <- bad_se_mean[!is.na(bad_se_mean)]
    se_meanWarning <- paste("The following parameters have a Monte Carlo standard error greater than 10% of the posterior standard deviation:<br>",
                          paste(bad_se_mean, collapse = ", "))
    
    if(length(bad_se_mean) < 1){
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                  padding:5px; opacity:.3'>",
                  tags$a("No parameters have a standard error greater than 10% of the posterior standard deviation.", 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</div>"))
    } else {
      HTML(paste0("<div style='background-color:red; color:white; 
                  padding:5px; opacity:.3'>",
                  tags$a(se_meanWarning, 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</div>"))
    }
  })
  
  output$rhat <- renderUI({
    
    bad_rhat <- rownames(sso@summary)[sso@summary[, "Rhat"] > 1.1]
    bad_rhat <- bad_rhat[!is.na(bad_rhat)]
    rhatWarning <- paste("The following parameters have an Rhat value above 1.1:<br>",
                            paste(bad_rhat, collapse = ", "))
    
    if(length(bad_rhat) < 1){
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                  padding:5px; opacity:.3'>",
                  tags$a("No parameters have an Rhat value above 1.1.", 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</div>"))
    } else {
      HTML(paste0("<div style='background-color:red; color:white; 
                  padding:5px; opacity:.3'>",
                  tags$a(rhatWarning, 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</div>"))
    }
  })
  #  # should be under 1.1
  
  
  
}


