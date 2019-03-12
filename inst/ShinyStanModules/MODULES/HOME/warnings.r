warningsUI <- function (id) {
  ns <- NS(id)
  
  fluidRow(
    wellPanel(id = "warningTab",
              
              if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") uiOutput(ns("divergence")),
              if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") uiOutput(ns("treedepth")),
              if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") uiOutput(ns("energy")),
              if(sso@misc$stan_method == "sampling") uiOutput(ns("n_eff")),
              if(sso@misc$stan_method == "sampling") uiOutput(ns("se_mean")),
              if(sso@misc$stan_method == "sampling") uiOutput(ns("rhat"))
    )
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
      HTML(paste0("<div style='background-color:white; color:black; text-align: left;
                    padding:5px; opacity:1'>", "<li>", 
                  tags$a(checkDivergences,onclick="customHref('Diagnose');customHref('Divergent Scatter');"),
                  "</li></div>"))
    } else {
      
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
      HTML(paste0("<div style='background-color:white; color:black; text-align: left;
                    padding:5px; opacity:1'>", "<li>", 
                  tags$a(check_treedepth,onclick="customHref('Diagnose');customHref('Treedepth Information');"),
                  "</li></div>"))
    } else {
      
    }
  })
  
  
  output$energy <- renderUI({
    ns <- session$ns
    
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
    
    if(length(bad_chains) > 0) {
      HTML(paste0("<div style='background-color:white; color:black; text-align: left;
                    padding:5px; opacity:1'>", "<li>", 
                  tags$a(paste0("E-BFMI indicated possible pathological behavior for the following chain(s): ",
                                bad_chains),onclick="customHref('Diagnose');customHref('Energy Information');"),
                  "</li></div>"))
    } else {
      
    }
  })
  
  
  output$n_eff <- renderUI({
    
    bad_n_eff <- rownames(sso@summary)[sso@summary[, "n_eff"] / ((sso@n_iter- sso@n_warmup) * sso@n_chain) < .1]
    bad_n_eff <- bad_n_eff[!is.na(bad_n_eff)]
    n_effWarning <- paste(length(bad_n_eff), 
                          "parameters have an effective sample size less than 10% of the total sample size.")
    
    if(length(bad_n_eff) < 1){
      
    } else {
      HTML(paste0("<div style='background-color:white; color:black; text-align: left;
                    padding:5px; opacity:1'>", "<li>", 
                  tags$a(n_effWarning, 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</li></div>"))
    }
  })
  
  
  output$se_mean <- renderUI({
    
    bad_se_mean <- rownames(sso@summary)[sso@summary[, "se_mean"] / sso@summary[, "sd"] > .1]
    bad_se_mean <- bad_se_mean[!is.na(bad_se_mean)]
    se_meanWarning <- paste(length(bad_se_mean), 
                            "parameters have a Monte Carlo standard error greater than 10% of the posterior standard deviation.")
    
    if(length(bad_se_mean) < 1){
      
    } else {
      HTML(paste0("<div style='background-color:white; color:black; text-align: left;
                    padding:5px; opacity:1'>", "<li>", 
                  tags$a(se_meanWarning, 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</li></div>"))
    }
  })
  
  output$rhat <- renderUI({
    
    bad_rhat <- rownames(sso@summary)[sso@summary[, "Rhat"] > 1.1]
    bad_rhat <- bad_rhat[!is.na(bad_rhat)]
    rhatWarning <- paste(length(bad_rhat), 
                         "parameters have an Rhat value above 1.1.")
    
    if(length(bad_rhat) < 1){
      
    } else {
      HTML(paste0("<div style='background-color:white; color:black; text-align: left;
                    padding:5px; opacity:1'>", "<li>", 
                  tags$a(rhatWarning, 
                         onclick = "customHref('Diagnose');customHref('rhat_neff_se_mean_plot_tab');"), "</li></div>"))
    }
  })
  
}


