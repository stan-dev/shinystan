server <- function(input, output, session) {
  # utilities
  source("MODULES/UTILS/plotOptions.R", local = TRUE)
  source("MODULES/UTILS/report.R", local = TRUE)
  
  # home tab
  source("MODULES/HOME/homepage.R", local = TRUE)
  source("MODULES/HOME/warnings.r", local = TRUE)
  
  # diagnoses tab
  source("MODULES/DIAGNOSE/diagnoseHomepage.R", local = TRUE)
  
  source("MODULES/DIAGNOSE/divergentScatter.r", local = TRUE)
  source("MODULES/DIAGNOSE/divergentTransitions.r", local = TRUE)
  source("MODULES/DIAGNOSE/energy.r", local = TRUE)
  source("MODULES/DIAGNOSE/treedepth.r", local = TRUE)
  source("MODULES/DIAGNOSE/stepSize.r", local = TRUE)
  source("MODULES/DIAGNOSE/parallelCoordinates.r", local = TRUE)
  source("MODULES/DIAGNOSE/pairs.r", local = TRUE)
  source("MODULES/DIAGNOSE/acceptance.r", local = TRUE)
  
  source("MODULES/DIAGNOSE/tracePlot.r", local = TRUE)
  source("MODULES/DIAGNOSE/rankPlot.r", local = TRUE)
  source("MODULES/DIAGNOSE/rhat_n_eff_se_mean.r", local = TRUE)
  source("MODULES/DIAGNOSE/autoCorrelation.r", local = TRUE)
  
  source("MODULES/DIAGNOSE/statsTableHMC.r", local = TRUE)
  source("MODULES/DIAGNOSE/rhat_n_eff_se_mean_stats.r", local = TRUE)
  
  # source("MODULES/DIAGNOSE/diagnosticReport.r", local = TRUE)
  
  # estimate tab
  source("MODULES/ESTIMATE/estimateHomepage.R", local = TRUE)
  
  source("MODULES/ESTIMATE/visualEstimate.R", local = TRUE)
  source("MODULES/ESTIMATE/scatterPlot.R", local = TRUE)
  source("MODULES/ESTIMATE/densityPlot.R", local = TRUE)
  source("MODULES/ESTIMATE/histogramPlot.R", local = TRUE)
  source("MODULES/ESTIMATE/intervalsPlot.R", local = TRUE)
  source("MODULES/ESTIMATE/areasPlot.R", local = TRUE)
  
  source("MODULES/ESTIMATE/numericalEstimate.R", local = TRUE)
  source("MODULES/ESTIMATE/summaryTable.R", local = TRUE)
  source("MODULES/ESTIMATE/summaryTableLatex.R", local = TRUE)
  
  # more tab
  source("MODULES/MORE/about.R", local = TRUE)
  source("MODULES/MORE/modelCode.R", local = TRUE)
  source("MODULES/MORE/help.R", local = TRUE)

  # internal functions or events that are required for general use.
  # save and close button
  observeEvent(
    input$save_and_close_button, 
    stopApp()
  )
  
  # transformation options
  transformation_choices <-
    c(
      "abs", "atanh",
      cauchit = "pcauchy", cloglog = ".cloglog",
      "exp", "expm1",
      "identity", inverse = ".inverse", inv_logit = "plogis",
      "log", "log10", "log2", "log1p", logit = "qlogis",
      probit = "pnorm",
      square = ".square", "sqrt"
    )
  
  
  # to reduce code in modules for selecting theme's in Options.
  select_theme <- function(name){
    switch(name,
           "bayesplot default" = "theme_default()", 
           "light" = "theme_light()",
           "gray" = "theme_gray()",
           "dark" = "theme_dark()",
           "minimal" = "theme_minimal()",
           "linedraw" = "theme_linedraw()",
           "classic" = "theme_classic()")
  }
  
  
  # To make listed parameter overviews
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
        names(ch_out) <- c(paste("ALL (remaining)", group), ch)
        choices[[i]] <- ch_out
      }
    }
    
    choices
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
  
  
  # this is used to reference the HTML links to the correct page on the homepage
  # module. Need to find a way to actually incorporate this in the module and not
  # in the main server file.
  toc_entries <- c("Estimate", "Diagnose", "Explore", "Model Code")
  observe({
    local({
      lapply(toc_entries, function(x) {
        id <- paste0("toc_", if (x == "Model Code") "more" else tolower(x))
        shinyjs::onclick(id, updateTabsetPanel(session, "nav", selected = x))
     })
    })
  })

  # calling modules
  
  # home tab
  callModule(homepage, "homepage")
  # diagnose tab
  callModule(diagnose, "diagnoseHomepage")
  # estimate tab
  callModule(estimate, "estimateHomepage")
  # about tab
  callModule(about, "about")
  callModule(modelCode, "modelCode")
  callModule(help, "help")
  
}

