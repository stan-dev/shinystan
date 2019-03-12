rhat_n_eff_se_meanUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  
  tagList(
    
    wellPanel(
      fluidRow(
        column(width = 3, 
               sliderInput(
                 ns("rhat_threshold"),
                 withMathJax("\\(\\hat{R} \\text{ warning threshold}\\) "),
                 ticks = FALSE,
                 value = 1.1,
                 min = 1,
                 max = 1.2,
                 step = 0.01
               )),
        column(width = 3, 
               sliderInput(
                 ns("n_eff_threshold"),
                 withMathJax("\\(n_{eff} \\text{ / } \\textit{N} \\text{ warning threshold}\\) "),
                 ticks = FALSE,
                 value = 10,
                 min = 0,
                 max = 100,
                 step = 5,
                 post = "%"
               )),
        column(width = 3,
               sliderInput(
                 ns("mcse_threshold"),
                 "\\(\\text{se}_{mean} \\text{ / } \\textit{sd} \\text{ warning threshold}\\) ",
                 ticks = FALSE,
                 value = 10,
                 min = 0,
                 max = 100,
                 step = 5,
                 post = "%"
               ))
      )
    ),
    fluidRow(
      column(width = 3, h4(withMathJax("\\(\\hat{R}\\)"), align = "center")),
      column(width = 3, h4(withMathJax("\\(n_{eff} / N\\)"), align = "center")),
      column(width = 3, h4(withMathJax("\\(mcse / sd\\)"), align = "center"))
    ),
    fluidRow(
      column(width = 3, plotOutput(ns("rhatPlot"))),
      column(width = 3, plotOutput(ns("n_effPlot"))),
      column(width = 3, plotOutput(ns("se_meanPlot")))
    ),
    fluidRow(
      column(width = 3, uiOutput(ns("rhat"))),
      column(width = 3, uiOutput(ns("n_eff"))),
      column(width = 3, uiOutput(ns("se_mean")))
    )
  )  
  
}


rhat_n_eff_se_mean <- function(input, output, session){
  
  
  plotOut_rhat <- function(){
    color_scheme_set("blue")
    mcmc_rhat_hist(sso@summary[, "Rhat"])
  }
  
  output$rhatPlot <- renderPlot({
    plotOut_rhat()
  })
  
  plotOut_n_eff <- function(){
    color_scheme_set("blue")
    mcmc_neff_hist(sso@summary[, "n_eff"] / ((sso@n_iter - sso@n_warmup) * sso@n_chain))
  }
  
  output$n_effPlot <- renderPlot({
    plotOut_n_eff()
  })
  
  plotOut_se_mean <- function(){
    se_sd_table <- tibble(diagnostic = rep("se_sd_ratio", length(sso@param_names)),
                          parameter = as.factor(sso@param_names),
                          value = sso@summary[, "se_mean"] / sso@summary[, "sd"],
                          rating = cut(sso@summary[, "se_mean"] / sso@summary[, "sd"], breaks = c(Inf, 0.5, 0.1, 0), 
                                       labels = c("low", "ok", "high")),
                          description = as.character(cut(sso@summary[, "se_mean"] / sso@summary[, "sd"], breaks = c(Inf, 0.5, 0.1, 0), 
                                                         labels = c(expression(MC[se] / sd <= 0.1),
                                                                    expression(MC[se] / sd <= 0.5),
                                                                    expression(MC[se] / sd > 0.5)))))
    
    color_scheme_set("blue") 
    ggplot(data = se_sd_table, mapping = aes_(x = ~value, color = ~rating, fill = ~rating)) + 
      geom_histogram(size = 0.25, na.rm = TRUE) + 
      labs(x = expression(MC[se] / sd),y = NULL) + 
      bayesplot:::dont_expand_y_axis(c(0.005, 0)) + bayesplot_theme_get() + 
      yaxis_title(FALSE) + yaxis_text(FALSE) + yaxis_ticks(FALSE) +
      theme(legend.position = "none")
  }
  
  output$se_meanPlot <- renderPlot({
    plotOut_se_mean()
  })
  
  
  output$rhat <- renderUI({
    
    bad_rhat <- rownames(sso@summary)[sso@summary[, "Rhat"] > reactive(input$rhat_threshold)()]
    bad_rhat <- bad_rhat[!is.na(bad_rhat)]
    rhatWarning <- paste0("The following parameters have an Rhat value above ", 
                          reactive(input$rhat_threshold)(), ":<br>",
                          paste(bad_rhat, collapse = ", "))
    
    if(length(bad_rhat) < 1){
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                  padding:5px; opacity:.3'>",
                  "No parameters have an Rhat value above ", reactive(input$rhat_threshold)(), ".", 
                  "</div>"))
    } else {
      HTML(paste0("<div style='background-color:red; color:white; 
                  padding:5px; opacity:.3'>",
                  rhatWarning, "</div>"))
    }
  })
  
  output$n_eff <- renderUI({
    
    bad_n_eff <- rownames(sso@summary)[sso@summary[, "n_eff"] / ((sso@n_iter- sso@n_warmup) * sso@n_chain) < 
                                         (reactive(input$n_eff_threshold)() / 100)]
    bad_n_eff <- bad_n_eff[!is.na(bad_n_eff)]
    n_effWarning <- paste0("The following parameters have an effective sample size less than ", 
                           reactive(input$n_eff_threshold)(), "% of the total sample size:<br>",
                           paste(bad_n_eff, collapse = ", "))
    
    if(length(bad_n_eff) < 1){
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                  padding:5px; opacity:.3'>",
                  "No parameters have an effective sample size less than ",
                  reactive(input$n_eff_threshold)(), "% of the total sample size.", 
                  "</div>"))
    } else {
      HTML(paste0("<div style='background-color:red; color:white; 
                  padding:5px; opacity:.3'>",
                  n_effWarning, "</div>"))
    }
  })
  
  
  output$se_mean <- renderUI({
    
    bad_se_mean <- rownames(sso@summary)[sso@summary[, "se_mean"] / sso@summary[, "sd"] > 
                                           (reactive(input$mcse_threshold)() / 100)]
    bad_se_mean <- bad_se_mean[!is.na(bad_se_mean)]
    se_meanWarning <- paste0("The following parameters have a Monte Carlo standard error greater than ",
                             reactive(input$mcse_threshold)(), "% of the posterior standard deviation:<br>",
                             paste(bad_se_mean, collapse = ", "))
    
    if(length(bad_se_mean) < 1){
      HTML(paste0("<div style='background-color:lightblue; color:black; 
                  padding:5px; opacity:.3'>",
                  "No parameters have a standard error greater than ", 
                  reactive(input$mcse_threshold)(), "% of the posterior standard deviation.", 
                  "</div>"))
    } else {
      HTML(paste0("<div style='background-color:red; color:white; 
                  padding:5px; opacity:.3'>",
                  se_meanWarning, "</div>"))
    }
  })
  
  return(reactive({
    list("rhatPlot" = plotOut_rhat(),
         "n_effPlot" = plotOut_n_eff(),
         "se_meanPlot" = plotOut_se_mean())
  }))
  
}
