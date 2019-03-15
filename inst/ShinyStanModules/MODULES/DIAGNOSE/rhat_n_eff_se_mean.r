rhat_n_eff_se_meanUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  
  tagList(
    wellPanel(
      fluidRow(
        column(width = 6,withMathJax(),
               radioButtons(ns("selectStat"), h5("Statistic"), 
                            choices = c("\\(\\hat{R}\\)",
                                        "\\(n_{eff}\\)",
                                        "\\(\\text{se}_{mean} \\text{ / } \\textit{sd}\\)"),
                            inline = FALSE)
        ),
        column(width = 6,
               uiOutput(ns("conditionalSlider"))
        )
        # splitLayout(
        # sliderInput(
        #   ns("rhat_threshold"),
        #   withMathJax("\\(\\hat{R} \\text{ warning threshold}\\) "),
        #   ticks = FALSE,
        #   value = 1.1,
        #   min = 1,
        #   max = 1.2,
        #   step = 0.01
        # ),
        #   sliderInput(
        #   ns("n_eff_threshold"),
        #   withMathJax("\\(n_{eff} \\text{ / } \\textit{N} \\text{ warning threshold}\\) "),
        #   ticks = FALSE,
        #   value = 10,
        #   min = 0,
        #   max = 100,
        #   step = 5,
        #   post = "%"
        # ),
        #   sliderInput(
        #     ns("mcse_threshold"),
        #     "\\(\\text{se}_{mean} \\text{ / } \\textit{sd} \\text{ warning threshold}\\) ",
        #     ticks = FALSE,
        #     value = 10,
        #     min = 0,
        #     max = 100,
        #     step = 5,
        #     post = "%"
        #   )
        # )
      )
    ),
    uiOutput(ns("conditionalUI"))
    # fluidRow(
    #   column(width = 12,
    #          splitLayout(
    #            verticalLayout(
    #              h4(withMathJax("\\(\\hat{R}\\)")),
    #              textOutput(ns("rhat"))   
    #            ),
    #            verticalLayout(
    #              h4(withMathJax("\\(n_{eff} / N\\)")),
    #              textOutput(ns("n_eff"))
    #            ),
    #            verticalLayout(
    #              h4(withMathJax("\\(mcse / sd\\)")),
    #              uiOutput(ns("se_mean"))
    #            )
    #          )
    #   )
    # ),
    # fluidRow(
    #   column(width = 4, plotOutput(ns("rhatPlot"))),
    #   column(width = 4, plotOutput(ns("n_effPlot"))),
    #   column(width = 4, plotOutput(ns("se_meanPlot")))
    # )
  )
}


rhat_n_eff_se_mean <- function(input, output, session){
  
  stat <- reactive({input$selectStat})
  
  output$conditionalSlider <- renderUI({
    if(stat() == "\\(\\hat{R}\\)"){
      tagList(
        withMathJax(),
        br(),
        sliderInput(
          session$ns("rhat_threshold"),
          "\\(\\hat{R} \\text{ warning threshold}\\)",
          ticks = FALSE,
          value = 1.1,
          min = 1,
          max = 1.2,
          step = 0.01,
          width = "30%"
        )
      )
    } else {
      if(stat() == "\\(n_{eff}\\)"){
        tagList(
          withMathJax(),
          br(),
          sliderInput(
            session$ns("n_eff_threshold"),
            "\\(n_{eff} \\text{ / } \\textit{N} \\text{ warning threshold}\\)",
            ticks = FALSE,
            value = 10,
            min = 0,
            max = 100,
            step = 5,
            post = "%",
            width = "30%"
          )
        )
      } else {
        if(stat() == "\\(\\text{se}_{mean} \\text{ / } \\textit{sd}\\)"){
          tagList(
            withMathJax(),
            br(),
            sliderInput(
              session$ns("mcse_threshold"),
              "\\(\\text{se}_{mean} \\text{ / } \\textit{sd} \\text{ warning threshold}\\)",
              ticks = FALSE,
              value = 10,
              min = 0,
              max = 100,
              step = 5,
              post = "%",
              width = "30%"
            )
          )
        }
      }
    }
  })
  
  # first plot functions than renderPlots using functions to enable pushing back plots out of module
  plotOut_rhat <- function(){
    color_scheme_set("blue")
    mcmc_rhat_hist(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "Rhat"])
  }
  
  plotOut_n_eff <- function(){
    color_scheme_set("blue")
    mcmc_neff_hist(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "n_eff"] / ((shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) * shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain))
  }
  
  plotOut_se_mean <- function(){
    se_sd_table <- tibble(diagnostic = rep("se_sd_ratio", length(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names)),
                          parameter = as.factor(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names),
                          value = shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "se_mean"] / shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "sd"],
                          rating = cut(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "se_mean"] / shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "sd"], breaks = c(Inf, 0.5, 0.1, 0),
                                       labels = c("low", "ok", "high")),
                          description = as.character(cut(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "se_mean"] / shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "sd"], breaks = c(Inf, 0.5, 0.1, 0),
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
  
  output$rhatPlot <- renderPlot({
    plotOut_rhat()
  })
  
  output$n_effPlot <- renderPlot({
    plotOut_n_eff()
  })
  
  output$se_meanPlot <- renderPlot({
    plotOut_se_mean()
  })
  
  # needed to get a FALSE value for including plot in report if the page 
  # has not been opened. Before page is opened include_.. is a logical(0) which
  # cannot be used in logical evaluation to inlcude or not and crashes report
  # generation. any(!is.na(.)) returns a FALSE for logical(0) and TRUE if 
  # the variable contains a value. So if the variable is defined return the input value
  # otherwise, if variable does not exist yet return a FALSE for report inclusion.
  include_rhat <- reactive(input$report_rhat)
  include_report_rhat <- reactive({
    ifelse(any(!is.na(include_rhat())), input$report_rhat, FALSE)
  })
  
  include_n_eff <- reactive(input$report_n_eff)
  include_report_n_eff <- reactive({
    ifelse(any(!is.na(include_n_eff())), input$report_n_eff, FALSE)
  })
  
  
  include_se_mean <- reactive(input$report_se_mean)
  include_report_se_mean <- reactive({
    ifelse(any(!is.na(include_se_mean())), input$report_se_mean, FALSE)
  })
  
  output$conditionalUI <- renderUI({
    if(stat() == "\\(\\hat{R}\\)"){
      tagList(
        withMathJax(),
        plotOutput(session$ns("rhatPlot")),
        hr(), 
        checkboxInput(session$ns("report_rhat"), "Include in report?", value = include_report_rhat())
      )
    } else {
      if(stat() == "\\(n_{eff}\\)"){
        tagList(
          withMathJax(),
          plotOutput(session$ns("n_effPlot")),
          hr(), 
          checkboxInput(session$ns("report_n_eff"), "Include in report?", value = include_report_n_eff())
        )
      } else {
        if(stat() == "\\(\\text{se}_{mean} \\text{ / } \\textit{sd}\\)"){
          tagList(
            withMathJax(),
            plotOutput(session$ns("se_meanPlot")),
            hr(), 
            checkboxInput(session$ns("report_se_mean"), "Include in report?", value = include_report_se_mean())
          )
        }
      }
    }
  })
  
  
  # 
  # 
  
  
  # 
  # 
  # output$rhat <- renderText({
  #   
  #   bad_rhat <- rownames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "Rhat"] > reactive(input$rhat_threshold)()]
  #   bad_rhat <- bad_rhat[!is.na(bad_rhat)]
  #   rhatWarning <- paste0("The following parameters have an Rhat value above ", 
  #                         reactive(input$rhat_threshold)(), ":<br>",
  #                         paste(bad_rhat, collapse = ", "))
  #   
  #   if(length(bad_rhat) < 1){
  #     paste0("No parameters have an Rhat value above ", reactive(input$rhat_threshold)(), ".")
  #   } else {
  #     rhatWarning
  #   }
  # })
  # 
  # output$n_eff <- renderText({
  #   
  #   bad_n_eff <- rownames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "n_eff"] / ((shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter- shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) * shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain) < 
  #                                        (reactive(input$n_eff_threshold)() / 100)]
  #   bad_n_eff <- bad_n_eff[!is.na(bad_n_eff)]
  #   n_effWarning <- paste0("The following parameters have an effective sample size less than ", 
  #                          reactive(input$n_eff_threshold)(), "% of the total sample size:<br>",
  #                          paste(bad_n_eff, collapse = ", "))
  #   
  #   if(length(bad_n_eff) < 1){
  #     paste0("No parameters have an effective sample size less than ",
  #                 reactive(input$n_eff_threshold)(), "% of the total sample size.")
  #   } else {
  #     n_effWarning
  #   }
  # })
  # 
  # 
  # output$se_mean <- renderUI({
  #   
  #   bad_se_mean <- rownames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "se_mean"] / shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "sd"] > 
  #                                          (reactive(input$mcse_threshold)() / 100)]
  #   bad_se_mean <- bad_se_mean[!is.na(bad_se_mean)]
  #   se_meanWarning <- paste0("The following parameters have a Monte Carlo standard error greater than ",
  #                            reactive(input$mcse_threshold)(), "% of the posterior standard deviation:<br>",
  #                            paste(bad_se_mean, collapse = ", "))
  #   
  #   if(length(bad_se_mean) < 1){
  #     HTML(paste0("<div style='background-color:lightblue; color:black; 
  #                 padding:5px; opacity:.3'>",
  #                 "No parameters have a standard error greater than ", 
  #                 reactive(input$mcse_threshold)(), "% of the posterior standard deviation.", 
  #                 "</div>"))
  #   } else {
  #     HTML(paste0("<div style='background-color:red; color:white; 
  #                 padding:5px; opacity:.3'>",
  #                 se_meanWarning, "</div>"))
  #   }
  # })
  # 
  # return(reactive({
  #   list("rhatPlot" = plotOut_rhat(),
  #        "n_effPlot" = plotOut_n_eff(),
  #        "se_meanPlot" = plotOut_se_mean())
  # }))
  # 

  
  
# Did not find a functioning better system yet for creating the correct list output. Should exist....
  return(reactive({

    if(include_report_rhat() == FALSE & include_report_n_eff() == FALSE & include_report_se_mean() == FALSE){
      list(
        "rhatPlot" = NULL,
        "n_effPlot" = NULL,
        "se_meanPlot" = NULL
      )
    } else {
      if(include_report_rhat() == TRUE & include_report_n_eff() == FALSE & include_report_se_mean() == FALSE){
        list(
          "rhatPlot" = plotOut_rhat(),
          "n_effPlot" = NULL,
          "se_meanPlot" = NULL
        )
      } else {
        if(include_report_rhat() == FALSE & include_report_n_eff() == TRUE & include_report_se_mean() == FALSE){
          list(
            "rhatPlot" = NULL,
            "n_effPlot" = plotOut_n_eff(),
            "se_meanPlot" = NULL
          )
        } else {
          if(include_report_rhat() == FALSE & include_report_n_eff() == FALSE & include_report_se_mean() == TRUE){
            list(
              "rhatPlot" = NULL,
              "n_effPlot" = NULL,
              "se_meanPlot" = plotOut_se_mean()
            )
          } else {
            if(include_report_rhat() == TRUE & include_report_n_eff() == TRUE & include_report_se_mean() == FALSE){
              list(
                "rhatPlot" = plotOut_rhat(),
                "n_effPlot" = plotOut_n_eff(),
                "se_meanPlot" = NULL
              )
            } else {
              if(include_report_rhat() == TRUE & include_report_n_eff() == FALSE & include_report_se_mean() == TRUE){
                list(
                  "rhatPlot" = plotOut_rhat(),
                  "n_effPlot" = NULL,
                  "se_meanPlot" = plotOut_se_mean()
                )
              } else {
                if(include_report_rhat() == FALSE & include_report_n_eff() == TRUE & include_report_se_mean() == TRUE){
                  list(
                    "rhatPlot" = NULL,
                    "n_effPlot" = plotOut_n_eff(),
                    "se_meanPlot" = plotOut_se_mean()
                  )
                } else {
                  if( include_report_rhat() == TRUE & include_report_n_eff() == TRUE & include_report_se_mean() == TRUE){
                    list(
                      "rhatPlot" = plotOut_rhat(),
                      "n_effPlot" = plotOut_n_eff(),
                      "se_meanPlot" = plotOut_se_mean()
                    )
                  }
                  
                }
              }
            }
            
          }
        }
      }
    }

    # list(
    #   "rhatPlot" = ifelse(include_report_rhat() == TRUE, plotOut_rhat(), NULL),
    #   "n_effPlot" = ifelse(include_report_n_eff() == TRUE, plotOut_n_eff(), NULL),
    #   "se_meanPlot" = ifelse(include_report_se_mean() == TRUE, plotOut_se_mean(), NULL)
    # )
    # list(
    #   if(include_report_rhat() == TRUE) {"rhatPlot" = plotOut_rhat()} else {"rhatPlot" = NULL},
    #   if(include_report_n_eff() == TRUE) {"n_effPlot" = plotOut_n_eff()} else {"n_effPlot" = NULL},
    #   if(include_report_se_mean() == TRUE) {"se_meanPlot" = plotOut_se_mean()} else {"se_meanPlot" = NULL}
    # )
    
    
  }))
  
}
