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
      ),
      fluidRow(align = "right",
               uiOutput(ns("conditionalPlotOptions"))
      )
    ),
    uiOutput(ns("conditionalUI"))
    
  )
}


rhat_n_eff_se_mean <- function(input, output, session){
  
  visualOptions_rhat <- callModule(plotOptions, "options_rhat")  
  visualOptions_n_eff <- callModule(plotOptions, "options_n_eff")  
  visualOptions_se_mean <- callModule(plotOptions, "options_se_mean")  
  stat <- reactive({input$selectStat})
  
  observe({
    toggle("caption_rhat", condition = input$showCaption_rhat)
  })
  observe({
    toggle("caption_n_eff", condition = input$showCaption_n_eff)
  })
  observe({
    toggle("caption_se_mean", condition = input$showCaption_se_mean)
  })
  
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
  
  output$conditionalPlotOptions <- renderUI({
    if(stat() == "\\(\\hat{R}\\)"){
      plotOptionsUI(session$ns("options_rhat"))
    } else {
      if(stat() == "\\(n_{eff}\\)"){
        plotOptionsUI(session$ns("options_n_eff"))
      } else {
        if(stat() == "\\(\\text{se}_{mean} \\text{ / } \\textit{sd}\\)"){
          plotOptionsUI(session$ns("options_se_mean"))
        }
      }
    }
  })
  
  
  # first plot functions than renderPlots using functions to enable pushing back plots out of module
  plotOut_rhat <- function(){
    mcmc_rhat_hist(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "Rhat"])
  }
  
  plotOut_n_eff <- function(){
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
    ggplot(data = se_sd_table, mapping = aes_(x = ~value, color = ~rating, fill = ~rating)) +
      geom_histogram(size = 0.25, na.rm = TRUE) +
      labs(x = expression(MC[se] / sd),y = NULL) +
      bayesplot:::dont_expand_y_axis(c(0.005, 0)) + bayesplot_theme_get() +
      yaxis_title(FALSE) + yaxis_text(FALSE) + yaxis_ticks(FALSE) +
      theme(legend.position = "none")
  }
  
  
  output$rhatPlot <- renderPlot({
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions_rhat()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_rhat()$theme)))) 
    out <- plotOut_rhat()
    bayesplot_theme_set(save_old_theme)
    suppressMessages(print(out)) # hide 'bins = 30' message ggplot
  })
  
  output$n_effPlot <- renderPlot({
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions_n_eff()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_n_eff()$theme)))) 
    out <- plotOut_n_eff()
    bayesplot_theme_set(save_old_theme)
    suppressMessages(print(out)) # hide 'bins = 30' message ggplot
  })
  
  output$se_meanPlot <- renderPlot({
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions_se_mean()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_se_mean()$theme)))) 
    out <- plotOut_se_mean()
    bayesplot_theme_set(save_old_theme)
    suppressMessages(print(out)) # hide 'bins = 30' message ggplot
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
  
  
  output$rhat <- renderText({
    
    bad_rhat <- rownames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "Rhat"] > reactive(input$rhat_threshold)()]
    bad_rhat <- bad_rhat[!is.na(bad_rhat)]
    rhatWarning <- paste0("The following parameters have an Rhat value above ",
                          reactive(input$rhat_threshold)(), ": ",
                          paste(bad_rhat, collapse = ", "))
    
    if(length(bad_rhat) < 1){
      paste0("No parameters have an Rhat value above ", reactive(input$rhat_threshold)(), ".")
    } else {
      rhatWarning
    }
  })
  
  output$n_eff <- renderText({
    
    bad_n_eff <- rownames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "n_eff"] / ((shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter- shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) * shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain) <
                                                                            (reactive(input$n_eff_threshold)() / 100)]
    bad_n_eff <- bad_n_eff[!is.na(bad_n_eff)]
    n_effWarning <- paste0("The following parameters have an effective sample size less than ",
                           reactive(input$n_eff_threshold)(), "% of the total sample size: ",
                           paste(bad_n_eff, collapse = ", "))
    
    if(length(bad_n_eff) < 1){
      paste0("No parameters have an effective sample size less than ",
             reactive(input$n_eff_threshold)(), "% of the total sample size.")
    } else {
      n_effWarning
    }
  })
  
  
  output$se_mean <- renderText({
    
    bad_se_mean <- rownames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "se_mean"] / shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "sd"] >
                                                                              (reactive(input$mcse_threshold)() / 100)]
    bad_se_mean <- bad_se_mean[!is.na(bad_se_mean)]
    se_meanWarning <- paste0("The following parameters have a Monte Carlo standard error greater than ",
                             reactive(input$mcse_threshold)(), "% of the posterior standard deviation: ",
                             paste(bad_se_mean, collapse = ", "))
    
    if(length(bad_se_mean) < 1){
      paste0("No parameters have a standard error greater than ",
                  reactive(input$mcse_threshold)(), "% of the posterior standard deviation.")
    } else {
      se_meanWarning
    }
  })
  
  
  output$conditionalUI <- renderUI({
    if(stat() == "\\(\\hat{R}\\)"){
      tagList(
        withMathJax(),
        plotOutput(session$ns("rhatPlot")),
        textOutput(session$ns("rhat")),
        checkboxInput(session$ns("showCaption_rhat"), "Show/Hide Caption"),
        hidden(
          uiOutput(session$ns("caption_rhat"))
        ),
        hr(), 
        checkboxInput(session$ns("report_rhat"), "Include in report?", value = include_report_rhat())
      )
    } else {
      if(stat() == "\\(n_{eff}\\)"){
        tagList(
          withMathJax(),
          plotOutput(session$ns("n_effPlot")),
          textOutput(session$ns("n_eff")),
          checkboxInput(session$ns("showCaption_n_eff"), "Show/Hide Caption"),
          hidden(
            uiOutput(session$ns("caption_n_eff"))
          ),
          hr(), 
          checkboxInput(session$ns("report_n_eff"), "Include in report?", value = include_report_n_eff())
        )
      } else {
        if(stat() == "\\(\\text{se}_{mean} \\text{ / } \\textit{sd}\\)"){
          tagList(
            withMathJax(),
            plotOutput(session$ns("se_meanPlot")),
            textOutput(session$ns("se_mean")),
            checkboxInput(session$ns("showCaption_se_mean"), "Show/Hide Caption"),
            hidden(
              uiOutput(session$ns("caption_se_mean"))
            ),
            hr(), 
            checkboxInput(session$ns("report_se_mean"), "Include in report?", value = include_report_se_mean())
          )
        }
      }
    }
  })
  
  
  captionOut_rhat <- function(){
    HTML(paste0("This is a histogram of split R-hat values for all the paramters in the model.",
                " Before Stan calculating the potential-scale-reduction statistic R-hat, each chain is split into two halves.",
                " This provides an additional means to detect non-stationarity in the individual chains. ",
                " R-hat vales are used to assess convergence and traditionally R-hat values above 1.1 were considered signs of bad convergence.",
                " For more information see ",
                tags$a('https://mc-stan.org/docs/reference-manual/notation-for-samples-chains-and-draws.html'),
                " or see <a href = 'https://arxiv.org/abs/1903.08008'>Vehtari et al. (2019)</a> for recent developments related to the R-hat statistic. ", 
                " Note that part of these recent developments are recommendations to only use the posterior sample if R-hat is below 1.01."))
  }
  captionOut_n_eff <- function(){
    HTML(paste0("This is a histogram of the effective sample size devided by the total number of itterations for all parameters in the model.",
                " If parameters have a very low ratio of effective sample size per itteration this can be indicative",
                " of a difficult posterior from which to sample for those parameters.",
                " The efficiency of the sampling can sometimes be increased by means of reparametrization of the model."
                ))
  }
  captionOut_se_mean <- function(){
    HTML(paste0("This is a histogram of the ratio of the Monte Carlo standard error of the mean to the posterior standard deviation for the estimated parameters.",
                " The Monte Carlo standard error is related to the accuracy of simulation. The smaller the standard error the closer",
                " the esimate for the parameter is expected to be to the true value.", 
                " The standard deviation is related to the uncertainty we have about the parameter value itself.",
                " We want our simulation uncertainty to be small in comparison to our uncertainty about the parameter itself.",
                " If we are very sure about the parameter and the standard deviation is small, we need many monte carlo itterations",
                " to ensure small standard errors of our estimates.",
                " However, if our posterior standard deviation itself is large, we do not need the same accuracy, hence fewer monte carlo samples are sufficient.",
                " For more information see for instance ",
                tags$a('https://statmodeling.stat.columbia.edu/2007/04/02/markov_chain_mo/')
                ))
  }
  
  output$caption_rhat <- renderUI({
    captionOut_rhat()
  })
  output$caption_n_eff <- renderUI({
    captionOut_n_eff()
  })
  output$caption_se_mean <- renderUI({
    captionOut_se_mean()
  })
  
  

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
          "rhatPlot" = {
            save_old_theme <- bayesplot_theme_get()
            color_scheme_set(visualOptions_rhat()$color)
            bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_rhat()$theme)))) 
            out <- list(plot = plotOut_rhat(), 
                        caption = captionOut_rhat())
            bayesplot_theme_set(save_old_theme)
            out
          },
          "n_effPlot" = NULL,
          "se_meanPlot" = NULL
        )
      } else {
        if(include_report_rhat() == FALSE & include_report_n_eff() == TRUE & include_report_se_mean() == FALSE){
          list(
            "rhatPlot" = NULL,
            "n_effPlot" = {
              save_old_theme <- bayesplot_theme_get()
              color_scheme_set(visualOptions_n_eff()$color)
              bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_n_eff()$theme)))) 
              out <- list(plot = plotOut_n_eff(), 
                          caption = captionOut_n_eff())
              bayesplot_theme_set(save_old_theme)
              out
            },
            "se_meanPlot" = NULL
          )
        } else {
          if(include_report_rhat() == FALSE & include_report_n_eff() == FALSE & include_report_se_mean() == TRUE){
            list(
              "rhatPlot" = NULL,
              "n_effPlot" = NULL,
              "se_meanPlot" = {
                save_old_theme <- bayesplot_theme_get()
                color_scheme_set(visualOptions_se_mean()$color)
                bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_se_mean()$theme)))) 
                out <- list(plot = plotOut_se_mean(), 
                            caption = captionOut_se_mean())
                bayesplot_theme_set(save_old_theme)
                out
              }
            )
          } else {
            if(include_report_rhat() == TRUE & include_report_n_eff() == TRUE & include_report_se_mean() == FALSE){
              list(
                "rhatPlot" = {
                  save_old_theme <- bayesplot_theme_get()
                  color_scheme_set(visualOptions_rhat()$color)
                  bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_rhat()$theme)))) 
                  out <- list(plot = plotOut_rhat(), 
                              caption = captionOut_rhat())
                  bayesplot_theme_set(save_old_theme)
                  out
                },
                "n_effPlot" = {
                  save_old_theme <- bayesplot_theme_get()
                  color_scheme_set(visualOptions_n_eff()$color)
                  bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_n_eff()$theme)))) 
                  out <- list(plot = plotOut_n_eff(), 
                              caption = captionOut_n_eff())
                  bayesplot_theme_set(save_old_theme)
                  out
                },
                "se_meanPlot" = NULL
              )
            } else {
              if(include_report_rhat() == TRUE & include_report_n_eff() == FALSE & include_report_se_mean() == TRUE){
                list(
                  "rhatPlot" = {
                    save_old_theme <- bayesplot_theme_get()
                    color_scheme_set(visualOptions_rhat()$color)
                    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_rhat()$theme)))) 
                    out <- list(plot = plotOut_rhat(), 
                                caption = captionOut_rhat())
                    bayesplot_theme_set(save_old_theme)
                    out
                  },
                  "n_effPlot" = NULL,
                  "se_meanPlot" = {
                    save_old_theme <- bayesplot_theme_get()
                    color_scheme_set(visualOptions_se_mean()$color)
                    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_se_mean()$theme)))) 
                    out <- list(plot = plotOut_se_mean(), 
                                caption = captionOut_se_mean())
                    bayesplot_theme_set(save_old_theme)
                    out
                  }
                )
              } else {
                if(include_report_rhat() == FALSE & include_report_n_eff() == TRUE & include_report_se_mean() == TRUE){
                  list(
                    "rhatPlot" = NULL,
                    "n_effPlot" = {
                      save_old_theme <- bayesplot_theme_get()
                      color_scheme_set(visualOptions_n_eff()$color)
                      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_n_eff()$theme)))) 
                      out <- list(plot = plotOut_n_eff(), 
                                  caption = captionOut_n_eff())
                      bayesplot_theme_set(save_old_theme)
                      out
                    },
                    "se_meanPlot" = {
                      save_old_theme <- bayesplot_theme_get()
                      color_scheme_set(visualOptions_se_mean()$color)
                      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_se_mean()$theme)))) 
                      out <- list(plot = plotOut_se_mean(), 
                                  caption = captionOut_se_mean())
                      bayesplot_theme_set(save_old_theme)
                      out
                    }
                  )
                } else {
                  if( include_report_rhat() == TRUE & include_report_n_eff() == TRUE & include_report_se_mean() == TRUE){
                    list(
                      "rhatPlot" = {
                        save_old_theme <- bayesplot_theme_get()
                        color_scheme_set(visualOptions_rhat()$color)
                        bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_rhat()$theme)))) 
                        out <- list(plot = plotOut_rhat(), 
                                    caption = captionOut_rhat())
                        bayesplot_theme_set(save_old_theme)
                        out
                      },
                      "n_effPlot" = {
                        save_old_theme <- bayesplot_theme_get()
                        color_scheme_set(visualOptions_n_eff()$color)
                        bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_n_eff()$theme)))) 
                        out <- list(plot = plotOut_n_eff(), 
                                    caption = captionOut_n_eff())
                        bayesplot_theme_set(save_old_theme)
                        out
                      },
                      "se_meanPlot" = {
                        save_old_theme <- bayesplot_theme_get()
                        color_scheme_set(visualOptions_se_mean()$color)
                        bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_se_mean()$theme)))) 
                        out <- list(plot = plotOut_se_mean(), 
                                    caption = captionOut_se_mean())
                        bayesplot_theme_set(save_old_theme)
                        out
                      }
                    )
                  }
                  
                }
              }
            }
            
          }
        }
      }
    }

    
  }))
  
}
