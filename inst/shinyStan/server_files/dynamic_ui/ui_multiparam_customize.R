output$ui_multiparam_customize <- renderUI({

  #   my_show_ci_line <- "yes"
  #   my_color_by_rhat <- "no"
  #   my_show_density <- "no"
  my_show_ci_line <- TRUE
  my_color_by_rhat <- FALSE
  my_show_density <- FALSE
  my_point_est <- "Median"
  my_fill_color <- "gray35"
  my_outline_color <- "gray35"
  my_est_color <- "lightgray"
  my_rhat_palette <- "Oranges"

  #   if (input$user_param_plot_customize == TRUE) {
  #     ok <- exists("shinystan_settings_param_plot")
  #     validate(need(ok == TRUE, message = ""))
  #     user_param_plot <- shinystan_settings_param_plot
  #     my_point_est <- user_param_plot$point_est
  #     my_fill_color <- user_param_plot$fill_color
  #     my_outline_color <- user_param_plot$outline_color
  #     my_est_color <- user_param_plot$est_color
  #   }

  #   if (input$user_param_plot_customize == TRUE) {
  #     ok <- exists("shinystan_settings_param_plot")
  #     validate(need(ok == TRUE, message = "Sorry, can't find any user param_plot settings."))
  #     user_param_plot <- shinystan_settings_param_plot
  #     my_show_density <- user_param_plot$show_density
  #     my_show_ci_line <- user_param_plot$show_ci_line
  #     my_color_by_rhat <- user_param_plot$color_by_rhat
  #   }

  absolutePanel(id = "controls_multiparam", class = "panel panel-default hvr-glow", fixed = FALSE,
    top = 400, right = 20, width = 240,
    draggable = TRUE,
    wellPanel(style = "background-color: #222222;",
              h4(style = "color: #428bca;", strong("shinyStan customize")),
              hr(),
              bsCollapse(# open = "multiparam_options_collapse",
                bsCollapsePanel(title = "Options", id = "multiparam_options_collapse",
                                checkboxInput("param_plot_show_density", label = "Kernal density estimates", value = my_show_density),
                                checkboxInput("param_plot_show_ci_line", label = "95% interval line", value = my_show_ci_line),
                                radioButtons("param_plot_point_est", label = "Point estimate", choices = c("Median", "Mean"), selected = my_point_est, inline = TRUE)
                ),
                bsCollapsePanel(title = "Aesthetics", id = "multiparam_colors_collapse",
                                withMathJax(),
                                checkboxInput("param_plot_color_by_rhat", label = "Color point est. by \\(\\hat{R}\\)", value = my_color_by_rhat),
                                selectInput("param_plot_fill_color", h6("Density/CI color"), choices = colors(), selected = my_fill_color, selectize = TRUE),
                                selectInput("param_plot_outline_color", h6("Outline color"), choices = colors(), selected = my_outline_color, selectize = TRUE),
                                conditionalPanel(condition = "input.param_plot_color_by_rhat == false",
                                                 selectInput("param_plot_est_color", h6("Point est. color"), choices = colors(), selected = my_est_color, selectize = TRUE)),
                                conditionalPanel(condition = "input.param_plot_color_by_rhat == true",
                                                 selectInput("param_plot_rhat_palette", h6("Rhat palette"), choices = c("Blues", "Grays", "Greens", "Oranges", "Purples", "Reds"), selected = my_rhat_palette, selectize=TRUE))
                )
              ),
              hr(),
              downloadButton("download_multiparam_plot", "Save as ggplot2 object")
    )
  )

})
