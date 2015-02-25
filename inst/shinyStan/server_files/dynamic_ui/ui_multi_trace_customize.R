output$ui_multi_trace_customize <- renderUI({
  my_palette <- "Default"
  my_rect <- "Warmup"
  my_rect_alpha <- 0.15
  my_layout <- "Long"

  #   if (input$user_multi_trace_customize == TRUE) {
  #     ok <- exists("shinystan_settings_multi_trace")
  #     validate(need(ok == TRUE, message = "Sorry, can't find any user multi_trace plot settings."))
  #     user_multi_trace <- shinystan_settings_multi_trace
  #     my_palette <- user_multi_trace$palette
  #     my_rect <- user_multi_trace$rect
  #     my_rect_alpha <- user_multi_trace$rect_alpha
  #     my_layout <- user_multi_trace$layout
  #   }

  absolutePanel(id = "controls_multi_trace", class = "panel panel-default hvr-glow", fixed = TRUE,
  top = 400, right = 20, width = 270,
  draggable = TRUE,
  wellPanel(style = "background-color: #222222 ;",
    h4(style = "color: #428bca; ", strong("shinyStan customize")),
    hr(),
    bsCollapse(
      bsCollapsePanel(title = "Options", id = "multi_trace_options_collapse",
                      numericInput("multi_trace_chain", label = "Chain (0 = all chains)", min = 0, max = object@nChains, step = 1, value = 0),
#                       checkboxInput("multi_trace_warmup", label = "Include warmup", TRUE),
                      radioButtons("multi_trace_layout", label = "Layout",
                                   choices = c("Long", "Grid"), selected = my_layout, inline = TRUE)
# selectInput("multi_trace_layout", label = "Layout",
#              choices = c("Long", "Grid"), selected = my_layout, size = 2, selectize = FALSE)

      ),
      bsCollapsePanel(title = "Aesthetics", id = "multi_trace_colors_collapse",
                      selectizeInput("multi_trace_palette", "Color palette", choices = c("Default", "Brewer (spectral)", "Rainbow", "Gray"), selected = my_palette),
#                       radioButtons("multi_trace_rect", label = "Shading", choices = c("None", "Samples", "Warmup"), selected = my_rect, inline = FALSE),
                      selectInput("multi_trace_rect", label = "Shading", choices = c("None", "Samples", "Warmup"), selected = my_rect, size = 2, selectize = FALSE),
                      sliderInput("multi_trace_rect_alpha", "Shading opacity", value = my_rect_alpha, min = 0, max = 1, step = 0.01)

      )
    ),
    hr(),
    downloadButton("download_multi_trace", "Save as ggplot2 object")
  )
)

})
