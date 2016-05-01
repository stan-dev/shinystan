# 
# output$ui_multitrace_customize <- renderUI({
#   my_palette <- "Default"
#   my_rect <- "Warmup"
#   my_rect_alpha <- 0.15
#   my_layout <- "Long"
#   
#   absolutePanel(id = "controls_multitrace", 
#                 class = "draggable_controls",
#                 fixed = TRUE,
#                 # top = 175, right = 20, width = 270,
#                 top = 300, right = 20, width = 270,
#                 draggable = TRUE,
#                 shinyjs::hidden(
#                   div(id = "multitrace_options",
#                       wellPanel(
#                         class = "optionswell",
#                         strongBig("Parameter estimates"),
#                         hr(class = "hroptions"),
#                         selectInput("multitrace_options_display", label = strongBig("Control"),
#                                     choices = c("Options", "Aesthetics"),
#                                     selected = "Options", width = "100%"),
#                         conditionalPanel(condition = "input.multitrace_options_display == 'Options'",
#                                          numericInput("multitrace_chain", label = "Chain (0 = all chains)", min = 0, max = object@nChains, step = 1, value = 0),
#                                          radioButtons("multitrace_layout", label = "Layout",
#                                                       choices = c("Long", "Grid"), selected = my_layout, inline = TRUE),
#                                          downloadButton("download_multitrace", "Save as ggplot2 object")
#                         ),
#                         conditionalPanel(condition = "input.multitrace_options_display == 'Aesthetics'",
#                                          selectizeInput("multitrace_palette", "Color palette", choices = c("Default", "Brewer (spectral)", "Rainbow", "Gray"), selected = my_palette),
#                                          # selectInput("multitrace_rect", label = "Shading", choices = c("None", "Samples", "Warmup"), selected = my_rect, size = 2, selectize = FALSE),
#                                          radioButtons("multitrace_rect", label = "Shading", choices = c("None", "Samples", "Warmup"), selected = my_rect, inline = TRUE),
#                                          sliderInput("multitrace_rect_alpha", "Shading opacity", value = my_rect_alpha, min = 0, max = 1, step = 0.01)
#                         )
#                       )
#                   )
#                 )
#   )
# })
