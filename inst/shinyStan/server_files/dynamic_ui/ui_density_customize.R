output$ui_density_customize <- renderUI({

  my_point_est <- "None"
  my_fill_color <- "gray35"
  my_line_color <- "lightgray"
  #   my_y_breaks <- "None"
  my_x_breaks <- "Some"
  my_CI <- "None"

  #   if (input$user_dens_customize == TRUE) {
  #     ok <- exists("shinystan_settings_density")
  #     validate(need(ok == TRUE, message = "Sorry, can't find any user density settings."))
  #     user_dens <- shinystan_settings_density
  #     my_point_est <- user_dens$point_est
  #     my_fill_color <- user_dens$fill_color
  #     my_line_color <- user_dens$line_color
  #     my_CI <- user_dens$CI
  #     my_y_breaks <- user_dens$y_breaks
  #     my_x_breaks <- user_dens$x_breaks
  #   }

  bsCollapse(
    bsCollapsePanel(title = "View Options", id = "density_collapse",
                    fluidRow(
                      column(3,numericInput("dens_chain", label = h5(style = "color: white;", "Chain (0 = all)"), min = 0, max = object@nChains, step = 1, value = 0)),
                      column(3, conditionalPanel(condition = "input.dens_chain == 0",
                                                 radioButtons("dens_chain_split", label = h5("All chains"), choices = c("Together", "Separate"), selected = "Together", inline = FALSE))),
                      column(2, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_point_est", h5("Pt est"), choices = c("None","Mean","Median","MAP"), selected = my_point_est, selectize = FALSE, size = 4))),
                      column(2, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_ci", h5("CI %"), choices = c("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95), selected = my_CI, selectize = FALSE, size = 4))),
                      column(2, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_x_breaks", h5("x breaks"), choices = c("None", "Some", "Many"), selected = my_x_breaks, selectize = FALSE, size = 3)))
                    ),
                    fluidRow(
                      column(3, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_fill_color", h5(style = "color: white;", "Fill color"), choices = colors(), selected = my_fill_color, selectize = TRUE))),
                      column(3, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_line_color", h5(style = "color: white;", "Line color"), choices = colors(), selected = my_line_color, selectize = TRUE)))
                    ),
                    hr(),
                    downloadButton("download_density", "Save as ggplot2 object")
    )
  )
})
