output$ui_bivariate_customize <- renderUI({
  my_ellipse_lev <- "None"
  my_pt_alpha <- 0.10
  my_pt_size     <- 3.5
  my_pt_shape    <- 10
  my_pt_color    <- "firebrick"
  my_ellipse_color    <- "black"
  my_ellipse_lty      <- 1
  my_ellipse_lwd      <- 1
  my_ellipse_alpha    <- 1

#   if (input$user_contour_customize == TRUE) {
#     ok <- exists("shinystan_settings_contour")
#     validate(need(ok == TRUE, message = "Sorry, can't find any user bivariate plot settings."))
#     user_contour <- shinystan_settings_contour
#     ops <- user_contour[["ops"]]
#
#     my_ellipse_lev <- ops$ellipselev
#     my_pt_alpha    <- ops$pt_alpha
#     my_pt_size     <- ops$pt_size
#     my_pt_shape    <- ops$pt_shape
#     my_pt_color    <- ops$pt_color
#     my_ellipsecolor    <- ops$ellipsecolor
#     my_ellipselty      <- ops$ellipselty
#     my_ellipselwd      <- ops$ellipselwd
#     my_ellipsealpha    <- ops$ellipsealpha
#   }


bsCollapse(
  bsCollapsePanel(title = "View Options", id = "bivariate_collapse",
                  fluidRow(
                    column(12,
                           fluidRow(
                             column(2,
                                    radioButtons("bivariate_ellipse_lev", label = h5("Ellipse"), selected = my_ellipse_lev, inline = FALSE,
                                                 choices = list("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95, "99%" = 0.99))
                             ),
                             column(10,
                                    fluidRow(
                                      column(4, selectInput("bivariate_pt_color", h5("Point Color"), choices = colors(), selected = my_pt_color)),
                                      column(3, numericInput("bivariate_pt_size", h5("Size"), value = my_pt_size, min = 0, max = 10, step = 0.5)),
                                      column(3, numericInput("bivariate_pt_shape", h5("Shape"), value = my_pt_shape, min = 1, max = 10, step = 1)),
                                      column(2, sliderInput("bivariate_pt_alpha", h5("Opacity"), value = my_pt_alpha, min = 0, max = 1, step = 0.01, ticks = FALSE))
                                    ),
                                    fluidRow(
                                      column(4, selectInput("bivariate_ellipse_color", h5("Ellipse Color"), choices = colors(), selected = my_ellipse_color)),
                                      column(3, numericInput("bivariate_ellipse_lwd", h5("Size"), value = my_ellipse_lwd, min = 0, max = 5, step = 0.5)),
                                      column(3, numericInput("bivariate_ellipse_lty", h5("Shape"), value = my_ellipse_lty, min = 1, max = 6, step = 1)),
                                      column(2, sliderInput("bivariate_ellipse_alpha", h5("Opacity"), value = my_ellipse_alpha, min = 0, max = 1, step = 0.01, ticks = FALSE))
                                    ))
                           )
                    )
                  ),
                  hr(),
                  downloadButton("download_bivariate", "Save as ggplot2 object")
  )
)
})
