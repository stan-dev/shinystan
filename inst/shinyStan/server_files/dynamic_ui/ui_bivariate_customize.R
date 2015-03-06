output$ui_bivariate_customize <- renderUI({
  my_ellipse_lev <- "None"
  my_pt_size     <- 3.5
  my_pt_shape    <- 10
  my_pt_color    <- "firebrick"
  my_ellipse_color    <- "black"
  my_ellipse_lty      <- 1
  my_ellipse_lwd      <- 1
  my_ellipse_alpha    <- 1
  my_lines_color <- "gray"
  
  alpha_calc_pt <- function(N) {
    if (N <= 100) return(1)
    else if (N <= 200) return(0.75)
    else if (N >= 1500) return(0.15) 
    else 1 - pnorm(N/1500)
  }
  alpha_calc_lines <- function(N) {
    if (N < 50) return(0.5)
    else if (N > 1000) return(0.15) 
    else 1 - pnorm(N/750)
  }
  
  my_pt_alpha <- alpha_calc_pt(nIter)
  
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
  
  bsCollapse(id = "bivariate_collapse_all",
    bsCollapsePanel(title = "View Options", id = "bivariate_collapse",
                    bsCollapse(
                      bsCollapsePanel(title = span(style = "color:#428bca;","Points"), id = "bivariate_points_collapse",
                                      fluidRow(
                                        column(4, selectInput("bivariate_pt_color", strong("Color"), choices = colors(), selected = my_pt_color)),
                                        column(2, offset = 1, numericInput("bivariate_pt_size", strong("Size"), value = my_pt_size, min = 0, max = 10, step = 0.5)),
                                        column(2, numericInput("bivariate_pt_shape", strong("Shape"), value = my_pt_shape, min = 1, max = 10, step = 1)),
                                        column(2, sliderInput("bivariate_pt_alpha", strong("Opacity"), value = my_pt_alpha, min = 0, max = 1, step = 0.01, ticks = FALSE))
                                      )
                      ),
                      bsCollapsePanel(title = span(style = "color:#428bca;","Ellipse"), id = "bivariate_ellipse_collapse",
                                      radioButtons("bivariate_ellipse_lev", label = "", selected = my_ellipse_lev, inline = TRUE,
                                                   choices = list("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95, "99%" = 0.99)),
                                      fluidRow(
                                        column(4, selectInput("bivariate_ellipse_color", strong("Color"), choices = colors(), selected = my_ellipse_color)),
                                        column(2, offset = 1, numericInput("bivariate_ellipse_lwd", strong("Size"), value = my_ellipse_lwd, min = 0, max = 5, step = 0.5)),
                                        column(2, numericInput("bivariate_ellipse_lty", strong("Shape"), value = my_ellipse_lty, min = 1, max = 6, step = 1)),
                                        column(2, sliderInput("bivariate_ellipse_alpha", strong("Opacity"), value = my_ellipse_alpha, min = 0, max = 1, step = 0.01, ticks = FALSE))
                                      )
                      ),
                      bsCollapsePanel(title = span(style = "color:#428bca;","Lines"), id = "bivariate_lines_collapse",
                                      radioButtons("bivariate_lines", label = "", choices = c("Hide" = "hide", "Behind" = "back", "In front" = "front"), selected = "back", inline = TRUE),
                                      fluidRow(
                                        column(4, selectInput("bivariate_lines_color", strong("Color"), choices = colors(), selected = my_lines_color)),
                                        column(2, offset = 1, sliderInput("bivariate_lines_alpha", label = strong("Opacity"), value = alpha_calc_lines(nIter), min = 0, max = 1, step = 0.01, ticks = FALSE))
                                      )
                      )
                    ),
                    hr(),
                    downloadButton("download_bivariate", "Save as ggplot2 object")
    )
  )
})
