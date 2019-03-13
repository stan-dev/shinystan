areasPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3, h5("Point estimate")),
        column(width = 4, h5("Parameter")),
        column(width = 4, h5("Posterior interval"))
      ),
      fluidRow(
        column(width = 3,
               radioButtons(
                 inputId = ns("param_plot_point_est"),
                 label = NULL,
                 choices = c("Median", "Mean"),
                 selected = "Median",
                 inline = TRUE
               )),
        column(
          width = 4,
          selectizeInput(
            inputId = ns("diagnostic_param"),
            label = NULL,
            multiple = TRUE,
            choices = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names,
            selected = if(length(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names) > 9) shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1:10] else shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names
          )
        ),
        column(
          width = 4,
          sliderInput(
            inputId = ns("param_plot_ci_level"),
            label = NULL,
            width = "75%",
            ticks = FALSE,
            min = 50,
            max = 95,
            value = 50,
            step = 5,
            post = "%"
          )
        )
      )
    ),
    plotOutput(ns("plot1"))
  )
}

areasPlot <- function(input, output, session){
  
  param <- reactive(input$diagnostic_param)
  pointEstimate <- reactive(input$param_plot_point_est)
  interval <- reactive(input$param_plot_ci_level)
  
  output$plot1 <- renderPlot({
    
    color_scheme_set("blue")
    validate(
      need(length(param()) > 0, "Select at least one parameter.")
    )
    mcmc_areas(
      shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ],
      pars = param(),
      point_est = tolower(pointEstimate()),
      prob = interval()/100
    )
  })
}