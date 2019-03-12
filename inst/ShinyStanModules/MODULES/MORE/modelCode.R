modelCodeUI <- function(id){
  ns <- NS(id)

  tagList(
    div(div(
      img(
        src = "wide_ensemble.png",
        class = "wide-ensemble",
        width = "100%"
      )
    ),
    div(
      style = "margin-top: 25px",
      img(src = "stan_logo.png", class = "stan-logo"),
      div(id = "shinystan-title", "ShinyStan")
    )),
    fluidRow(
      align="center",
      br(), br(),
    tags$textarea(
      id = "user_model_code",
      wrap = "off",
      cols = 80,
      rows = 20,
      sso@model_code
    )
  )
  )
}

modelCode <- function(input, output, session){
  
}
  

