div(
  style = "text-align: center; margin-top: 100px;",
  shinystan_version(),
  br(),br(),
  a(
    style = "font-size: 16px;",
    strong("Stan Development Team"),
    href = "http://mc-stan.org/team/"
  ),
  br(),
  a(
    style = "font-size: 14px;", 
    "mc-stan.org", 
    href = "http://mc-stan.org/"
  ),
  div(
    actionLink(
      inputId = "shinystan_citation_show",
      label = "Show Citation",
      style = "font-size: 12px;",
      class = "aoptions"
    ),
    div(shinyjs::hidden(
      wellPanel(
        id = "citation_div",
        style = "text-align: left;",
        includeHTML("html/citation.html")
      )
    ))
  ),
  br(),br(),
  h6("Author"),
  helpText(style = "font-size: 12px;", "Jonah Gabry"),
  br(),
  h6(style = "font-size: 12px;", "Contributors"),
  helpText(style = "font-size: 12px;", includeHTML("html/contribs.html")),
  br(),
  h6("Logo"),
  helpText(
    style = "font-size: 12px;",
    a(
      href = "http://mc-stan.org/team/", 
      "Michael Betancourt"
    )
  ),
  br(),
  h6("Shiny"),
  helpText(
    style = "font-size: 12px;",
    "ShinyStan is powered by the",
    a(
      href = "http://shiny.rstudio.com", 
      "Shiny web application framework"
    ),
    "(RStudio)"
  ),
  br(),
  h6("Source code"),
  a(
    style = "color: #190201;",
    href = "http://github.com/stan-dev/shinystan",
    target = "_blank",
    tags$i(class = "fa fa-github fa-3x")
  )
)
