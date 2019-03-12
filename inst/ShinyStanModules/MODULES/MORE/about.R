aboutUI <- function(id){

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
    div(
      style = "text-align: center; margin-top: 100px;",
      h5(paste("Version:", sso@misc$sso_version)),
      br(),
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
      br(),br(),
      h6("Author"),
      helpText(style = "font-size: 12px;", "Jonah Gabry"),
      br(),
      h6(style = "font-size: 12px;", "Contributors"),
      helpText(style = "font-size: 12px;", 
               HTML("Michael Andreae<br/>
Michael Betancourt<br/>
                Bob Carpenter<br/>
                Yuanjun Gao<br/>
                Andrew Gelman<br/>
                Ben Goodrich<br/>
                Daniel Lee<br/>
                Dongying Song<br/>
                Rob Trangucci ")),
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
  )
}


about <- function(input, output, session){
  
}