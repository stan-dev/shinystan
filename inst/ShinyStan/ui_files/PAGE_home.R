tagList(
  logo_and_name(),
  div(class = "home-links",
      div(id = "model-name",
          br(),
          h2("Model:"),
          h4(.model_name))),
  br(), br(), br(), br(),
  includeHTML("html/home_page_links.html")
)