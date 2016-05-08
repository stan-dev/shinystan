navbarMenu(
  title = "More",
  
  #### model code ####
  tabPanel(
    title = "Model Code",
    source_ui("model_code.R")
  ),
  
  #### notepad ####
  tabPanel(
    title = "Notepad",
    source_ui("notepad.R")
  ),
  
  #### about ####
  tabPanel(
    title = "About",
    logo_and_name(),
    div(
      style = "margin-top: 75px;",
      source_ui("about.R")
    )
  ),
  
  #### glossary ####
  tabPanel(
    title = "Glossary",
    div(
      style = "background-color: white;",
      h1(style = "text-align: center;", "Glossary"),
      source_ui("glossary.R"),
      hr(),
      stan_manual()
    )
  ),
  
  #### help ####
  tabPanel(
    title = "Help",
    h1(style = "text-align: center;", "Help"),
    source_ui("help.R")
  )
)