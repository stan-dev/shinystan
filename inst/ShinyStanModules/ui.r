# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________
tagList(
  tags$head(
    tags$script("src"="func.js")),
  tags$noscript(
    style = "color: orange; font-size: 30px; text-align: center;",
    "Please enable JavaScript to use ShinyStan."
  ), 
  shinyjs::useShinyjs(),
  includeCSS("css/ShinyStan.css"),
  
  navbarPage(
    tags$button(
      id = 'save_and_close_button',
      type = "button",
      class = "btn action-button",
      onclick = "window.close();",
      "Close"
    ), 
    id = "nav",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinythemes::shinytheme("flatly"),
    windowTitle = "ShinyStan",
    
    
    #### HOME ####
    tabPanel(
      title = strong(style = "color: #B2011D;", "ShinyStan"),
      value = "home",
      homepageUI("homepage")
    ),
    
    #### DIAGNOSE ####
    tabPanel(
      title = "Diagnose",
      icon = icon("medkit"),
      diagnoseUI("diagnoseHomepage")
    ),
    
    #### ESTIMATE ####
    tabPanel(
      title = "Estimate",
      icon = icon("stats", lib = "glyphicon"),
      estimateUI("estimateHomepage")
    ),
    navbarMenu(
      title = "More",
      
      #### model code ####
      tabPanel(
        title = "Model Code",
        modelCodeUI("modelCode")
      ),
      
      #### about ####
      tabPanel(
        title = "About",
        aboutUI("about")
      ),
      
    tabPanel(
      title = "Report",
      reportUI("report")
    ),
      #### glossary ####
      tabPanel(
        title = "Glossary",
        glossaryUI("glossary")
      ),
      
      #### help ####
      tabPanel(
        title = "Help",
        helpUI("help")
    )
    )
    
  ) # End navbarPage
) # End tagList

# End shinyUI -------------------------------------------------------------
# -------------------------------------------------------------------------
