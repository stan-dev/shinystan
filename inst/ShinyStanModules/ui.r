# home tab
source("MODULES/HOME/homepage.r", local = TRUE)
source("MODULES/HOME/warnings.r", local = TRUE)

# diagnoses tab
source("MODULES/DIAGNOSE/diagnoseHomepage.r", local = TRUE)

source("MODULES/DIAGNOSE/divergentScatter.r", local = TRUE)
source("MODULES/DIAGNOSE/divergentTransitions.r", local = TRUE)
source("MODULES/DIAGNOSE/energy.r", local = TRUE)
source("MODULES/DIAGNOSE/treedepth.r", local = TRUE)
source("MODULES/DIAGNOSE/stepSize.r", local = TRUE)
source("MODULES/DIAGNOSE/parallelCoordinates.r", local = TRUE)
source("MODULES/DIAGNOSE/pairs.r", local = TRUE)
source("MODULES/DIAGNOSE/acceptance.r", local = TRUE)

source("MODULES/DIAGNOSE/tracePlot.r", local = TRUE)
source("MODULES/DIAGNOSE/rhat_n_eff_se_mean.r", local = TRUE)
source("MODULES/DIAGNOSE/autoCorrelation.r", local = TRUE)

source("MODULES/DIAGNOSE/statsTableHMC.r", local = TRUE)
source("MODULES/DIAGNOSE/rhat_n_eff_se_mean_stats.r", local = TRUE)
source("MODULES/DIAGNOSE/autoCorrelationStats.r", local = TRUE)

# estimate tab
source("MODULES/ESTIMATE/estimateHomepage.r", local = TRUE)

source("MODULES/ESTIMATE/visualEstimate.r", local = TRUE)
source("MODULES/ESTIMATE/scatterPlot.r", local = TRUE)
source("MODULES/ESTIMATE/densityPlot.r", local = TRUE)
source("MODULES/ESTIMATE/histogramPlot.r", local = TRUE)
source("MODULES/ESTIMATE/intervalsPlot.r", local = TRUE)
source("MODULES/ESTIMATE/areasPlot.r", local = TRUE)

source("MODULES/ESTIMATE/numericalEstimate.r", local = TRUE)
source("MODULES/ESTIMATE/summaryTable.r", local = TRUE)

# more tab
source("MODULES/MORE/about.r", local = TRUE)
source("MODULES/MORE/modelCode.r", local = TRUE)
source("MODULES/MORE/help.r", local = TRUE)
source("MODULES/MORE/glossary.r", local = TRUE)
# source("MODULES/MORE/test.r", local = TRUE)
# report
source("MODULES/REPORT/report2.r", local = TRUE)
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
