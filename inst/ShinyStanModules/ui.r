# utilities
source("MODULES/UTILS/plotOptions.R", local = TRUE)
source("MODULES/UTILS/report.R", local = TRUE)

# home tab
source("MODULES/HOME/homepage.R", local = TRUE)
source("MODULES/HOME/warnings.r", local = TRUE)

# diagnoses tab
source("MODULES/DIAGNOSE/diagnoseHomepage.R", local = TRUE)

source("MODULES/DIAGNOSE/divergentScatter.r", local = TRUE)
source("MODULES/DIAGNOSE/divergentTransitions.r", local = TRUE)
source("MODULES/DIAGNOSE/energy.r", local = TRUE)
source("MODULES/DIAGNOSE/treedepth.r", local = TRUE)
source("MODULES/DIAGNOSE/stepSize.r", local = TRUE)
source("MODULES/DIAGNOSE/parallelCoordinates.r", local = TRUE)
source("MODULES/DIAGNOSE/pairs.r", local = TRUE)
source("MODULES/DIAGNOSE/acceptance.r", local = TRUE)

source("MODULES/DIAGNOSE/tracePlot.r", local = TRUE)
source("MODULES/DIAGNOSE/rankPlot.r", local = TRUE)
source("MODULES/DIAGNOSE/rhat_n_eff_se_mean.r", local = TRUE)
source("MODULES/DIAGNOSE/autoCorrelation.r", local = TRUE)

source("MODULES/DIAGNOSE/statsTableHMC.r", local = TRUE)
source("MODULES/DIAGNOSE/rhat_n_eff_se_mean_stats.r", local = TRUE)

# estimate tab
source("MODULES/ESTIMATE/estimateHomepage.R", local = TRUE)

source("MODULES/ESTIMATE/visualEstimate.R", local = TRUE)
source("MODULES/ESTIMATE/scatterPlot.R", local = TRUE)
source("MODULES/ESTIMATE/densityPlot.R", local = TRUE)
source("MODULES/ESTIMATE/histogramPlot.R", local = TRUE)
source("MODULES/ESTIMATE/intervalsPlot.R", local = TRUE)
source("MODULES/ESTIMATE/areasPlot.R", local = TRUE)

source("MODULES/ESTIMATE/numericalEstimate.R", local = TRUE)
source("MODULES/ESTIMATE/summaryTable.R", local = TRUE)
source("MODULES/ESTIMATE/summaryTableLatex.R", local = TRUE)

# more tab
source("MODULES/MORE/about.R", local = TRUE)
source("MODULES/MORE/modelCode.R", local = TRUE)
source("MODULES/MORE/help.R", local = TRUE)

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
