library(shiny)
library(bayesplot)
sso <- shinystan::eight_schools

plotModuleUI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("Button"), label = "generate"),
    sliderInput(ns("slider"), "draw N", min= 2,max = 10, value = 5),
    plotOutput(ns("plot1"))
  )
}

plotModule <- function(input, output, session){
  
 n <- reactive({input$slider})
  
    observeEvent(input$Button, {
  output$plot1 <- renderPlot({
    plotOut(n = n())
    })
  })
    
    
  plotOut <- function(n){
    
    mcmc_acf(sso@posterior_sample, "mu",lags = n)
  }
  
 
  
   return(reactive({plotOut(n = n())}))

}


ui <- function(id) {
  tagList(
  tabPanel( "Plot Tab",
    navlistPanel(
      tabPanel("plotting",
               plotModuleUI("plotModule"),
      downloadButton('downloadPlot', 'Download Plot'),
      downloadButton('downloadRDS', 'Download RDS'))
    )
  )
  )
}

server <- function(input, output, session){
  
  
  getPlot <-  callModule(plotModule, "plotModule")
    
  
  
  output$downloadPlot <- downloadHandler(
    filename = 'test.pdf',
    content = function(file) {
      # device <- function(..., width, height) {
        # grDevices::pdf(..., width = width, height = height)
      # }
      # ggsave(file, plot = getPlot(), device = device)
      # cowplot::save_plot(file, cowplot::plot_grid(getPlot(), getPlot()))
      pdf(file)
      print(getPlot() + 
              labs(title = "MLB run scoring, 1901-2015",
                   subtitle = "Run scoring has been falling for 15 years, reversing a 30 year upward trend",
                   caption = "Source: the Lahman baseball database", 
                   x = "year", y = "team runs per game") )
      print(getPlot())
      dev.off()
    })  
  
  output$downloadRDS <- downloadHandler(
    filename = 'test.rds',
    content = function(file) {
      # device <- function(..., width, height) {
      # grDevices::pdf(..., width = width, height = height)
      # }
      # ggsave(file, plot = getPlot(), device = device)
      # cowplot::save_plot(file, cowplot::plot_grid(getPlot(), getPlot()))
      saveRDS(getPlot(), file)
    })  
  
  
}

shinyApp(ui, server)
