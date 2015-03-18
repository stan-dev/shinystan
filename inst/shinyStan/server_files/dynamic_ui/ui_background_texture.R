output$ui_background_texture <- renderUI({
  bg <- input$background_texture
  if (bg == "default") NULL 
    else tags$style(type = "text/css", paste0("body {background-image: url('textures/",bg,".jpg');}"))
})