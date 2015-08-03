output$user_text_saved <- renderText({
  if (input$save_user_model_info > 0) {
    paste("Saved", format(Sys.time(), "%a %b %d %Y %X"))
  }
})
output$user_code_saved <- renderText({
  if (input$save_user_model_code > 0) {
    paste("Saved", format(Sys.time(), "%a %b %d %Y %X"))
  }
})