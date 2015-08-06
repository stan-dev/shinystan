# output$hamiltonian_gif <- renderImage({
#   list(src="www/ham-sim-stepsize-ok.gif")
# }, deleteFile = FALSE)

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
observeEvent(input$save_user_model_info, handlerExpr = {
  model_info <- input$user_model_info
  if (model_info == "")
    model_info <- "Use this space to store notes about your model"
  slot(object, "user_model_info") <<- model_info
})
observeEvent(input$save_user_model_code, handlerExpr = {
  model_code <- input$user_model_code
  if (model_code == "")
    model_code <- "Use this space to store your model code" 
  slot(object, "model_code") <<- model_code
})
