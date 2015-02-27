output$ui_credits <- renderUI({
  jonah_and_stan <- "Jonah Sol Gabry and Stan Development Team"
  michael <- "Michael Andreae,"
  yuanjun <- "Yuanjun Gao,"
  dongying <- "Dongying Song"
  HTML(paste(strong(jonah_and_stan), paste("with", michael, yuanjun, dongying), sep = '<br/>'))
})
