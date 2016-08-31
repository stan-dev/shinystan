if (exists(".SHINYSTAN_OBJECT")) {
  object <- .SHINYSTAN_OBJECT
} else {
  object <- get(".SHINYSTAN_OBJECT", envir = shinystan:::.sso_env)  
}

path_to_extract_sso <- file.path("server_files","utilities","extract_sso.R")
server_files <- list.files("server_files", full.names = TRUE, recursive = TRUE)
SERVER_FILES <- server_files[!server_files %in% path_to_extract_sso]
source("global_utils.R", local = TRUE)
source("server_utils.R", local = TRUE)
source(path_to_extract_sso, local = TRUE)

# BEGIN server ------------------------------------------------------
# ___________________________________________________________________
function(input, output, session) {
  
  # If not running on server then automatically stop app whenever browser tab
  # (or any session) is closed
  if (!nzchar(Sys.getenv("SHINY_PORT"))) {
    session$onSessionEnded(function() stopApp(object))
  }
  
  # Stop the app when "Save & Close" button is clicked
  observeEvent(
    input$save_and_close_button, 
    stopApp(object)
  )
  
  # Source all files from server_files directory and subdirectories
  for (f in SERVER_FILES) 
    source(f, local = TRUE)

  # Link to pages from home page table of contents
  toc_entries <- c("Estimate", "Diagnose", "Explore", "Model Code")
  observe({
    local({
      lapply(toc_entries, function(x) {
        id <- paste0("toc_", if (x == "Model Code") "more" else tolower(x))
        shinyjs::onclick(id, updateTabsetPanel(session, "nav", selected = x))
      })
    })
  })
  
  # Toggle options dropdowns
  options_trigger_ids <- c("table", "multiparam", "autocorr", "rhat_warnings", 
                           "bivariate", "trivariate", "density", "hist")
  observe({
    lapply(seq_along(options_trigger_ids), function(j) {
      shinyjs::onclick(
        id = paste0(options_trigger_ids[j], "_options_show"),
        shinyjs::toggle(
          id = paste0(options_trigger_ids[j], "_options"),
          anim = TRUE,
          animType = "slide",
          time = 0.4
        )
      )
    })
  })
  
  # Enable/disable individual options 
  density_trigger_ids <-
    c("point_est", "ci", "x_breaks", "fill_color", "line_color")
  observe({
    lapply(seq_along(density_trigger_ids), function(j) {
      shinyjs::toggleState(
        id = paste0("dens_", density_trigger_ids[j]),
        condition = input$dens_chain_split == "Together"
      )
    })
    shinyjs::toggleState(id = "ac_flip", condition = input$ac_combine == FALSE)
  })
  
  # Links to glossary
  observe({
    shinyjs::onclick(
      "open_glossary_from_table",
      updateTabsetPanel(session, "nav", selected = "Glossary")
    )
    shinyjs::onclick(
      "open_glossary_from_nuts_table",
      updateTabsetPanel(session, "nav", selected = "Glossary")
    )
  })
  
  # Enable/disable diagnostic plots
  diagnostic_trigger_ids <- 
    paste0("diagnostic_", c("param", "param_transform", "param_transform_go"))
  observe({
    diag_nav <- input$diagnostics_navlist
    local({
      if (diag_nav != 'By model parameter')
        lapply(diagnostic_trigger_ids, function(x)
          shinyjs::disable(id = x))
      else
        lapply(diagnostic_trigger_ids, function(x)
          shinyjs::enable(id = x))
    })
  })
  
  # Links to quick definitions
  observeEvent(
    input$open_quick_rhat, 
    shinyjs::info(includeText("text/quick_rhat.txt"))
  )
  observeEvent(
    input$open_quick_neff, 
    shinyjs::info(includeText("text/quick_neff.txt"))
  )
  observeEvent(
    input$open_quick_mcse, 
    shinyjs::info(includeText("text/quick_mcse.txt"))
  )
  
  # Show/hide citation
  observeEvent(
    input$shinystan_citation_show,
    shinyjs::toggle(
      id = "citation_div",
      anim = TRUE,
      animType = "fade"
    )
  )
  
} 
# END server ------------------------------------------------------
# _________________________________________________________________
