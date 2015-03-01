# validate input tests ----------------------------------------------------
tests <- reactive({
  t1 <- need(input$y_name != "", message = "Waiting for y \n")
  t2 <- need(input$yrep_name != "", message = "Waiting for y_rep \n")
  validate(t1, t2)
})

# y_rep -------------------------------------------------------------------
y_rep <- reactive({
  yreps <- grep(paste0("^",input$yrep_name,"\\["), param_names)
  y_rep <- samps_post_warmup[,,yreps]
  dd <- dim(y_rep)
  y_rep <- array(y_rep, dim = c(prod(dd[1:2]), dd[3]))
  y_rep
})

# sample_ids_for_hist ------------------------------------------------------
sample_ids_for_hist <- reactive({
  go <- input$resample_hist_go          
  isolate({
    y_rep <- y_rep()
    sample_ids <- sample(nrow(y_rep), 8)  
    sample_ids
  })
})
# sample_ids_for_dens ------------------------------------------------------
sample_ids_for_dens <- reactive({
  go <- input$resample_dens_go          
  isolate({
    y_rep <- y_rep()
    sample_ids <- sample(nrow(y_rep), min(50, nrow(y_rep)))  
    sample_ids
  })
})
# sample_id_for_resids ------------------------------------------------------
sample_id_for_resids <- reactive({
  go <- input$resample_resids_go          
  isolate({
    y_rep <- y_rep()
    sample_id <- sample(nrow(y_rep), 1)  
    sample_id
  })
})


.pp_hists_rep_vs_obs <- function(y, y_rep_samp, geom = "histogram") {
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + no_lgnd)
  graphs <- lapply(1:(1 + nrow(y_rep_samp)), function(i) {
    if (i == 1) g <-  qplot(x = y, geom = geom, 
                            color = I("#428bca"), 
                            fill = I("#428bca"),
                            alpha = 2/3) + labs(y = "", x = "y")
    else g <- qplot(x = y_rep_samp[i-1, ], geom = geom, 
                 color = I("gray35"), 
                 fill = I("black"),
                 alpha = 2/3) + labs(y = "", x = rownames(y_rep_samp)[i-1])
    
    g + thm 
  })
  graphs
}

.pp_dens_rep_vs_obs <- function(y, y_rep_samp, x_lim, y_lim) {
  dat <- data.frame(t(y_rep_samp))
  dat <- cbind(y = y, dat)
  mdat <- melt(dat)
  mdat$which <- "y_rep"
  mdat$which[mdat$variable == "y"] <- "y"
  graph <- ggplot(mdat, aes(x = value, group = variable, fill = which, color = which, alpha = which, size = which))
  graph <- graph + 
    geom_density() + 
    scale_color_manual(values = c("#428bca", "gray35")) + 
    scale_fill_manual(values = c("#428bca", "gray35")) + 
    scale_alpha_manual(values = c(2/3, 0)) + 
    scale_size_manual(values = c(1/3, 1/2)) + 
    scale_y_continuous(limits = y_lim) +
    scale_x_continuous(limits = x_lim) 
  graph + labs(x = "", y = "") + theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + no_lgnd)
}

.pp_hists_test_statistics <- function(stat_y, stat_y_rep, which, geom = "histogram") {
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs)
  graph <- ggplot(data.frame(x = stat_y_rep), aes(x = x)) 
  if (geom == "histogram") {
    graph <- graph + stat_bin(aes(y=..count../sum(..count..)), color = "gray35", fill = "black", alpha = 2/3) 
  }
  if (geom == "density") {
    graph <- graph +
      geom_density(color = "gray35", fill = "black", alpha = 2/3)
  }
  graph + 
    geom_vline(xintercept = stat_y, color = "#428bca", size = 1.5, alpha = 2/3) +
    labs(y = "", x = paste0(which, "(y_rep)")) +
    thm 
}

.pp_hist_resids <- function(resids) {
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + no_lgnd)
  graph <- ggplot(data.frame(x = resids), aes(x = x)) + 
    stat_bin(aes(y=..count../sum(..count..)), color = "gray35", fill = "black", alpha = 2/3) +
    stat_function(fun=dnorm, args=list(mean=mean(resids), sd=sd(resids)), color = "#428bca", alpha = 2/3)
  graph + thm + labs(y = "", x = names(resids))
}

.pp_avg_rep_vs_avg_resid_rep <- function(rowMeans_y_rep, rowMeans_resids){
  dat <- data.frame(x = rowMeans_y_rep, y = rowMeans_resids)
  xy_labs <- labs(x = "Average y_rep", y = "Average residual")
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_lgnd)
  graph <- ggplot(dat, aes(x, y)) + 
    geom_point(color = "gray35", size = 3.75, shape = 19) + 
    xy_labs 
    
  graph + xy_labs + thm 
}


.pp_y_vs_avg_rep <- function(y, colMeans_y_rep){
  dat <- data.frame(x = y, y = colMeans_y_rep, z = abs(y-colMeans_y_rep))
  xy_labs <- labs(x = "y", y = "Average y_rep")
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_lgnd)
  graph <- ggplot(dat, aes(x, y, color = z)) + 
    geom_point(size = 3.75, alpha = 1, shape = 19) + 
    scale_color_gradient(low = "gray35", high = "gray85") +
    annotate("text", label = "The darker the point the closer \n it is to the line x = y",
             x = min(y), y = 0.9*max(colMeans_y_rep), hjust = 0, 
             color = "gray35", size = 4.5) +
    xy_labs 
  
  graph + xy_labs + thm
}


