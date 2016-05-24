pp_yrep_clr <- "#487575"
pp_yrep_fill <- "#6B8E8E"

.pp_hists_rep_vs_obs <- function(y, yrep_samp, geom = "histogram") {
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + no_lgnd)
  graphs <- lapply(1:(1 + nrow(yrep_samp)), function(i) {
    if (i == 1) 
      g <-  qplot(x = y, geom = geom, color = I(vline_base_clr), 
                  size = I(0.2), fill = I(base_fill)) + labs(y = "", x = "y")
    else 
      g <- qplot(x = yrep_samp[i-1, ], geom = geom, 
                 color = I(pp_yrep_clr), fill = I(pp_yrep_fill),
                 size = I(0.2)) + labs(y = "", x = rownames(yrep_samp)[i-1])
    g + thm 
  })
  graphs
}

.pp_dens_rep_vs_obs <- function(y, yrep_samp, x_lim) {
  dat <- data.frame(t(yrep_samp))
  dat <- cbind(y = y, dat)
  mdat <- suppressMessages(reshape2::melt(dat))
  mdat$which <- "yrep"
  mdat$which[mdat$variable == "y"] <- "y"
  graph <- ggplot(mdat, aes(x = value, group = variable, fill = which, 
                            color = which, alpha = which, size = which))
  graph <- graph + 
    geom_density() + 
    scale_color_manual(values = c(vline_base_clr, pp_yrep_clr)) + 
    scale_fill_manual(values = c(base_fill, pp_yrep_fill)) + 
    scale_alpha_manual(values = c(3/4, 0)) + 
    scale_size_manual(values = c(1/3, 1/2)) + 
    scale_x_continuous(limits = x_lim) 
  graph + labs(x = "", y = "") + 
    theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + no_lgnd)
}

.pp_hists_test_statistics <- function(stat_y, stat_yrep, which, geom = "histogram") {
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs)
  graph <- ggplot(data.frame(x = stat_yrep), aes(x = x)) 
  if (geom == "histogram") { 
    graph <- graph + stat_bin(aes(y=..count../sum(..count..)), 
                              color = pp_yrep_clr, fill = pp_yrep_fill, size = 0.2) 
  }
  if (geom == "density") {
    graph <- graph +
      geom_density(color = pp_yrep_clr, fill = pp_yrep_fill, size = 0.2)
  }
  graph + 
    geom_vline(xintercept = stat_y, color = vline_base_clr, size = 1.5, alpha = 1) +
    labs(y = "", x = paste0(which, "(yrep)")) +
    thm 
}

.pp_hist_resids <- function(resids) {
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + no_lgnd)
  graph <- ggplot(data.frame(x = resids), aes(x = x)) + 
    stat_bin(aes(y=..count../sum(..count..)), 
             color = vline_base_clr, fill = base_fill, size = 0.2)
  graph + thm + labs(y = "", x = names(resids))
}

.pp_avg_rep_vs_avg_resid_rep <- function(rowMeans_yrep, rowMeans_resids){
  dat <- data.frame(x = rowMeans_yrep, y = rowMeans_resids)
  xy_labs <- labs(x = "Average yrep", y = "Average residual")
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_lgnd)
  graph <- ggplot(dat, aes(x, y)) + 
    geom_hline(yintercept = 0, color = vline_base_clr, size = 0.75) + 
    geom_point(fill = pp_yrep_fill, color = pp_yrep_clr, size = 2.75, 
               alpha = 0.75, shape = 21) + 
    xy_labs 
    
  graph + xy_labs + thm 
}


.pp_y_vs_avg_rep <- function(y, colMeans_yrep, zoom_to_zero = FALSE){
  dat <- data.frame(x = y, y = colMeans_yrep, z = abs(y-colMeans_yrep))
  xy_labs <- labs(x = "y", y = "Average yrep")
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis)
  graph <- ggplot(dat, aes(x, y)) + 
    geom_abline(intercept = 0, slope = 1, color = vline_base_clr, size = 0.75) +
    geom_point(fill = pp_yrep_fill, color = pp_yrep_clr, size = 2.75, 
               alpha = 0.75, shape = 21) + 
    xy_labs + thm
  
  if (zoom_to_zero) {
    graph <- graph + 
      geom_hline(yintercept = 0, size = 3, color = axis_line_color) + 
      geom_vline(xintercept = 0, size = 0.5, color = axis_line_color) +
      thm %+replace% theme(axis.line = element_blank())
  }
  
  graph 
}

