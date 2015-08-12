# This file is part of shinystan
# Copyright (C) Jonah Gabry
#
# shinystan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinystan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.

pp_yrep_clr <- "#487575"
pp_yrep_fill <- "#6B8E8E"


# validate input tests ----------------------------------------------------
pp_tests <- reactive({
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
    if (i == 1) 
      g <-  qplot(x = y, geom = geom, color = I(vline_base_clr), 
                  size = I(0.2), fill = I(base_fill)) + labs(y = "", x = "y")
    else 
      g <- qplot(x = y_rep_samp[i-1, ], geom = geom, 
                 color = I(pp_yrep_clr), fill = I(pp_yrep_fill),
                 size = I(0.2)) + labs(y = "", x = rownames(y_rep_samp)[i-1])
    g + thm 
  })
  graphs
}

.pp_dens_rep_vs_obs <- function(y, y_rep_samp, x_lim) {
  dat <- data.frame(t(y_rep_samp))
  dat <- cbind(y = y, dat)
  mdat <- reshape2::melt(dat)
  mdat$which <- "y_rep"
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

.pp_hists_test_statistics <- function(stat_y, stat_y_rep, which, geom = "histogram") {
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs)
  graph <- ggplot(data.frame(x = stat_y_rep), aes(x = x)) 
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
    labs(y = "", x = paste0(which, "(y_rep)")) +
    thm 
}

.pp_hist_resids <- function(resids) {
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + no_lgnd)
  graph <- ggplot(data.frame(x = resids), aes(x = x)) + 
    stat_bin(aes(y=..count../sum(..count..)), 
             color = vline_base_clr, fill = base_fill, size = 0.2)
  graph + thm + labs(y = "", x = names(resids))
}

.pp_avg_rep_vs_avg_resid_rep <- function(rowMeans_y_rep, rowMeans_resids){
  dat <- data.frame(x = rowMeans_y_rep, y = rowMeans_resids)
  xy_labs <- labs(x = "Average y_rep", y = "Average residual")
  thm <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_lgnd)
  graph <- ggplot(dat, aes(x, y)) + 
    geom_hline(yintercept = 0, color = vline_base_clr, size = 0.75) + 
    geom_point(fill = pp_yrep_fill, color = pp_yrep_clr, size = 2.75, 
               alpha = 0.75, shape = 21) + 
    xy_labs 
    
  graph + xy_labs + thm 
}


.pp_y_vs_avg_rep <- function(y, colMeans_y_rep, zoom_to_zero = FALSE){
  dat <- data.frame(x = y, y = colMeans_y_rep, z = abs(y-colMeans_y_rep))
  xy_labs <- labs(x = "y", y = "Average y_rep")
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
      thm %+replace% theme(axis.line = ggplot2::element_blank())
  }
  
  graph 
}

