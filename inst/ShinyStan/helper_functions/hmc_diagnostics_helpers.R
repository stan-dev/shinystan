thm <- theme_classic() %+replace% (no_lgnd + fat_axis + axis_labs + transparent)
thm_no_yaxs <- thm + no_yaxs

.sampler_param_pw <- function(sp, which = "accept_stat__", warmup_val) {
  if (!which %in% colnames(sp[[1]]))
    return(NULL)
  sp_pw <- lapply(1:length(sp), function(i) {
    out <- sp[[i]][, which]
  })
  sp_mat <- do.call("cbind", sp_pw)
  colnames(sp_mat) <- paste0("chain:", 1:ncol(sp_mat))
  sp_mat <- cbind(iterations = seq(from = warmup_val + 1, to = warmup_val + nrow(sp_mat)), 
                  sp_mat)
  as.data.frame(sp_mat)
}


.sampler_param_vs_param <- function(p, sp, divergent = NULL, hit_max_td = NULL, 
                                    p_lab, sp_lab, chain = 0, violin = FALSE, 
                                    smoother = FALSE) {
  xy_labs <- labs(
    y = if (missing(p_lab)) NULL else p_lab, 
    x = if (missing(sp_lab)) NULL else sp_lab
    )
  df <- data.frame(sp = do.call("c", sp), p = c(p))
  if (violin)
    df$sp <- as.factor(round(df$sp, 4))
  if (!is.null(divergent))
    df$divergent <- do.call("c", divergent)
  if (!is.null(hit_max_td))
    df$hit_max_td <- do.call("c", hit_max_td)
  
  base <- ggplot(df, aes(sp,p)) + 
    xy_labs + 
    thm 
  
  if (chain == 0) {
    if (violin) graph <- base + geom_violin(color = vline_base_clr, fill = base_fill) 
    else {
      graph <- base + geom_point(alpha = 1/3, color = pt_outline_clr, fill = base_fill, shape = 19) 
      if (smoother) 
        graph <- graph + stat_smooth(color = overlay_fill, se = FALSE)
      if (!is.null(divergent))
        graph <- graph + geom_point(data = subset(df, divergent == 1), aes(sp,p), 
                                    color = divergent_clr, fill = divergent_fill,
                                    size = 3, shape = div_and_hit_shape)
      if (!is.null(hit_max_td))
        graph <- graph + geom_point(data = subset(df, hit_max_td == 1), aes(sp,p), 
                                    color = hit_max_td_clr, fill = hit_max_td_fill, 
                                    size = 3, shape = div_and_hit_shape)
    }
    return(graph)
  }
  chain_data <- data.frame(sp = sp[, chain], p = p[, chain])
  if (!is.null(divergent))
    chain_data$div <- divergent[, chain]
  if (!is.null(hit_max_td))
    chain_data$hit <- hit_max_td[, chain]
  chain_clr <- color_vector_chain(ncol(sp))[chain]
  chain_fill <- chain_clr
  if (violin) {
    chain_data$sp <- as.factor(round(chain_data$sp, 4))
    graph <- base + 
      geom_violin(color = vline_base_clr, fill = base_fill) + 
      geom_violin(data = chain_data, aes(sp, p), color = chain_clr, 
                  fill = chain_fill, alpha = 0.5)
    return(graph)
  }
  graph <- base + geom_point(alpha = 1/3, color = pt_outline_clr, fill = base_fill, shape = 19)
  if (smoother) 
    graph <- graph + stat_smooth(color = overlay_fill, se = FALSE)
  graph <- graph + geom_point(data = chain_data, aes(sp,p), color = chain_fill, alpha = 0.5)
  if (smoother) 
    graph <- graph + stat_smooth(data = chain_data, aes(sp,p), color = chain_fill, se = FALSE)
  if (!is.null(divergent))
    graph <- graph + geom_point(data = subset(chain_data, div == 1), aes(sp,p), 
                                color = divergent_clr, fill = divergent_fill,
                                size = 3, shape = div_and_hit_shape)
  if (!is.null(hit_max_td))
    graph <- graph + geom_point(data = subset(chain_data, hit == 1), aes(sp,p), 
                                color = hit_max_td_clr, fill = hit_max_td_fill, 
                                size = 3, shape = div_and_hit_shape)
  graph
}

.sampler_param_vs_sampler_param_violin <- function(df_x, df_y, lab_x, lab_y, 
                                                   chain = 0) {
  xy_labs <- labs(y = lab_y, x = lab_x)
  df <- data.frame(x = do.call("c", df_x), y = do.call("c", df_y))
  df$x <- as.factor(df$x)
  
  base <- ggplot(df, aes(x,y)) + 
    xy_labs + 
    thm
  
  graph <- base + geom_violin(color = vline_base_clr, fill = base_fill) 
  if (chain == 0) 
    return(graph)
  
  chain_clr <- color_vector_chain(ncol(df_x))[chain]
  chain_fill <- chain_clr
  chain_data <- data.frame(x = as.factor(df_x[, chain]), y = df_y[, chain])
  graph + geom_violin(data = chain_data, aes(x,y), color = chain_clr, 
                      fill = chain_fill, alpha = 0.5)
}

.p_hist <- function(df, lab, chain = 0) {
  thm <- thm_no_yaxs
  mdf <- reshape2::melt(df, id.vars = "iterations")
  base <- ggplot(mdf, aes(x = value)) + 
    geom_histogram(aes_string(y="..density.."),
                   binwidth = diff(range(mdf$value))/30, fill = base_fill, 
                   color = vline_base_clr, size = 0.2) + 
    labs(x = if (missing(lab)) NULL else lab, y = "") + 
    thm
  
  if (chain == 0) {
    graph <- base + 
      geom_vline(xintercept = mean(mdf$value), color = vline_base_clr, size = .8) + 
      geom_vline(xintercept = median(mdf$value), color = vline_base_clr, lty = 2, size = 1)
    return(graph)
  }
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  chain_clr <- color_vector_chain(ncol(df) - 1)[chain]
  chain_fill <- chain_clr
  base + thm + geom_histogram(data = chain_data,
                              aes_string(y="..density.."),
                              binwidth = diff(range(chain_data$value))/30,
                              fill = chain_fill, alpha = 0.5) +
    geom_vline(xintercept = mean(chain_data$value), color = chain_clr, size = .8) + 
    geom_vline(xintercept = median(chain_data$value), 
               color = chain_clr, lty = 2, size = 1)
}

.treedepth_ndivergent_hist <- function(df_td, df_nd, chain = 0, divergent = c("All", 0, 1)) {
  plot_title <- theme(plot.title = element_text(size = 11, hjust = 0))
  plot_theme <- thm_no_yaxs + plot_title
  x_lab <- if (divergent == "All") "Treedepth (All)" else paste0("Treedepth (Divergent = ", divergent,")")
  plot_labs <- labs(x = x_lab, y = "") 
  
  mdf_td <- reshape2::melt(df_td, id.vars = "iterations")
  mdf_nd <- reshape2::melt(df_nd, id.vars = "iterations")
  mdf <- cbind(mdf_td, div = mdf_nd$value)
  plot_data <- if (divergent == "All") 
    mdf else subset(mdf, div == divergent)
  if (nrow(plot_data) == 0) 
    return(NULL)
  
  graph <- ggplot(plot_data, aes(x = factor(value)), na.rm = TRUE) + 
    geom_bar(aes(y=..count../sum(..count..)), width=1, fill = base_fill,
             color = vline_base_clr, size = 0.2) + 
    plot_labs + 
    plot_theme
  if (chain == 0) 
    return(graph)
  
  chain_clr <- color_vector_chain(ncol(df_td) - 1)[chain]
  chain_fill <- chain_clr
  chain_data <- subset(plot_data, variable == paste0("chain:",chain))
  graph + geom_bar(data = chain_data, aes(y=..count../sum(..count..)), 
                   fill = chain_fill, alpha = 0.5, width = 1)
}

.dynamic_trace_diagnostics <- function(param_samps, param_name, chain = 0,
                                       stack = FALSE, grid = FALSE, 
                                       group = NULL) {
  dim_samps <- dim(param_samps)
  if (is.null(dim_samps)) {
    nChains <- 1
  } else {
    nChains <- dim_samps[2]
  }
  if (nChains == 1) {
    param_chains <- xts::as.xts(ts(param_samps, start = 1))
  } else {
    if (chain != 0) {
      param_samps <- param_samps[, chain]
      param_chains <- xts::as.xts(ts(param_samps, start = 1))
    } else {
      param_chains <- xts::as.xts(ts(param_samps[,1], start = 1))
      for (i in 2:nChains) {
        param_chains <- cbind(param_chains,
                              xts::as.xts(ts(param_samps[,i], start = 1)))
      }
      colnames(param_chains) <- paste0("Chain", 1:nChains)
    }
  }
  `%>%` <- dygraphs::`%>%`
  y_axis_label_remove <- if (stack) "white" else NULL
  step_plot <- param_name %in% c("Treedepth", "Divergent")
  fill_graph <- param_name == "Divergent"
  stroke_width <- if (step_plot) 0.33 else 0.75
  clrs <- color_vector(nChains) 
  if (chain != 0) clrs <- clrs[chain]
  dygraphs::dygraph(param_chains, xlab = param_name, ylab = NULL, 
                    group = group) %>%
    dygraphs::dyOptions(colors = clrs, stackedGraph = stack, drawGrid = grid,
                        stepPlot = step_plot, #axisLabelFontSize = 11,
                        fillGraph = fill_graph, fillAlpha = 0.5,
                        strokeWidth = 0.75, animatedZooms = TRUE, 
                        drawXAxis = TRUE, drawYAxis = !fill_graph, 
                        drawAxesAtZero = TRUE, axisLineColor = "black", 
                        retainDateWindow = TRUE) %>%
    dygraphs::dyAxis("x", pixelsPerLabel = 1e7, axisLineWidth = 3) %>%
    dygraphs::dyAxis("y", pixelsPerLabel = 30, axisLabelWidth = 30) %>%
    dygraphs::dyLegend(show = "never") %>%
    dygraphs::dyHighlight(highlightCircleSize = 2,
                          highlightSeriesBackgroundAlpha = 1/3,
                          hideOnMouseOut = TRUE,
                          highlightSeriesOpts = list(strokeWidth = 1.5)) %>%
    dygraphs::dyCSS(css = "css/ShinyStan_dygraphs.css")
}
