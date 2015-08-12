# ggplot theme elements to be used as needed  -----------------------------
blue_color <- "#006dcc"
theme <- function(...) ggplot2::theme(...)
transparent <- theme(panel.background = ggplot2::element_blank(), 
                     plot.background = ggplot2::element_blank())
axis_line_color <- "gray20"
axis_color <- theme(axis.line = ggplot2::element_line(color = axis_line_color))
axis_labs <- theme(axis.title = ggplot2::element_text(face = "bold", size = 13))
title_txt <- theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
fat_axis <- theme(axis.line.x = ggplot2::element_line(size = 3, color = axis_line_color), 
                  axis.line.y = ggplot2::element_line(size = 0.5, color = axis_line_color))
h_lines <- theme(panel.grid.major = 
                   ggplot2::element_line(size = 0.10, linetype = 3, color = "turquoise4"),
                 panel.grid.major.x = ggplot2::element_blank())
v_lines <- theme(panel.grid.major = 
                   ggplot2::element_line(size = 0.25, linetype = 3, color = "turquoise4"),
                 panel.grid.major.y = ggplot2::element_blank())
no_lgnd <- theme(legend.position = "none")
lgnd_bot <- theme(legend.position = "bottom", legend.background = ggplot2::element_blank())
lgnd_top <- theme(legend.position = "top", legend.background = ggplot2::element_blank())
lgnd_left <- theme(legend.position = "left", legend.background = ggplot2::element_blank())
lgnd_right <- theme(legend.position = "right", legend.background = ggplot2::element_blank())
no_yaxs <- theme(axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), 
                 axis.text.y = ggplot2::element_blank())
strip_txt <- theme(strip.text = 
                     ggplot2::element_text(size = 12, face = "bold", color = "white"),
                   strip.background = 
                     ggplot2::element_rect(color = axis_line_color, fill = axis_line_color))

base_fill <- "#66a7e0" 
overlay_fill <- "#006dcc" 
vline_base_clr <- "#006dcc"
pt_outline_clr <- "#328ad6"
divergent_fill <- "#ae0001" 
hit_max_td_fill <- "#eeba30"
divergent_clr <-  "black" 
hit_max_td_clr <- "black" 
div_and_hit_shape <- 21
