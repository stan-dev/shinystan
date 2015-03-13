tooltip_ids <- c("download_multiview", "dynamic_trace_stack", "download_all_summary", "tex_options", "bivariate_transform_x", "trivariate_transform_x", "bivariate_transform_y", "trivariate_transform_y", "trivariate_transform_z")
tooltip_msgs <- c("Will be a list object with one element per plot.", 
                  "If 'Stacked' is selected, the chains will be stacked on top of one another rather than drawing them independently. The first series specified in the input data will wind up on top of the chart and the last will be on bottom. Note that the y-axis values no longer correspond to the true values when this option is enabled.",
                  "Save as data.frame (.RData)", "Print latex table to R console", 
                  rep("A function of x, e.g. log(x), log(x/(1-x)), sqrt(x), etc.",2),
                  rep("A function of y, e.g. log(y), log(y/(1-y)), sqrt(y), etc.",2),
                  "A function of z, e.g. log(z), log(z/(1-z)), sqrt(z), etc.")
tooltip_placements <- c(rep("right", 4), rep("top", 5))
