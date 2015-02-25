# remove unsaved parameters from param_dims
get_stanfit_param_dims <- function(stanfit, param_names) {
  # first get rid of indexing, keeping only the names
  raw_names <- sapply(param_names, FUN = function(i){
    strsplit(i, split = "\\[")[[1]][1]
  })

  raw_names <- unique(raw_names)
  keep_dims <- which(names(stanfit@par_dims) %in% raw_names)
  stanfit@par_dims[keep_dims]
}
