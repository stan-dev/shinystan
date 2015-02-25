# load multiple packages

Librarian <- function(pkgs) {
  # pkgs: a character vector of package names
  invisible(
    lapply(X = pkgs, FUN = require, character.only = TRUE)
    )
}
