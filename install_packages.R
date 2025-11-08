# install_packages.R
# Helper script to install all required packages for the Anomaly Tracker app.

required_packages <- c(
  "shiny",
  "dplyr",
  "purrr",
  "tibble",
  "DT",
  "viridisLite",
  "shinyBS",
  "shinyvalidate",
  "ggplot2",
  "reactlog" # optional but nice for debugging; safe to install
)

install_if_missing <- function(pkgs) {
  installed <- rownames(installed.packages())
  to_install <- pkgs[!(pkgs %in% installed)]

  if (length(to_install) == 0L) {
    message("All required packages are already installed.")
    return(invisible(TRUE))
  }

  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
  message("Done.")
  invisible(TRUE)
}

install_if_missing(required_packages)
