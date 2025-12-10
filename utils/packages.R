# ============================================================
# packages.R
# Utility script to check, install, and load all dependencies
# for the map_by_fips project.
# ============================================================

required_packages <- c(
  "ggplot2",
  "sf",
  "dplyr",
  "cowplot",
  "biscale",     # for bi_class, bi legend, bivariate palettes
  "viridisLite",
  "scales"
)

# Function to install missing packages and load all packages
load_required_packages <- function(pkgs = required_packages) {
  
  # Identify missing packages
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  
  # Install any missing packages
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org/")
  }
  
  # Load all packages
  lapply(pkgs, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  })
  
  invisible(TRUE)
}

# Automatically load packages when sourced
load_required_packages()
