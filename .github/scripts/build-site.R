# Build the static GitHub Pages site from the repository root.

dir.create("site/assets", recursive = TRUE, showWarnings = FALSE)
source("combined-figure/COI_plots.R")

if (!file.exists("figures/Fig1.png")) {
  stop("The combined figure was not created.")
}

copied <- file.copy("figures/Fig1.png", "site/assets/Fig1.png", overwrite = TRUE)
if (!copied) {
  stop("The combined figure could not be copied into the site.")
}
