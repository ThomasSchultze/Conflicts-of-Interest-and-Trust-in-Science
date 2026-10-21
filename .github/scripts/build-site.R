# Build the static GitHub Pages site from the repository root.

dir.create("site/assets", recursive = TRUE, showWarnings = FALSE)
source("combined-figure/COI_plots.R")

if (!file.exists("combined-figure/Fig1.png")) {
  stop("The combined figure was not created.")
}

copied <- file.copy("combined-figure/Fig1.png", "site/assets/Fig1.png", overwrite = TRUE)
if (!copied) {
  stop("The combined figure could not be copied into the site.")
}

supplemental_pdf <- "supplemental-material/supplemental-information.pdf"
if (!file.exists(supplemental_pdf)) {
  stop("The supplemental information PDF is missing.")
}
dir.create("site/supplemental-material", recursive = TRUE, showWarnings = FALSE)
if (!file.copy(supplemental_pdf, "site/supplemental-material/supplemental-information.pdf", overwrite = TRUE)) {
  stop("The supplemental information PDF could not be copied into the site.")
}
