# Conflicts of Interest and Trust in Science

Supplementary data, analysis code, and study materials for a five-experiment project on conflicts of interest and trust in science.

## Preregistration

All studies were preregistered on the [Open Science Framework](https://osf.io/79gcq/overview?view_only=f1e6bd8117524cd18f9f6e71cb6669a2).

## Repository contents

| Location | Contents |
| --- | --- |
| `experiments/experiment-1` through `experiments/experiment-5` | Self-contained folders containing anonymised participant-level data, the R analysis script, and study materials for each experiment. |
| `combined-figure` | Tidy figure data and the R script that creates the combined central result figure. |
| `site` | Source files for the public results site, published automatically with GitHub Pages. |

## Reproducing the analyses

Use R from the repository root. The scripts use `BayesFactor`, `dplyr`, `yarrr`, `HDInterval`, `ggplot2`, `tidyr`, `bain`, `gridExtra`, `cowplot`, and `ggbreak` as applicable. Install missing packages once, then run:

```r
source("experiments/experiment-1/COI_Exp1.R")
source("experiments/experiment-2/COI_Exp2.R")
source("experiments/experiment-3/COI_Exp3.R")
source("experiments/experiment-4/COI_Exp4.R")
source("experiments/experiment-5/COI_Exp5.R")
source("combined-figure/COI_plots.R")
```

The final command writes the central combined figure to `combined-figure/Fig1.png` at 600 dpi. Individual analysis scripts retain the original analysis workflow and print results to the R console.

## Data notes

The data are public with participant consent and contain no direct identifying information. `ID` is a study-specific participant identifier. See [experiments/README.md](experiments/README.md) for a concise variable guide and materials inventory.

## Licensing and citation

The R code is available under the [MIT License](LICENSE). Data and study materials are available under [CC BY 4.0](LICENSE-DATA.md). A formal citation will be added when the associated paper is published; until then, please cite this repository, the authors, and the OSF preregistration.
