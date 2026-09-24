# Conflicts of Interest and Trust in Science

Supplementary data, analysis code, and study materials for a five-experiment project on conflicts of interest and trust in science.

## Preregistration

All studies were preregistered on the [Open Science Framework](https://osf.io/79gcq/overview?view_only=f1e6bd8117524cd18f9f6e71cb6669a2).

## Repository contents

| Location | Contents |
| --- | --- |
| `data/experiment-1` through `data/experiment-5` | Anonymised participant-level CSV data for the five experiments. |
| `data/derived` | Tidy data sets used to create the combined central figure. |
| `code/analysis` | One R script reproducing the analyses for each experiment. |
| `code/figures/COI_plots.R` | R script that creates the combined central result figure. |
| `figures` | Generated, publication-quality figures. Run the figure script to create `Fig1.png`. |
| `materials` | Publicly shareable study materials; these will be added when available. |

## Reproducing the analyses

Use R from the repository root. The scripts use `BayesFactor`, `dplyr`, `yarrr`, `HDInterval`, `ggplot2`, `tidyr`, `bain`, `gridExtra`, `cowplot`, and `ggbreak` as applicable. Install missing packages once, then run:

```r
source("code/analysis/COI_Exp1.R")
source("code/analysis/COI_Exp2.R")
source("code/analysis/COI_Exp3.R")
source("code/analysis/COI_Exp4.R")
source("code/analysis/COI_Exp5.R")
source("code/figures/COI_plots.R")
```

The final command writes the central combined figure to `figures/Fig1.png` at 600 dpi. Individual analysis scripts retain the original analysis workflow and print results to the R console.

## Data notes

The data are public with participant consent and contain no direct identifying information. `ID` is a study-specific participant identifier. See [data/README.md](data/README.md) for a concise variable guide.

## Licensing and citation

The R code is available under the [MIT License](LICENSE). Data and study materials are available under [CC BY 4.0](LICENSE-DATA.md). A formal citation will be added when the associated paper is published; until then, please cite this repository, the authors, and the OSF preregistration.
