# Data guide

## Participant-level files

Each `data/experiment-*` directory contains the original CSV data used by its corresponding script in `code/analysis`.

Common variables are:

| Variable | Description |
| --- | --- |
| `ID` | Study-specific, non-identifying participant ID. |
| `COI` | Conflict-of-interest condition. |
| `result` | Reported study-result condition (`pro` or `con`). |
| `AC` | Attention-check result; analysis scripts retain observations where it is `correct`. |
| `attention_check` | Participant response to the attention-check item. |
| `trust_study`, `trust_authors`, `trust_journal` | Trust ratings for the study, authors, and journal. |
| `trust_company` / `trust_funder` | Trust rating for the company or funder, where collected. |
| `est_replicability` | Estimated replicability rating. |

Experiment 3 additionally includes `participant_phd` and `published_empirical`; Experiment 5 includes `cond`.

## Derived plot data

`data/derived/plot_data_Exp*.csv` are prepared long-format inputs for `code/figures/COI_plots.R`. They contain `ID`, `COI`, `result`, `measure`, and `trust`, and are used to reproduce the combined central figure.
