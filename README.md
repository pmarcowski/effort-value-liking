# Effort–value dissociation: liking vs. price in consumer valuation

This repository contains the data, analysis notebooks, and experiment code for a paper showing that **effort can decrease hedonic value (liking) while increasing monetary value (price, willingness to pay, willingness to accept)**. Evidence comes from laboratory and online studies of performed, observed, and cognitive effort, with waiting as a control.

> Rendered notebooks: https://01a0406e-4303-000b-ba33-bacfff130e95.share.connect.posit.cloud/ · OSF: https://osf.io/5rjds/

## Studies

Each experiment lives in one directory under `experiments/` that holds its task code and its prepared data in `data/`. The task code keeps its internal study names (for example `ratingclicks2_cognitive`), which are the identifiers carried in the raw data and survey exports.

Experiments with an S prefix are reported in the Supplemental Material.

| Study | Effort | Judgments and design | Directory | Notebook |
|---|---|---|---|---|
| Experiment 1 | laboratory stepping | liking, wanting, or price; between subjects | `stepping/` | `main.qmd` |
| Experiment 2 | observed stepping (video) | liking or price; between subjects | `observed/` | `main.qmd` |
| Experiment 3 | online clicking | liking and price; within subjects | `clicking_within/` | `main.qmd` |
| Experiment 4 | clicking with an effort-contingent reveal of the object | liking or price; between subjects; progressive reveal | `reveal/` | `main.qmd` |
| Experiment 5 | cognitive: letter transposition or serial subtraction | liking or price; between subjects | `cognitive/` | `main.qmd` |
| Experiment 6A | laboratory stepping | buying or selling prices; between subjects | `prices_between/` | `main.qmd` |
| Experiment 6B | online clicking | buying and selling prices; within subjects | `prices_within/` | `main.qmd` |
| Experiment S1 | laboratory stepping | NASA-TLX after every trial (effort manipulation validation) | `taskload_stepping/` | `supplementary.qmd` |
| Experiment S2 | online clicking | NASA-TLX after every trial (effort manipulation validation) | `taskload_clicking/` | `supplementary.qmd` |
| Experiment S3 | none | price and liking with counterbalanced order | `price_liking_order/` | `supplementary.qmd` |
| Experiment S4 | none | liking under small or large explicit prices | `price_magnitude/` | `supplementary.qmd` |
| Experiment S5 | foraging game | liking or price; prior experience between subjects | `foraging/` | `supplementary.qmd` |
| Experiment S6 | passive wait of 10, 20, or 40 s | liking or price and object preview; between subjects | `wait/` | `supplementary.qmd` |
| Experiment S7 | clicking with a post-effort reveal of the object | liking or price; between subjects | `reveal/` | `main.qmd` |

## Directory Structure

```
effort-value-liking/
├── analysis/
│   ├── index.qmd           # overview: the dissociation across experiments
│   ├── main.qmd            # Experiments 1 to 6B and S7
│   ├── supplementary.qmd   # Experiments S1 to S6, pooled analyses, appraisals
│   ├── _quarto.yml         # website configuration
│   └── evl.scss            # site styling
├── experiments/
│   ├── <study>/            # task code + prepared CSVs in data/
│   └── qualtrics/          # shared post-survey exports and their union (not distributed)
├── output/                 # exported figures and the overview's summary tables (generated)
├── R/
│   ├── build_qualtrics.R   # survey exports -> one combined post-survey
│   ├── prepare_data.R      # raw -> prepared CSVs
│   └── functions.R         # shared notebook helpers
├── LICENSE
└── README.md
```

The prepared data reproduce every reported analysis. Not included are the raw exports in each `data/raw/` and in `experiments/qualtrics/`, which carry recruitment-platform identifiers; the video shown in Experiment 2; and the object images of Experiments 4, 5, S6, and S7, which are the stimuli of Brady et al. (2013).

## Usage

Requires R (4.5 or later) and Quarto; the notebooks were rendered with R 4.5.1 and Quarto 1.10.18. Install the R packages once:

```r
install.packages(c("tidyverse", "afex", "emmeans", "here", "withr", "janitor", "interactions",
                   "ggh4x", "cowplot", "rstatix", "cocor", "Hmisc", "rmarkdown", "fs", "data.table"))
```

Then, from the repository root:

```bash
# rebuild the combined post-survey from the raw survey exports
Rscript R/build_qualtrics.R

# rebuild the prepared CSVs from the raw exports
Rscript R/prepare_data.R

# render the notebooks and the overview as a website in analysis/_site/
quarto render analysis
```

The first two steps need the raw exports; rendering the site reads only the prepared CSVs in this repository.

## Analyses

Analyses are implemented in the Quarto notebooks in `analysis/`, which render as a website in `analysis/_site/`: an overview page (`index.qmd`) gathers the dissociation test and the standardized effect sizes of every study, and the two notebooks are its pages. Every experiment section reports one factorial omnibus model, the linear effort slope of each judgment with its confidence interval, and the difference between those slopes, which is the dissociation test. The checks the design supports follow: time on task, task-load moderation, naive beliefs, and participant rating profiles.

* **`main.qmd`** — Experiments 1 to 6B in the order of the paper, each with everything reported about that sample, including Experiment S7, the post-effort target reveal that follows Experiment 4, and the task-by-task detail of Experiment 5.
* **`supplementary.qmd`** — the effort-manipulation validation samples (Experiments S1 and S2), the two control studies on the baseline relation between price and liking (Experiments S3 and S4), the foraging and waiting studies (Experiments S5 and S6), the two pooled analyses (performed versus observed effort, and rating profiles across experiments), the post-experiment appraisals of each sample (perceived instrumentality and task load), and two exploratory analyses of Experiment 4 (object liking and rating response time).

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
