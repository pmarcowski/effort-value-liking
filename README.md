# Effort–value dissociation: liking vs. price in consumer valuation

This repository contains data, analysis notebooks, and materials for a paper showing that **effort can decrease hedonic value (liking) while increasing monetary value (price/WTP/WTA)**. Evidence comes from lab and online studies with real effort, observed effort, and within- vs between-subjects pricing.

> OSF: https://osf.io/5rjds/.

## Table of Contents

* [Overview](#overview)
* [Directory Structure](#directory-structure)
* [Installation](#installation)
* [Usage](#usage)
* [Analyses](#analyses)
* [License](#license)
* [Citation](#citation)
* [Contact](#contact)

## Overview

Across multiple experiments, participants who **worked** to obtain stimuli often **liked** them **less** but **priced** them **higher**. We combine lab and online samples to reveal dissociable effects on hedonic vs. monetary judgments.

---

## Directory Structure

```

effort-value-liking/
├── analysis/
│   ├── core.Rmd
│   ├── online.Rmd
│   └── supplementary.Rmd
├── data/
│   ├── raw/
│   └── prepared/
├── experiments/
├── output/
├── R/
│   └── prepare_data.R
├── LICENSE
└── README.md

````

## Installation

No installation is required beyond having R and the necessary libraries. To run the script, simply clone this repository or download the script files and execute them within your R environment. Refer to the header of each script for a complete list of necessary libraries for that specific analysis.

## Usage

1. Clone this repository and navigate to the project directory:

   ```bash
   git clone https://github.com/pmarcowski/effort-value-liking.git
   cd effort-value-liking
````

## Analyses

Analyses are implemented in the R Markdown notebooks in `analysis/`.

* **`core.Rmd`** — Lab studies (rating task), omnibus tests, participant-level profiles, figures.
* **`online.Rmd`** — Online studies (observed effort, task validation, within-subjects).
* **`supplementary.Rmd`** — Supplemental checks, robustness, additional figures.

> Knit artifacts are written under `analysis/`.

Analyses read processed files from `data/prepared/`. To regenerate them, **uncomment** the existing `source("R/prepare_data.R")` at the top of a notebook (or run it manually).

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

## Citation

If you use this code or data in your research, please cite our paper:

\[citation information to be added upon publication]

## Contact

For any questions or feedback, please contact the author directly.
