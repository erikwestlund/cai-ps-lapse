# Propensity Score Analysis: Effect of Care Lapse on Vision Loss in Diabetic Retinopathy

This repository contains R code for analyzing the effect of lapsing from care on vision loss outcomes in diabetic retinopathy patients using multiple propensity score methods.

## Project Overview

**Research Question**: What is the effect of lapsing from care on vision impairment and blindness among diabetic retinopathy patients?

**Study Design**: Propensity score analysis estimating the Average Treatment Effect in the Treated (ATT), where:
- **Treatment**: Having a lapsed episode during the observation window
- **Outcome**: Vision impairment (logMAR ≥ 0.3) and blindness (logMAR ≥ 1.0)
- **Estimand**: ATT - the effect of lapsing vs. not lapsing among those who lapsed

## Analysis Methods

The analysis implements 11 different propensity score approaches to ensure robust results:

### Matching Methods (MatchIt package)
- Nearest neighbor matching with various distance measures (GLM, GAM, GBM, LASSO, RPART)
- Mahalanobis distance matching
- Subclassification

### Weighting Methods
- **MatchIt**: Subclassification weighting
- **WeightIt**: CBPS, Entropy Balancing, BART
- **twang**: Gradient Boosted Models (primary method)

## Repository Structure

```
├── analysis.Rmd              # Main analysis pipeline
├── analysis_beta.Rmd         # Alternative analysis version  
├── results-summary.Rmd       # Summary tables and visualizations
├── twang-analysis.Rmd        # Focused analysis with marginal effects
├── settings.R                # Configuration and data paths
├── ps-lapse.Rproj           # RStudio project file
└── ps_results_summary.rds   # Cached results summary
```

## Getting Started

### Prerequisites
- R (≥ 4.0)
- RStudio (recommended)
- Required R packages:
  ```r
  install.packages(c("MatchIt", "WeightIt", "twang", "survey", "margins", 
                     "cobalt", "dplyr", "ggplot2", "knitr", "rmarkdown"))
  ```

### Configuration
1. Update `settings.R` with your data paths:
   ```r
   s_root <- "path/to/your/data/directory"
   source_file_path <- "path/to/your/csv/file"
   ```

2. Ensure your CSV data file contains the required variables (see analysis.Rmd for full list)

### Running the Analysis

#### Quick Start
```r
# Run main analysis
rmarkdown::render("analysis.Rmd")

# Generate summary
rmarkdown::render("results-summary.Rmd")
```

#### Step-by-Step
```r
source("settings.R")

# 1. Main propensity score analysis (slow - uses caching)
rmarkdown::render("analysis.Rmd")

# 2. Results visualization and comparison
rmarkdown::render("results-summary.Rmd") 

# 3. Detailed marginal effects analysis
rmarkdown::render("twang-analysis.Rmd")
```

## Key Features

### Caching System
- Computationally expensive models are cached as RDS files
- Results stored in `ps_results/` directory
- Control re-running with `rerun_model` and `rerun_diagnostics` flags

### Diagnostic Outputs
- Balance plots for covariate distribution
- Propensity score overlap diagnostics  
- Love plots showing standardized mean differences
- Comprehensive balance tables

### Subgroup Analysis
Marginal effects calculated for clinically relevant subgroups:
- PDR patients on any treatment
- PDR patients on anti-VEGF only
- PDR patients on PRP only
- NPDR patients by severity and treatment status

## Data Requirements

### Required Variables
- **Outcomes**: Vision measurements (logMAR)
- **Treatment**: Lapse indicator
- **Covariates**: Demographics, comorbidities, treatment history, baseline vision
- **Identifiers**: De-identified patient MRN

### Data Processing
- Excludes patients with missing CCI/DCSI scores
- Creates binary outcome variables for vision impairment/blindness
- Generates composite treatment and severity measures

## Results

Primary results use the twang GBM method with inverse probability weighting. Key outputs include:

1. **Odds ratios** comparing lapse vs. no lapse across all methods
2. **Marginal effects** showing probability differences by subgroup
3. **Balance diagnostics** assessing covariate balance after matching/weighting
4. **Sensitivity analysis** comparing results across multiple methods

## Notes and Limitations

- Analysis handles clustered data (multiple eyes per patient) using survey methods
- Some computationally intensive methods (genetic matching, optimal matching) are disabled
- Results depend on the assumption that all confounders are measured and included
- Current implementation focuses on ATT; ATE analysis would require modifications

## Contact

**Author**: Erik Westlund  
**Project**: Social Determinants of Health in Diabetic Retinopathy

## License

This code is provided for research purposes. Please contact the author before using for other applications.