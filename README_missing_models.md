# Running Missing Models

## Problem
Some propensity score models are missing for certain imputations (e.g., entropy for imputations 9, 32, 43), which prevents complete outcome model fitting and pooling.

## Solution

### Step 1: Run Missing PS Models
```r
# Edit configuration in run_missing_ps.R if needed:
method <- "entropy"
imps <- c(9, 32, 43)

# Run the script
source("run_missing_ps.R")
```

This will:
- Use the same `ps_strategy_base` function that the main analysis uses
- Show detailed diagnostics (weights, PS scores, sample sizes)
- Automatically cache results to `ps_cache/`
- Report which imputations succeeded/failed

### Step 2: Run Outcome Models for Those Imputations
Once PS models are complete:
```r
# Use the same method and imputation numbers
method <- "entropy"
imps <- c(9, 32, 43)  # Or only the ones that succeeded from Step 1

source("run_missing_outcome_models.R")
```

This will:
- Fit outcome models only for specified imputations
- Show coefficient estimates and p-values
- Save to `outcome_cache/`
- Report completeness (e.g., "47/50 complete")

## Files Created
- `run_missing_ps.R` - Run specific PS models
- `run_missing_outcome_models.R` - Run specific outcome models
- Results saved to standard cache directories used by main analysis

## Checking Completeness
After running, the scripts will report:
- How many models exist for the method
- Which imputations are still missing (if any)
- Whether all 50 are complete

## Integration
Once all models are complete, you can run:
- `reanalysis-10-pooling_results.qmd` to pool all results using Rubin's rules