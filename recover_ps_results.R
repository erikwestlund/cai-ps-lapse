# Recovery script for propensity score results
# Run this in your R session where the models were fitted

# Check if twang_results exists in your environment
if (exists("twang_results")) {
  cat("Found twang_results with", length(twang_results), "models\n")
  
  # Load required paths
  source("dependencies.R")
  
  # Extract weights from the existing models
  twang_weights <- list()
  for (i in 1:length(twang_results)) {
    twang_weights[[i]] <- twang_results[[i]]$weights
  }
  
  # Save the results
  ps_output <- list(
    twang_results = twang_results,
    weight_summary = if(exists("balance_summary_all")) balance_summary_all else NULL,
    avg_weights = if(exists("avg_weights")) avg_weights else NULL,
    formula_used = if(exists("ps_formula")) ps_formula else NULL,
    n_imputations = length(twang_results),
    method = "twang_gbm",
    n_trees = if(exists("n_trees")) n_trees else NULL,
    analysis_mode = if(exists("analysis_mode")) analysis_mode else "unknown"
  )
  
  saveRDS(ps_output, file.path(reanalysis_data_dir, "ps_results_twang.rds"))
  saveRDS(twang_weights, file.path(reanalysis_data_dir, "twang_weights.rds"))
  
  cat("Successfully saved results to:\n")
  cat("  -", file.path(reanalysis_data_dir, "ps_results_twang.rds"), "\n")
  cat("  -", file.path(reanalysis_data_dir, "twang_weights.rds"), "\n")
  
} else {
  cat("ERROR: twang_results not found in environment\n")
  cat("The models need to be re-run\n")
  
  # Check if partial results exist on disk
  if (file.exists(file.path(reanalysis_data_dir, "ps_results_twang.rds"))) {
    cat("\nHowever, ps_results_twang.rds exists on disk - may be from a previous run\n")
  }
}