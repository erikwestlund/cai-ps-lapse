# Script to run specific missing outcome models
# Usage: Set method and imps variables, then source this file

# Load dependencies
source("dependencies.R")
source("functions.R")
source("alt_specs.R")
setup_analysis(seed = 2025)

# Configuration - EDIT THESE AS NEEDED
method <- "entropy"
imps <- c(9, 32, 43)

# Initialize logging
cat("\n================================================================================\n")
cat("RUNNING MISSING OUTCOME MODELS\n")
cat("================================================================================\n")
cat("Method:", method, "\n")
cat("Imputations to process:", paste(imps, collapse = ", "), "\n")
cat("================================================================================\n\n")

# Load required data
cat("Loading data...\n")
imputed_datasets <- readRDS(file.path(reanalysis_data_dir, "imputed_datasets.rds"))
variable_lists <- readRDS(file.path(reanalysis_data_dir, "variable_lists.rds"))

# Get formulas
ps_formula <- get_matching_formula_reanalysis()
outcome_formulas <- get_analysis_formulas_reanalysis()
outcome_formula <- outcome_formulas$full

# Set cache directories
cache_dir <- file.path(reanalysis_data_dir, "ps_cache")
outcome_cache_dir <- file.path(reanalysis_data_dir, "outcome_cache")

# Ensure outcome cache directory exists
if (!dir.exists(outcome_cache_dir)) {
  dir.create(outcome_cache_dir, recursive = TRUE)
}

cat("\nPS Formula:\n")
print(ps_formula)
cat("\nOutcome Formula:\n")
print(outcome_formula)
cat("\n")

# Process each specified imputation
results <- list()
successful_imps <- c()
failed_imps <- c()

for (i in imps) {
  cat(paste0("\n", paste(rep("-", 60), collapse = ""), "\n"))
  cat(paste0("Processing ", method, " - Imputation ", i, "\n"))
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  # Check if outcome model already exists
  outcome_cache_file <- file.path(outcome_cache_dir, 
                                 paste0("outcome_", method, "_imp", i, ".rds"))
  
  if (file.exists(outcome_cache_file)) {
    cat("  WARNING: Outcome model already exists. Overwriting...\n")
  }
  
  # Check for PS results
  ps_cache_file <- file.path(cache_dir, paste0(method, "_imp", i, ".rds"))
  
  if (!file.exists(ps_cache_file)) {
    cat("  ERROR: PS results not found at:", ps_cache_file, "\n")
    cat("  Skipping this imputation.\n")
    failed_imps <- c(failed_imps, i)
    next
  }
  
  # Load PS results
  cat("  Loading PS results...\n")
  ps_result <- readRDS(ps_cache_file)
  
  if (!ps_result$success) {
    cat("  ERROR: PS method failed for this imputation\n")
    cat("  Error message:", ps_result$error, "\n")
    failed_imps <- c(failed_imps, i)
    next
  }
  
  # Get the imputed dataset
  if (i > length(imputed_datasets)) {
    cat("  ERROR: Imputation", i, "not found in imputed_datasets\n")
    failed_imps <- c(failed_imps, i)
    next
  }
  
  imp_data <- imputed_datasets[[i]]
  cat("  Dataset dimensions:", nrow(imp_data), "rows x", ncol(imp_data), "cols\n")
  
  # Fit outcome model
  cat("  Fitting outcome model...\n")
  
  outcome_result <- tryCatch({
    # Call the same function used in file 9
    result <- fit_outcome_model_cached(
      ps_result = ps_result,
      outcome_formula = outcome_formula,
      data = imp_data,
      method_name = method,
      imputation = i,
      cache_dir = outcome_cache_dir
    )
    
    # Display key results
    if (result$success) {
      cat("  SUCCESS: Outcome model fitted\n")
      cat("  Coefficients extracted:\n")
      
      # Show coefficient summary
      coef_df <- data.frame(
        Variable = names(result$coefficients),
        Estimate = round(result$coefficients, 4),
        SE = round(sqrt(diag(result$vcov)), 4)
      )
      coef_df$OR <- round(exp(coef_df$Estimate), 3)
      coef_df$p_value <- round(2 * pnorm(abs(coef_df$Estimate/coef_df$SE), lower.tail = FALSE), 4)
      
      # Display lapse coefficient
      lapse_rows <- grep("ever_lapse", coef_df$Variable)
      if (length(lapse_rows) > 0) {
        cat("\n  Ever_lapse coefficient:\n")
        print(coef_df[lapse_rows, ])
      }
      
      cat("\n  Model info:\n")
      cat("    N observations:", result$n_obs, "\n")
      cat("    N coefficients:", length(result$coefficients), "\n")
      cat("    Cache file:", outcome_cache_file, "\n")
      
      successful_imps <- c(successful_imps, i)
    } else {
      cat("  FAILED: ", result$error, "\n")
      failed_imps <- c(failed_imps, i)
    }
    
    result
  }, error = function(e) {
    cat("  ERROR during fitting:", e$message, "\n")
    failed_imps <- c(failed_imps, i)
    list(success = FALSE, error = e$message)
  })
  
  # Store result
  results[[as.character(i)]] <- outcome_result
  
  # Force garbage collection
  gc(verbose = FALSE)
}

# Summary
cat("\n================================================================================\n")
cat("SUMMARY\n")
cat("================================================================================\n")
cat("Method:", method, "\n")
cat("Requested imputations:", paste(imps, collapse = ", "), "\n")
cat("Successfully fitted:", paste(successful_imps, collapse = ", "), 
    if(length(successful_imps) == 0) "(none)" else "", "\n")
cat("Failed:", paste(failed_imps, collapse = ", "), 
    if(length(failed_imps) == 0) "(none)" else "", "\n")

# Check what's now in the cache for this method
cat("\nChecking outcome cache for", method, ":\n")
outcome_files <- list.files(outcome_cache_dir, 
                           pattern = paste0("outcome_", method, "_imp\\d+\\.rds"),
                           full.names = FALSE)
cat("  Total", method, "outcome models in cache:", length(outcome_files), "\n")

if (length(outcome_files) > 0) {
  # Extract imputation numbers
  imp_numbers <- as.numeric(gsub(paste0("outcome_", method, "_imp(\\d+)\\.rds"), "\\1", outcome_files))
  imp_numbers <- sort(imp_numbers)
  
  # Find any still missing
  expected_imps <- 1:50
  still_missing <- setdiff(expected_imps, imp_numbers)
  
  if (length(still_missing) > 0) {
    cat("  Still missing imputations:", paste(still_missing, collapse = ", "), "\n")
  } else {
    cat("  All 50 imputations now have outcome models!\n")
  }
}

cat("\nDone!\n")