# Script to run specific missing propensity score models
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
cat("RUNNING MISSING PROPENSITY SCORE MODELS\n")
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

# Set cache directory
cache_dir <- file.path(reanalysis_data_dir, "ps_cache")

# Ensure cache directory exists
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

cat("\nPS Formula:\n")
print(ps_formula)
cat("\n")

# Get the PS method configuration
config <- get_analysis_config("final")
ps_methods <- get_ps_methods(config)

# Check if method exists
if (!method %in% names(ps_methods)) {
  stop("Method '", method, "' not found in available PS methods.\n",
       "Available methods: ", paste(names(ps_methods), collapse = ", "))
}

# Get the method strategy
method_strategy <- ps_methods[[method]]
cat("Method type:", class(method_strategy)[1], "\n\n")

# Process each specified imputation
results <- list()
successful_imps <- c()
failed_imps <- c()

for (i in imps) {
  cat(paste0("\n", paste(rep("-", 60), collapse = ""), "\n"))
  cat(paste0("Processing ", method, " - Imputation ", i, "\n"))
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  # Check if PS model already exists
  ps_cache_file <- file.path(cache_dir, paste0(method, "_imp", i, ".rds"))
  
  if (file.exists(ps_cache_file)) {
    cat("  WARNING: PS model already exists at:\n")
    cat("  ", ps_cache_file, "\n")
    cat("  Overwriting...\n")
  }
  
  # Check if imputation exists
  if (i > length(imputed_datasets)) {
    cat("  ERROR: Imputation", i, "not found in imputed_datasets\n")
    cat("  Available imputations: 1 to", length(imputed_datasets), "\n")
    failed_imps <- c(failed_imps, i)
    next
  }
  
  # Get the imputed dataset
  imp_data <- imputed_datasets[[i]]
  cat("  Dataset dimensions:", nrow(imp_data), "rows x", ncol(imp_data), "cols\n")
  
  # Run the PS method
  cat("  Fitting", method, "propensity score model...\n")
  
  ps_result <- tryCatch({
    # Execute the PS method using the strategy pattern
    result <- method_strategy$execute(
      data = imp_data,
      formula = ps_formula,
      estimand = "ATT"
    )
    
    # Display key results
    if (result$success) {
      cat("  SUCCESS: PS model fitted\n")
      
      # Show some diagnostics based on method type
      if (!is.null(result$weights)) {
        cat("  Weights summary:\n")
        cat("    Min:", round(min(result$weights), 4), "\n")
        cat("    Max:", round(max(result$weights), 4), "\n")
        cat("    Mean:", round(mean(result$weights), 4), "\n")
        cat("    SD:", round(sd(result$weights), 4), "\n")
        
        # Check for extreme weights
        extreme_weights <- sum(result$weights > 10 | result$weights < 0.1)
        if (extreme_weights > 0) {
          cat("    WARNING: ", extreme_weights, " extreme weights (>10 or <0.1)\n")
        }
      }
      
      if (!is.null(result$ps)) {
        cat("  Propensity scores summary:\n")
        cat("    Min:", round(min(result$ps), 4), "\n")
        cat("    Max:", round(max(result$ps), 4), "\n")
        cat("    Mean:", round(mean(result$ps), 4), "\n")
      }
      
      if (!is.null(result$n_treated)) {
        cat("  Sample sizes:\n")
        cat("    Treated:", result$n_treated, "\n")
        cat("    Control:", result$n_control, "\n")
      }
      
      # Save the result
      cat("  Saving to cache...\n")
      saveRDS(result, ps_cache_file)
      cat("  Saved to:", ps_cache_file, "\n")
      
      successful_imps <- c(successful_imps, i)
    } else {
      cat("  FAILED:", result$error, "\n")
      failed_imps <- c(failed_imps, i)
    }
    
    result
  }, error = function(e) {
    cat("  ERROR during PS fitting:\n")
    cat("  ", e$message, "\n")
    
    # Try to provide more debugging info
    if (grepl("undefined columns", e$message)) {
      cat("  This might be a formula/variable issue.\n")
      cat("  Check that all variables in the formula exist in the dataset.\n")
    }
    
    failed_imps <- c(failed_imps, i)
    list(success = FALSE, error = e$message)
  })
  
  # Store result
  results[[as.character(i)]] <- ps_result
  
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
cat("\nChecking PS cache for", method, ":\n")
ps_files <- list.files(cache_dir, 
                      pattern = paste0(method, "_imp\\d+\\.rds"),
                      full.names = FALSE)
cat("  Total", method, "PS models in cache:", length(ps_files), "\n")

if (length(ps_files) > 0) {
  # Extract imputation numbers
  imp_numbers <- as.numeric(gsub(paste0(method, "_imp(\\d+)\\.rds"), "\\1", ps_files))
  imp_numbers <- sort(imp_numbers)
  
  # Find any still missing
  expected_imps <- 1:50
  still_missing <- setdiff(expected_imps, imp_numbers)
  
  if (length(still_missing) > 0) {
    cat("  Still missing imputations:", paste(still_missing, collapse = ", "), "\n")
  } else {
    cat("  All 50 imputations now have PS models!\n")
  }
  
  # Show which ones we have
  if (length(imp_numbers) <= 10) {
    cat("  Available imputations:", paste(imp_numbers, collapse = ", "), "\n")
  } else {
    cat("  Available imputations: ", paste(imp_numbers[1:5], collapse = ", "), 
        "...", paste(imp_numbers[(length(imp_numbers)-4):length(imp_numbers)], collapse = ", "), "\n")
  }
}

cat("\n================================================================================\n")
cat("NEXT STEPS\n")
cat("================================================================================\n")
cat("If PS models were successfully created, you can now:\n")
cat("1. Run reanalysis-9-outcome_models.qmd to fit outcome models\n")
cat("2. Or use run_missing_outcomes.R to fit just these specific outcome models\n")
cat("\nTo run outcome models for just these imputations:\n")
cat("  method <- '", method, "'\n")
cat("  imps <- c(", paste(successful_imps, collapse = ", "), ")\n")
cat("  source('run_missing_outcomes.R')\n")