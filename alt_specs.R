# ============================================================================
# Alternative Propensity Score Specifications - Strategy Pattern Implementation
# ============================================================================
# This file provides a modular approach to running multiple PS methods
# on multiply imputed data with standardized interfaces and caching

# Load required packages with installation check
required_ps_packages <- c("MatchIt", "WeightIt", "twang", "cobalt", "survey", "mice")

for (pkg in required_ps_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    if (is.null(getOption("repos")) || getOption("repos") == "@CRAN@") {
      options(repos = c(CRAN = "https://cloud.r-project.org/"))
    }
    cat(paste0("Installing ", pkg, "...\n"))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ============================================================================
# Configuration
# ============================================================================

# Define analysis modes
get_analysis_config <- function(mode = "test") {
  if (mode == "test") {
    list(
      n_imputations = 2,
      n_trees_gbm = 1000,
      n_trees_twang = 10000,  # Match analysis.Rmd line 157
      cache_enabled = TRUE
    )
  } else if (mode == "final") {
    list(
      n_imputations = 50,
      n_trees_gbm = 3000,
      n_trees_twang = 10000,  # Match analysis.Rmd line 157
      cache_enabled = TRUE
    )
  } else {
    stop("Invalid mode. Use 'test' or 'final'")
  }
}

# ============================================================================
# Caching Utilities
# ============================================================================

# Get cache file path for a specific method and imputation
get_cache_path <- function(method_name, imputation_id, cache_dir = "ps_alt_cache") {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  file.path(cache_dir, paste0(method_name, "_imp", imputation_id, ".rds"))
}

# Check if cached result exists and is valid
check_cache <- function(method_name, imputation_id, cache_dir = "ps_alt_cache", force_rerun = FALSE) {
  if (force_rerun) return(FALSE)
  
  cache_file <- get_cache_path(method_name, imputation_id, cache_dir)
  file.exists(cache_file)
}

# Load cached result
load_cache <- function(method_name, imputation_id, cache_dir = "ps_alt_cache") {
  cache_file <- get_cache_path(method_name, imputation_id, cache_dir)
  if (file.exists(cache_file)) {
    readRDS(cache_file)
  } else {
    NULL
  }
}

# Save result to cache
save_cache <- function(result, method_name, imputation_id, cache_dir = "ps_alt_cache") {
  cache_file <- get_cache_path(method_name, imputation_id, cache_dir)
  saveRDS(result, cache_file)
}

# ============================================================================
# PS Method Strategies
# ============================================================================

# Base strategy function that all methods will use
ps_strategy_base <- function(data, formula, method_name, params, imputation_id, 
                            cache_dir = "ps_alt_cache", use_cache = TRUE) {
  
  # Check cache first
  if (use_cache && check_cache(method_name, imputation_id, cache_dir)) {
    cat(paste0("  Loading cached ", method_name, " for imputation ", imputation_id, "\n"))
    return(load_cache(method_name, imputation_id, cache_dir))
  }
  
  cat(paste0("  Running ", method_name, " for imputation ", imputation_id, "\n"))
  
  # Standardized result structure
  result <- list(
    method = method_name,
    imputation_id = imputation_id,
    ps_object = NULL,
    weights = NULL,
    matched_data = NULL,
    diagnostics = list(),
    error = NULL,
    success = FALSE
  )
  
  tryCatch({
    # Call the specific strategy implementation
    ps_result <- params$strategy_fn(data, formula, params)
    
    # Extract standardized components based on result type
    if (inherits(ps_result, "matchit")) {
      result$ps_object <- ps_result
      result$success <- TRUE
      
      # Get matched data or weights depending on method
      if (params$method == "subclass") {
        result$matched_data <- match.data(ps_result, subclass = "subclass")
        result$weights <- result$matched_data$weights
      } else {
        result$matched_data <- get_matches(ps_result, data = data)
        result$weights <- rep(1, nrow(result$matched_data))
      }
      
    } else if (inherits(ps_result, "weightit")) {
      result$ps_object <- ps_result
      result$weights <- ps_result$weights
      result$success <- TRUE
      
    } else if (inherits(ps_result, "ps")) {
      # twang result - use get.weights() like in analysis.Rmd line 346
      result$ps_object <- ps_result
      result$weights <- get.weights(ps_result, stop.method = "es.mean")
      result$success <- TRUE
      
    } else {
      stop("Unknown PS result type")
    }
    
    # Add diagnostics if successful
    if (result$success) {
      result$diagnostics <- extract_diagnostics(result, data)
    }
    
  }, error = function(e) {
    result$error <- e$message
    warning(paste0("Error in ", method_name, " (imp ", imputation_id, "): ", e$message))
  })
  
  # Save to cache if successful
  if (result$success && use_cache) {
    save_cache(result, method_name, imputation_id, cache_dir)
  }
  
  return(result)
}

# Extract standardized diagnostics
extract_diagnostics <- function(ps_result, original_data) {
  diag <- list()
  
  tryCatch({
    if (inherits(ps_result$ps_object, "matchit")) {
      # MatchIt diagnostics
      diag$balance_summary <- bal.tab(ps_result$ps_object, stats = c("m", "v"))
      diag$n_treated <- sum(ps_result$ps_object$treat == 1)
      diag$n_control <- sum(ps_result$ps_object$treat == 0)
      
    } else if (inherits(ps_result$ps_object, "weightit")) {
      # WeightIt diagnostics
      diag$balance_summary <- bal.tab(ps_result$ps_object, stats = c("m", "v"))
      diag$effective_sample_size <- sum(ps_result$weights)^2 / sum(ps_result$weights^2)
      
    } else if (inherits(ps_result$ps_object, "ps")) {
      # twang diagnostics
      diag$balance_summary <- bal.table(ps_result$ps_object)
      diag$n_trees_used <- ps_result$ps_object$desc$es.mean$n.trees  # es.mean specific
    }
  }, error = function(e) {
    diag$diagnostic_error <- e$message
  })
  
  return(diag)
}

# ============================================================================
# Individual PS Method Implementations
# ============================================================================

# MatchIt nearest neighbor with GLM distance
ps_nearest_glm <- function(data, formula, params) {
  matchit(formula = formula, 
         data = data,
         method = "nearest",
         distance = "glm",
         estimand = "ATT",
         replace = TRUE)
}

# MatchIt nearest neighbor with GAM distance
ps_nearest_gam <- function(data, formula, params) {
  matchit(formula = formula,
         data = data,
         method = "nearest", 
         distance = "gam",
         estimand = "ATT",
         replace = TRUE)
}

# MatchIt nearest neighbor with GBM distance
ps_nearest_gbm <- function(data, formula, params) {
  # Ensure response is numeric {0,1} for Bernoulli GBM
  data2 <- data.frame(data)
  outcome_var <- all.vars(formula)[1]
  if (outcome_var %in% names(data2)) {
    y <- data2[[outcome_var]]
    if (is.factor(y)) {
      y <- as.numeric(as.character(y))
    } else if (is.character(y)) {
      y <- as.numeric(y)
    } else if (is.logical(y)) {
      y <- as.numeric(y)
    }
    data2[[outcome_var]] <- y
    if (any(!is.na(data2[[outcome_var]]) & !data2[[outcome_var]] %in% c(0, 1))) {
      stop(paste0("Response variable ", outcome_var, " must be numeric 0/1 for GBM. Found values: ",
                  paste(unique(data2[[outcome_var]]), collapse = ", ")))
    }
  }

  matchit(formula = formula,
          data = data2,
          method = "nearest",
          distance = "gbm",
          estimand = "ATT",
          replace = TRUE,
          distance.options = list(
            n.trees = params$n_trees,
            interaction.depth = 3,
            shrinkage = 0.01
          ))
}

# MatchIt nearest neighbor with LASSO distance
ps_nearest_lasso <- function(data, formula, params) {
  matchit(formula = formula,
         data = data,
         method = "nearest",
         distance = "lasso",
         estimand = "ATT",
         replace = TRUE)
}

# MatchIt nearest neighbor with RPART distance
ps_nearest_rpart <- function(data, formula, params) {
  matchit(formula = formula,
         data = data,
         method = "nearest",
         distance = "rpart",
         estimand = "ATT",
         replace = TRUE)
}

# MatchIt nearest neighbor with Mahalanobis distance
ps_nearest_mahalanobis <- function(data, formula, params) {
  matchit(formula = formula,
         data = data,
         method = "nearest",
         distance = "mahalanobis",
         estimand = "ATT",
         replace = TRUE)
}

# MatchIt subclassification with GLM
ps_subclass_glm <- function(data, formula, params) {
  matchit(formula = formula,
         data = data,
         method = "subclass",
         distance = "glm",
         estimand = "ATT",
         subclass = 5)
}

# WeightIt CBPS
ps_cbps <- function(data, formula, params) {
  weightit(formula = formula,
          data = data,
          method = "cbps",
          estimand = "ATT",
          over = FALSE)
}

# WeightIt Entropy Balancing
ps_entropy <- function(data, formula, params) {
  weightit(formula = formula,
          data = data,
          method = "ebal",
          estimand = "ATT")
}

# WeightIt BART
ps_bart <- function(data, formula, params) {
  # Ensure response is numeric {0,1} for BART
  data2 <- data.frame(data)
  outcome_var <- all.vars(formula)[1]
  if (outcome_var %in% names(data2)) {
    y <- data2[[outcome_var]]
    if (is.factor(y)) {
      y <- as.numeric(as.character(y))
    } else if (is.character(y)) {
      y <- as.numeric(y)
    } else if (is.logical(y)) {
      y <- as.numeric(y)
    }
    data2[[outcome_var]] <- y
    if (any(!is.na(data2[[outcome_var]]) & !data2[[outcome_var]] %in% c(0, 1))) {
      stop(paste0("Response variable ", outcome_var, " must be numeric 0/1 for BART. Found values: ",
                  paste(unique(data2[[outcome_var]]), collapse = ", ")))
    }
  }
  
  weightit(formula = formula,
          data = data2,
          method = "bart",
          estimand = "ATT")
}

# Twang GBM (matching analysis.Rmd lines 156-159)
ps_twang_gbm <- function(data, formula, params) {
  # Ensure response is numeric {0,1} for Bernoulli GBM
  data2 <- data.frame(data)
  outcome_var <- all.vars(formula)[1]
  if (outcome_var %in% names(data2)) {
    y <- data2[[outcome_var]]
    if (is.factor(y)) {
      y <- as.numeric(as.character(y))
    } else if (is.character(y)) {
      y <- as.numeric(y)
    } else if (is.logical(y)) {
      y <- as.numeric(y)
    }
    data2[[outcome_var]] <- y
    if (any(!is.na(data2[[outcome_var]]) & !data2[[outcome_var]] %in% c(0, 1))) {
      stop(paste0("Response variable ", outcome_var, " must be numeric 0/1 for GBM. Found values: ",
                  paste(unique(data2[[outcome_var]]), collapse = ", ")))
    }
  }
  
  ps(formula = formula,
     data = data2,
     n.trees = params$n_trees,
     interaction.depth = 3,
     shrinkage = 0.01,
     estimand = "ATT",
     stop.method = c("es.mean", "ks.max"))
}

# ============================================================================
# Method Registry
# ============================================================================

# Get all available PS methods with their configurations
get_ps_methods <- function(config = NULL) {
  if (is.null(config)) {
    config <- get_analysis_config("test")
  }
  
  list(
    nearest_glm = list(
      name = "nearest_glm",
      strategy_fn = ps_nearest_glm,
      method = "nearest",
      package = "matchit"
    ),
    nearest_gam = list(
      name = "nearest_gam",
      strategy_fn = ps_nearest_gam,
      method = "nearest",
      package = "matchit"
    ),
    nearest_gbm = list(
      name = "nearest_gbm",
      strategy_fn = ps_nearest_gbm,
      method = "nearest",
      package = "matchit",
      n_trees = config$n_trees_gbm
    ),
    nearest_lasso = list(
      name = "nearest_lasso",
      strategy_fn = ps_nearest_lasso,
      method = "nearest",
      package = "matchit"
    ),
    nearest_rpart = list(
      name = "nearest_rpart",
      strategy_fn = ps_nearest_rpart,
      method = "nearest",
      package = "matchit"
    ),
    nearest_mahalanobis = list(
      name = "nearest_mahalanobis",
      strategy_fn = ps_nearest_mahalanobis,
      method = "nearest",
      package = "matchit"
    ),
    subclass_glm = list(
      name = "subclass_glm",
      strategy_fn = ps_subclass_glm,
      method = "subclass",
      package = "matchit"
    ),
    cbps = list(
      name = "cbps",
      strategy_fn = ps_cbps,
      method = "cbps",
      package = "weightit"
    ),
    entropy = list(
      name = "entropy",
      strategy_fn = ps_entropy,
      method = "ebal",
      package = "weightit"
    ),
    bart = list(
      name = "bart",
      strategy_fn = ps_bart,
      method = "bart",
      package = "weightit"
    ),
    twang_gbm = list(
      name = "twang_gbm",
      strategy_fn = ps_twang_gbm,
      method = "twang",
      package = "twang",
      n_trees = config$n_trees_twang
    )
  )
}

# ============================================================================
# Orchestrator Functions
# ============================================================================

# Run a single PS method across all imputations
run_ps_method <- function(method_name, imputed_datasets, formula, 
                         cache_dir = "ps_alt_cache", use_cache = TRUE,
                         config = NULL) {
  
  if (is.null(config)) {
    config <- get_analysis_config("test")
  }
  
  methods <- get_ps_methods(config)
  
  if (!method_name %in% names(methods)) {
    stop(paste0("Unknown method: ", method_name))
  }
  
  method_params <- methods[[method_name]]
  n_imp <- min(length(imputed_datasets), config$n_imputations)
  
  cat(paste0("\n", paste(rep("=", 50), collapse = ""), "\n"))
  cat(paste0("Running ", method_name, " across ", n_imp, " imputations\n"))
  cat(paste0(paste(rep("=", 50), collapse = ""), "\n"))
  
  results <- list()
  
  for (i in 1:n_imp) {
    results[[i]] <- ps_strategy_base(
      data = imputed_datasets[[i]],
      formula = formula,
      method_name = method_name,
      params = method_params,
      imputation_id = i,
      cache_dir = cache_dir,
      use_cache = use_cache
    )
  }
  
  # Summary statistics
  n_success <- sum(sapply(results, function(x) x$success))
  cat(paste0("\nCompleted ", method_name, ": ", n_success, "/", n_imp, " successful\n"))
  
  return(results)
}

# Run all PS methods (no loop - explicit calls for easy debugging)
run_all_ps_methods <- function(imputed_datasets, formula, 
                              cache_dir = "ps_alt_cache", use_cache = TRUE,
                              config = NULL, methods_to_run = NULL) {
  
  if (is.null(config)) {
    config <- get_analysis_config("test")
  }
  
  all_methods <- names(get_ps_methods(config))
  
  if (is.null(methods_to_run)) {
    methods_to_run <- all_methods
  }
  
  results <- list()
  
  # Run each method explicitly (no loop for easy debugging)
  if ("nearest_glm" %in% methods_to_run) {
    results$nearest_glm <- run_ps_method("nearest_glm", imputed_datasets, formula, 
                                        cache_dir, use_cache, config)
  }
  
  if ("nearest_gam" %in% methods_to_run) {
    results$nearest_gam <- run_ps_method("nearest_gam", imputed_datasets, formula,
                                        cache_dir, use_cache, config)
  }
  
  if ("nearest_gbm" %in% methods_to_run) {
    results$nearest_gbm <- run_ps_method("nearest_gbm", imputed_datasets, formula,
                                        cache_dir, use_cache, config)
  }
  
  if ("nearest_lasso" %in% methods_to_run) {
    results$nearest_lasso <- run_ps_method("nearest_lasso", imputed_datasets, formula,
                                          cache_dir, use_cache, config)
  }
  
  if ("nearest_rpart" %in% methods_to_run) {
    results$nearest_rpart <- run_ps_method("nearest_rpart", imputed_datasets, formula,
                                          cache_dir, use_cache, config)
  }
  
  if ("nearest_mahalanobis" %in% methods_to_run) {
    results$nearest_mahalanobis <- run_ps_method("nearest_mahalanobis", imputed_datasets, formula,
                                                cache_dir, use_cache, config)
  }
  
  if ("subclass_glm" %in% methods_to_run) {
    results$subclass_glm <- run_ps_method("subclass_glm", imputed_datasets, formula,
                                         cache_dir, use_cache, config)
  }
  
  if ("cbps" %in% methods_to_run) {
    results$cbps <- run_ps_method("cbps", imputed_datasets, formula,
                                 cache_dir, use_cache, config)
  }
  
  if ("entropy" %in% methods_to_run) {
    results$entropy <- run_ps_method("entropy", imputed_datasets, formula,
                                    cache_dir, use_cache, config)
  }
  
  if ("bart" %in% methods_to_run) {
    results$bart <- run_ps_method("bart", imputed_datasets, formula,
                                cache_dir, use_cache, config)
  }
  
  if ("twang_gbm" %in% methods_to_run) {
    # Check if we should load from reanalysis-4 cache
    twang_cache_dir <- file.path("reanalysis_data", "ps_cache")
    if (dir.exists(twang_cache_dir)) {
      cat("\nLoading twang_gbm results from reanalysis-4 cache...\n")
      results$twang_gbm <- load_twang_from_reanalysis4(imputed_datasets, config)
    } else {
      # Run fresh if cache doesn't exist
      results$twang_gbm <- run_ps_method("twang_gbm", imputed_datasets, formula,
                                        cache_dir, use_cache, config)
    }
  }
  
  return(results)
}

# Load twang results from reanalysis-4
load_twang_from_reanalysis4 <- function(imputed_datasets, config) {
  twang_cache_dir <- file.path("reanalysis_data", "ps_cache")
  n_imp <- min(length(imputed_datasets), config$n_imputations)
  
  results <- list()
  
  for (i in 1:n_imp) {
    # Check both possible file naming conventions
    twang_file1 <- file.path(twang_cache_dir, paste0("twang_imp_", i, ".rds"))
    twang_file2 <- file.path(twang_cache_dir, paste0("ps_imp_", i, ".rds"))  # Alternative naming
    
    twang_file <- if (file.exists(twang_file1)) twang_file1 else twang_file2
    
    if (file.exists(twang_file)) {
      cached <- readRDS(twang_file)
      
      # Convert to our standard format
      results[[i]] <- list(
        method = "twang_gbm",
        imputation_id = i,
        ps_object = cached$model,
        weights = cached$weights,
        matched_data = NULL,
        diagnostics = list(
          n_trees_used = cached$n_trees_used,
          balance_summary = cached$balance_table
        ),
        error = NULL,
        success = TRUE
      )
      
      cat(paste0("  Loaded twang_gbm for imputation ", i, " from reanalysis-4\n"))
    } else {
      results[[i]] <- list(
        method = "twang_gbm",
        imputation_id = i,
        ps_object = NULL,
        weights = NULL,
        matched_data = NULL,
        diagnostics = list(),
        error = "File not found in reanalysis-4 cache",
        success = FALSE
      )
      warning(paste0("twang_gbm results not found for imputation ", i))
    }
  }
  
  return(results)
}

# ============================================================================
# Outcome Analysis Functions
# ============================================================================

# Fit outcome model for a single PS result
fit_outcome_model <- function(ps_result, data, formula, method_name = NULL) {
  if (!ps_result$success) {
    return(list(success = FALSE, error = ps_result$error))
  }
  
  result <- list(
    method = ifelse(!is.null(method_name), method_name, ps_result$method),
    imputation_id = ps_result$imputation_id,
    success = FALSE
  )
  
  tryCatch({
    # Create survey design based on PS method type
    if (inherits(ps_result$ps_object, "matchit")) {
      if ("subclass" %in% names(ps_result$matched_data)) {
        # Subclassification
        design <- svydesign(ids = ~e_mrn_deidentified, 
                          data = ps_result$matched_data, 
                          weights = ~weights)
        design <- postStratify(design, ~subclass, 
                             xtabs(~subclass, data = ps_result$matched_data))
      } else {
        # Matching
        design <- svydesign(ids = ~e_mrn_deidentified, 
                          data = ps_result$matched_data, 
                          weights = ~1)
      }
      
    } else if (inherits(ps_result$ps_object, "weightit") || 
              inherits(ps_result$ps_object, "ps")) {
      # Weighting methods
      data$ps_weights <- ps_result$weights
      design <- svydesign(ids = ~e_mrn_deidentified, 
                        weights = ~ps_weights, 
                        data = data)
      
    } else {
      stop("Unknown PS result type")
    }
    
    # Fit the outcome model
    model <- svyglm(formula, design = design, family = quasibinomial())
    
    # Extract coefficient for lapse
    coef_summary <- coef(summary(model))
    coef_lapse <- coef_summary["ever_lapse_binary", ]
    
    result$estimate <- coef_lapse["Estimate"]
    result$se <- coef_lapse["Std. Error"]
    result$model <- model
    result$success <- TRUE
    
  }, error = function(e) {
    result$error <- e$message
    warning(paste0("Error fitting outcome model for ", result$method, 
                  " (imp ", result$imputation_id, "): ", e$message))
  })
  
  return(result)
}

# Run outcome analysis for all PS results
run_outcome_analysis <- function(ps_results, imputed_datasets, formula, config = NULL) {
  if (is.null(config)) {
    config <- get_analysis_config("test")
  }
  
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("Running outcome analysis\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  
  outcome_results <- list()
  
  for (method_name in names(ps_results)) {
    cat(paste0("Fitting outcome models for ", method_name, "...\n"))
    
    method_ps_results <- ps_results[[method_name]]
    method_outcomes <- list()
    
    n_imp <- min(length(method_ps_results), config$n_imputations)
    
    for (i in 1:n_imp) {
      if (!is.null(method_ps_results[[i]])) {
        method_outcomes[[i]] <- fit_outcome_model(
          ps_result = method_ps_results[[i]],
          data = imputed_datasets[[i]],
          formula = formula,
          method_name = method_name
        )
      }
    }
    
    outcome_results[[method_name]] <- method_outcomes
    
    # Summary
    n_success <- sum(sapply(method_outcomes, function(x) !is.null(x) && x$success))
    cat(paste0("  Completed: ", n_success, "/", n_imp, " successful\n"))
  }
  
  return(outcome_results)
}

# ============================================================================
# Pooling Functions (Rubin's Rules)
# ============================================================================

# Pool results across imputations using Rubin's rules
pool_results <- function(outcome_results) {
  # Filter out unsuccessful results
  valid_results <- outcome_results[sapply(outcome_results, function(x) 
    !is.null(x) && x$success)]
  
  if (length(valid_results) == 0) {
    return(list(success = FALSE, error = "No valid results to pool"))
  }
  
  # Extract estimates and SEs
  estimates <- sapply(valid_results, function(x) x$estimate)
  ses <- sapply(valid_results, function(x) x$se)
  
  # Remove any NA values
  valid_idx <- !is.na(estimates) & !is.na(ses)
  estimates <- estimates[valid_idx]
  ses <- ses[valid_idx]
  
  if (length(estimates) == 0) {
    return(list(success = FALSE, error = "No valid estimates to pool"))
  }
  
  m <- length(estimates)  # Number of imputations
  
  # Pooled estimate (average of estimates)
  pooled_estimate <- mean(estimates)
  
  # Within-imputation variance (average of squared SEs)
  within_var <- mean(ses^2)
  
  # Between-imputation variance
  between_var <- var(estimates)
  
  # Total variance using Rubin's rules
  total_var <- within_var + between_var + (between_var / m)
  pooled_se <- sqrt(total_var)
  
  # Degrees of freedom (Barnard-Rubin adjustment)
  lambda <- (between_var + between_var/m) / total_var
  df_old <- (m - 1) / lambda^2
  
  # Calculate confidence intervals and p-value
  t_stat <- pooled_estimate / pooled_se
  p_value <- 2 * pt(abs(t_stat), df = df_old, lower.tail = FALSE)
  ci_lower <- pooled_estimate - qt(0.975, df_old) * pooled_se
  ci_upper <- pooled_estimate + qt(0.975, df_old) * pooled_se
  
  # Convert to odds ratio scale
  or <- exp(pooled_estimate)
  or_ci_lower <- exp(ci_lower)
  or_ci_upper <- exp(ci_upper)
  
  return(list(
    success = TRUE,
    estimate = pooled_estimate,
    se = pooled_se,
    or = or,
    or_ci_lower = or_ci_lower,
    or_ci_upper = or_ci_upper,
    p_value = p_value,
    n_imputations = m,
    df = df_old,
    t_stat = t_stat
  ))
}

# Pool results for all methods
pool_all_methods <- function(outcome_results) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("Pooling results using Rubin's rules\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  
  pooled_results <- list()
  
  for (method_name in names(outcome_results)) {
    pooled <- pool_results(outcome_results[[method_name]])
    
    if (pooled$success) {
      pooled$method <- method_name
      pooled_results[[method_name]] <- pooled
      
      cat(sprintf("%-20s: OR = %.3f (%.3f, %.3f), p = %.4f\n",
                 method_name, pooled$or, pooled$or_ci_lower, 
                 pooled$or_ci_upper, pooled$p_value))
    } else {
      cat(sprintf("%-20s: Failed - %s\n", method_name, pooled$error))
    }
  }
  
  return(pooled_results)
}

# ============================================================================
# Results Formatting Functions
# ============================================================================

# Format pooled results for forest plot
format_for_forest_plot <- function(pooled_results) {
  # Create data frame for plotting
  forest_data <- do.call(rbind, lapply(names(pooled_results), function(method) {
    res <- pooled_results[[method]]
    if (res$success) {
      data.frame(
        method = method,
        or = res$or,
        lower = res$or_ci_lower,
        upper = res$or_ci_upper,
        p_value = res$p_value,
        n_imputations = res$n_imputations,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }))
  
  # Create readable labels
  forest_data$method_label <- forest_data$method
  forest_data$method_label <- gsub("_", " ", forest_data$method_label)
  forest_data$method_label <- gsub("nearest ", "NN ", forest_data$method_label)
  forest_data$method_label <- gsub("subclass ", "Subclass ", forest_data$method_label)
  forest_data$method_label <- gsub("twang gbm", "IPTW GBM (twang)", forest_data$method_label)
  forest_data$method_label <- gsub("cbps", "IPTW CBPS", forest_data$method_label)
  forest_data$method_label <- gsub("entropy", "IPTW Entropy Balancing", forest_data$method_label)
  forest_data$method_label <- gsub("bart", "IPTW BART", forest_data$method_label)
  forest_data$method_label <- tools::toTitleCase(forest_data$method_label)
  
  # Order by odds ratio
  forest_data <- forest_data[order(forest_data$or), ]
  
  return(forest_data)
}

# Create summary table
create_summary_table <- function(pooled_results) {
  summary_data <- do.call(rbind, lapply(names(pooled_results), function(method) {
    res <- pooled_results[[method]]
    if (res$success) {
      data.frame(
        Method = method,
        OR = sprintf("%.3f", res$or),
        CI_95 = sprintf("(%.3f, %.3f)", res$or_ci_lower, res$or_ci_upper),
        P_value = sprintf("%.4f", res$p_value),
        N_Imputations = res$n_imputations,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }))
  
  # Sort by OR
  summary_data <- summary_data[order(as.numeric(gsub("([0-9.]+)", "\\1", summary_data$OR))), ]
  
  return(summary_data)
}

# ============================================================================
# Main Orchestrator Function
# ============================================================================

# Run complete analysis pipeline with intermediate saves
# Processes one method at a time, saves after each, can resume from crashes
run_alternative_ps_analysis_sequential <- function(imputed_datasets, ps_formula, outcome_formula,
                                                 mode = "test", cache_dir = "ps_alt_cache",
                                                 pooled_cache_dir = NULL,
                                                 use_cache = TRUE, 
                                                 force_rerun = FALSE,
                                                 all_methods = NULL) {
  
  config <- get_analysis_config(mode)
  n_imp <- min(length(imputed_datasets), config$n_imputations)
  
  # Set up pooled results cache directory
  if (is.null(pooled_cache_dir)) {
    pooled_cache_dir <- file.path(cache_dir, "pooled_results")
  }
  if (!dir.exists(pooled_cache_dir)) {
    dir.create(pooled_cache_dir, recursive = TRUE)
  }
  
  # Set up summary file
  summary_file <- file.path(pooled_cache_dir, "pooled_summary.csv")
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("ALTERNATIVE PS SPECIFICATIONS ANALYSIS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Mode:", mode, "\n")
  cat("Max imputations:", config$n_imputations, "\n")
  cat("Cache:", ifelse(use_cache, "ENABLED", "DISABLED"), "\n")
  cat("PS cache directory:", cache_dir, "\n")
  cat("Pooled results directory:", pooled_cache_dir, "\n")
  cat("Force rerun:", ifelse(force_rerun, "YES", "NO"), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Get all available methods if not specified
  if (is.null(all_methods)) {
    all_methods <- names(get_ps_methods(config))
  }
  
  # Check which methods have cached PS results
  cat("\nChecking cached PS results:\n")
  methods_with_cache <- c()
  for (method in all_methods) {
    # Check for at least one cached file
    if (method == "twang_gbm") {
      cache_file <- file.path("reanalysis_data", "ps_cache", "twang_imp_1.rds")
    } else {
      cache_file <- file.path(cache_dir, paste0(method, "_imp_1.rds"))
    }
    if (file.exists(cache_file)) {
      methods_with_cache <- c(methods_with_cache, method)
      cat(paste0("  ✓ ", method, " - cached PS results found\n"))
    } else {
      cat(paste0("  ✗ ", method, " - no cached PS results\n"))
    }
  }
  
  # Initialize results storage
  pooled_results <- list()
  summary_data <- data.frame()
  
  # Load any existing pooled results if not forcing rerun
  if (!force_rerun && file.exists(summary_file)) {
    cat("\nLoading existing pooled results:\n")
    existing_summary <- read.csv(summary_file, stringsAsFactors = FALSE)
    for (i in 1:nrow(existing_summary)) {
      method <- existing_summary$method[i]
      pooled_file <- file.path(pooled_cache_dir, paste0("pooled_", method, ".rds"))
      if (file.exists(pooled_file)) {
        pooled_results[[method]] <- readRDS(pooled_file)
        cat(paste0("  Loaded pooled results for ", method, "\n"))
      }
    }
    summary_data <- existing_summary
  }
  
  # Process each method one at a time
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("Processing methods sequentially\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  
  for (method_name in methods_with_cache) {
    # Skip if already processed (unless forcing rerun)
    if (!force_rerun && method_name %in% names(pooled_results)) {
      cat(paste0("\n--- Skipping ", method_name, " (already processed) ---\n"))
      next
    }
    
    cat(paste0("\n--- Processing ", method_name, " ---\n"))
    
    tryCatch({
      # Step 1: Load PS results for this method only
      ps_method_results <- list()
      
      if (method_name == "twang_gbm") {
        # Special handling for twang_gbm
        ps_method_results <- load_twang_from_reanalysis4(imputed_datasets, config)
        cat("  Loaded from reanalysis-4 cache\n")
      } else {
        # Load from regular cache
        for (i in 1:n_imp) {
          cached <- load_cache(method_name, i, cache_dir)
          if (!is.null(cached)) {
            ps_method_results[[i]] <- cached
          }
        }
        if (length(ps_method_results) > 0) {
          cat(paste0("  Loaded ", length(ps_method_results), " cached PS results\n"))
        }
      }
      
      if (length(ps_method_results) == 0) {
        cat(paste0("  WARNING: No PS results found for ", method_name, "\n"))
        next
      }
      
      # Step 2: Run outcome analysis for this method
      cat("  Fitting outcome models...\n")
      method_outcome_results <- list()
      n_fitted <- 0
      
      for (i in seq_along(ps_method_results)) {
        if (!is.null(ps_method_results[[i]]) && ps_method_results[[i]]$success) {
          outcome_result <- fit_outcome_model(
            ps_result = ps_method_results[[i]],
            data = imputed_datasets[[i]],
            formula = outcome_formula,
            method_name = method_name
          )
          if (outcome_result$success) {
            method_outcome_results[[i]] <- outcome_result
            n_fitted <- n_fitted + 1
            if (n_fitted %% 10 == 0) {
              cat(paste0("    Fitted ", n_fitted, " models...\n"))
            }
          }
        }
      }
      
      # Step 3: Pool results for this method
      if (length(method_outcome_results) > 0) {
        cat("  Pooling results...\n")
        method_pooled <- pool_single_method(method_outcome_results, method_name)
        if (!is.null(method_pooled) && method_pooled$success) {
          pooled_results[[method_name]] <- method_pooled
          
          # Save pooled result immediately
          pooled_file <- file.path(pooled_cache_dir, paste0("pooled_", method_name, ".rds"))
          saveRDS(method_pooled, pooled_file)
          
          # Update summary data
          new_row <- data.frame(
            method = method_name,
            or = method_pooled$or,
            ci_lower = method_pooled$ci_lower,
            ci_upper = method_pooled$ci_upper,
            p_value = method_pooled$p_value,
            n_imputations = length(method_outcome_results),
            timestamp = Sys.time(),
            stringsAsFactors = FALSE
          )
          
          # Remove old entry if exists
          summary_data <- summary_data[summary_data$method != method_name, ]
          summary_data <- rbind(summary_data, new_row)
          
          # Save summary immediately
          write.csv(summary_data, summary_file, row.names = FALSE)
          
          # Log the results
          cat(paste0("  SUCCESS: Pooled ", length(method_outcome_results), " imputations\n"))
          cat(paste0("  Pooled estimate for ", method_name, ":\n"))
          cat(paste0("    OR = ", sprintf("%.3f", method_pooled$or), 
                    " (95% CI: ", sprintf("%.3f", method_pooled$ci_lower),
                    " - ", sprintf("%.3f", method_pooled$ci_upper), ")\n"))
          cat(paste0("    p-value = ", sprintf("%.4f", method_pooled$p_value), "\n"))
          cat(paste0("  Saved to: ", pooled_file, "\n"))
        }
      }
      
      # Clear method-specific data to free memory
      rm(ps_method_results, method_outcome_results)
      gc()  # Force garbage collection
      
    }, error = function(e) {
      cat(paste0("  ERROR in ", method_name, ": ", e$message, "\n"))
      cat("  Continuing with next method...\n")
    })
  }
  
  # Step 4: Format final results
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("Formatting final results\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  if (length(pooled_results) > 0) {
    forest_data <- format_for_forest_plot(pooled_results)
    summary_table <- create_summary_table(pooled_results)
    
    # Save final outputs
    saveRDS(forest_data, file.path(pooled_cache_dir, "forest_data.rds"))
    saveRDS(summary_table, file.path(pooled_cache_dir, "summary_table.rds"))
    
    cat("\nFinal summary of pooled results:\n")
    cat(paste(rep("-", 50), collapse = ""), "\n")
    for (method in names(pooled_results)) {
      res <- pooled_results[[method]]
      cat(sprintf("%-20s: OR = %.3f (%.3f - %.3f), p = %.4f\n",
                  method, res$or, res$ci_lower, res$ci_upper, res$p_value))
    }
    cat(paste(rep("-", 50), collapse = ""), "\n")
  } else {
    forest_data <- NULL
    summary_table <- NULL
    cat("No pooled results available\n")
  }
  
  # Return results
  return(list(
    pooled_results = pooled_results,
    forest_data = forest_data,
    summary_table = summary_table,
    config = config,
    summary_file = summary_file,
    pooled_cache_dir = pooled_cache_dir
  ))
}

# Wrapper for backwards compatibility
run_alternative_ps_analysis <- function(imputed_datasets, ps_formula, outcome_formula,
                                       mode = "test", cache_dir = "ps_alt_cache",
                                       use_cache = TRUE, methods_to_run = NULL,
                                       all_methods = NULL, force_rerun = FALSE) {
  
  # Call the sequential version
  run_alternative_ps_analysis_sequential(
    imputed_datasets = imputed_datasets,
    ps_formula = ps_formula,
    outcome_formula = outcome_formula,
    mode = mode,
    cache_dir = cache_dir,
    use_cache = use_cache,
    force_rerun = force_rerun,
    all_methods = all_methods
  )
}

# ============================================================================
# Diagnostic Functions
# ============================================================================

# Extract balance statistics for a method across imputations
get_balance_summary <- function(ps_results, method_name) {
  method_results <- ps_results[[method_name]]
  
  balance_stats <- lapply(seq_along(method_results), function(i) {
    if (!is.null(method_results[[i]]) && method_results[[i]]$success) {
      diag <- method_results[[i]]$diagnostics
      if (!is.null(diag$balance_summary)) {
        list(
          imputation = i,
          balance = diag$balance_summary
        )
      }
    }
  })
  
  # Remove NULLs
  balance_stats <- balance_stats[!sapply(balance_stats, is.null)]
  
  return(balance_stats)
}

# Print method comparison
print_method_comparison <- function(pooled_results, reference_method = "twang_gbm") {
  if (!reference_method %in% names(pooled_results)) {
    cat("Reference method", reference_method, "not found in results\n")
    return(invisible(NULL))
  }
  
  ref_or <- pooled_results[[reference_method]]$or
  
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("Method Comparison (Reference:", reference_method, ")\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")
  
  for (method in names(pooled_results)) {
    res <- pooled_results[[method]]
    if (res$success) {
      rel_diff <- 100 * (res$or / ref_or - 1)
      cat(sprintf("%-20s: OR = %.3f (%+.1f%% vs reference)\n",
                 method, res$or, rel_diff))
    }
  }
}